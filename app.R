###Author : Matt
###Date   : 2020-09-26
###Purpose: The shiny app displaying the interactive survival tables

#load libs
library(tidyverse)
library(shiny)
library(survival)
library(survminer)
library(shinythemes)
library(reactable)
library(DT)
library(highcharter)

#load in pre-computed data
#the script that produced this data is in /code/01_data_prep.R
load(here::here('data', 'pre_computed_data.Rdata'))

#define ui
ui  <- fluidPage( 
        theme = shinytheme("cosmo"),
        titlePanel("Survival Tables"),
        fluidRow(
          column(2,
                 wellPanel(
                   p("A survival analysis of time to staphylococcus infection (days) by treatment group (routine bathing vs body cleansing) among burn patients is used to demonstrate interactive survival tables."),
                   br(),
                   selectInput('option','Select Analysis Method:', choices=c('Kaplan Meier'=1,'Cumulative Events'=2)),
                   br(),
                   p("If Kaplan Meier is chosen as the method, the Number at Risk table is displayed. If Cumulative Events is chosen, the Number of Events table is displayed."),
                   br(),
                   p("The summary table below the main plot is interactable; click on a cell to view the patients who comprise it in the adjacent table."),
                   br(),
                   p("The adjacent table also contains an interactive, inline", strong("swimmer plot")," for each participant. Hover over the points on the plot to learn about the different event types!"),
                   br()
                 )       
          ),
          column(5,
                 h3(textOutput("plot_title")),
                 br(),
                 plotOutput('km', height="470", width="675"),
                 h5(textOutput("header")),
                 DT::dataTableOutput('summary_tab', width="700")
          ),
          column(5,
                 h3("Drill Down Table"),
                 br(),
                 reactableOutput('drill_tab', width="775")
          )
        )
     ) #end fluidPage


#define server
server <- function(input, output) {
  
  #Define Reactive Values
  values <- reactiveValues()
  
  #Track the following: table data source, table title, plot title and plot options 
  #These are elements that display/change on the UI based on user selection of analysis method. 
  observeEvent(input$option,{
    if (input$option == 1) {
      values$data        <- nar_summary_table
      values$plot_title  <- "Kaplan Meier Plot"
      values$plot_type   <- NULL
      values$table_title <- "Number at Risk"
    } else if (input$option == 2) {
      values$data        <- eve_summary_table  
      values$plot_title  <- "Cumulative Events Plot"
      values$plot_type   <- "event"
      values$table_title <- "Number of Events"
    }
  })
  
  #Plot Title
  output$plot_title <- renderText(values$plot_title)
  
  #Plot - courtesy of survminer!
  output$km <- renderPlot(ggsurvplot(sfit, conf.int = TRUE, palette=c("#5F4B8BFF" ,  "#E69A8DFF"), fun = values$plot_type)) 

  #Summary Table Title
  output$header <- renderText(values$table_title)
  
  #Summary Table
  output$summary_tab <- renderDataTable(
    datatable(values$data,
              #Make individual cells selectable, one at a time only and initialize the selected cells on load
              selection = list(mode = 'single', target = 'cell', selected= matrix(c(1,3),ncol=2)),
              rownames  = FALSE,
              #Remove col names to give the illusion the table is part of the figure :)
              colnames  = rep("", ncol(values$data)),
              options   = list(dom = 't',
                               columnDefs = list(
                                 list(className = 'dt-center', targets = '_all'),
                                 #hide the first column, which is treatment (its color coded below, so it's redundant, but still needed for drilling down)
                                 list(visible=FALSE, targets=c(0))))) %>%
      
      #This chunk takes care of the color coding
      formatStyle(
        'Treatment',
        target = 'row',
        color = styleEqual(c("Body Cleansing", "Routine Bath"), c("#5F4B8BFF" ,  "#E69A8DFF")))
  )
  
  #Subset the original patient level data (e.g. burn_1m) to create the drill down data
  #This is reactive, and based on user cell selection in the summary table (output$summary_tab)
  drill_data <- reactive({
    
    #Require selection by user before doing anything
    req(input$summary_tab_cells_selected)
    
    #Retrieve the selected cell coordinates - these help determine how to drill down
    row_coord <- as.integer(input$summary_tab_cells_selected[1])
    col_coord <- as.integer(input$summary_tab_cells_selected[2])
    
    #Note: it's helpful to print these to the console to get an idea of the coordinate mapping and for debugging!
    #print(row_coord)
    #print(col_coord)
    
    #Remove variables not used, based on what was selected. By default, all event_ and risk_ are in the burn_1m dataset
    #Keeping the ones we don't use in interfere with the position coordinates in the next chunk
    #e.g. removing the unused ones lets us use the same selection logic on line 137
    if(input$option==1) {
      drill_filtered <- burn_1m %>%
        select(-starts_with("event_"))
    } else {
      drill_filtered <- burn_1m %>%
        select(-starts_with("risk_"))
    }
    
    #Using the coordinates, subset the original patient level data
    drill_filtered <- drill_filtered %>%
      
      #Here we specify what variables we want to see in the drill down
      #The last variable is always the indicator variable (e.g. event or risk), and is based on the col_coord of the user selected cell
      select(c(1:13, col_coord+13)) %>%
      
      #Filter the last variable (i.e. the indicator) to be equal to 1
      filter(eval(parse(text=colnames(.)[ncol(.)])) == 1) 
    
    #Final filtering on rows
    if(row_coord == 1) {
      drill_filtered <- drill_filtered %>% 
        filter(Treatment == "Body Cleansing") %>%
        select(-c(Treatment, starts_with("event"), starts_with("risk")))
    } else {
      drill_filtered <- drill_filtered %>% 
        filter(Treatment == "Routine Bath")%>%
        select(-c(Treatment, starts_with("event"), starts_with("risk")))
      
    }
  })
  
  #Show the drilled down data
  output$drill_tab <- renderReactable(
    reactable(
      #Select only a few variables for display in the drill down
      drill_data() %>% select(ID, Sex, Race, Type, Swimmer), 
      
      #Basic table features - self explanatory
      defaultPageSize = 6,
      striped = TRUE,
      highlight = TRUE,
      searchable = TRUE,
     
      #Adjusting column widths
      columns = list(
        ID     = colDef(width=50),
        Sex    = colDef(width=65),
        Race   = colDef(width=65), 
        Type   = colDef(width=75),
        
        #Swimmer Plot - custom embedded HTML widget using highcharter
        #The swimmer plot uses drill_data() to compute the inline chart
        Swimmer = colDef(name = 'Swimmer Plot',
                         cell = function(value,index) {
                           
                           #Swimmer plot logic starts here
                           drill_data()[index,] %>%
                             hchart("bar",  hcaes(x = ID , y = Time), name = "Total Study Days") %>%
                             
                             #Primary Event Censoring
                             hc_add_series(drill_data()[index,], 
                                           "point",  
                                           marker = list(symbol = "triangle"), 
                                           hcaes(x=ID, y=Time, color = Censor_col, group = Name_col)) %>%
                             
                             #Secondary Event - Excision and its options
                             hc_add_series(drill_data()[index,], 
                                           "point",
                                           #name is the value of the tool tip for this series
                                           name = "Excision on Day",  
                                           marker = list(symbol = "circle"),
                                           hcaes(x=ID, y=Excise_Time),   
                                           color = "green") %>%
                             
                             #Secondary Event - Prophlaxis and its options
                             hc_add_series(drill_data()[index,], 
                                           "point",
                                           #name will value of the tool tip for this series
                                           name = "Prophylaxis on Day", 
                                           marker = list(symbol = "square"),     
                                           hcaes(x=ID, y=Prophylaxis_Time), 
                                           color = "purple") %>%
                             
                             #Axis
                             #Keep yAxis labels in for now - you could remove them since the plot is interactable but it helps in comparisons across patients
                             hc_yAxis(title = list(text = " "), min=0, max=100, labels = list(enabled=TRUE)) %>%
                             hc_xAxis(title = list(text = " "), labels = list(enabled=FALSE)) %>%
                             
                             #Misc plot options
                             hc_legend(enabled = FALSE) %>%
                             hc_size(width = 500, height = 70) %>%
                             
                             #Custom tool tip formatting - basic JS string
                             hc_tooltip(formatter = JS("function(){return (this.series.name + `:  ` + this.y)}"))
                             
                           
                         } #end cell function
        ) #end Swimmer
      ) #end columns list
    ) #end reactable
  ) #end renderReactable
} #end server

#Run the app
shinyApp(ui = ui, server = server)
