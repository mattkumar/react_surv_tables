###Author : Matt
###Date   : 2020-09-26
###Purpose: This script will pre-compute the data sets necessary for the shiny app. 

#Load libs
library(tidyverse)
library(survminer)
library(survival)
library(KMsurv)    #survival data set lives here
library(highcharter)
library(reactable)
library(gplots)    #allows you to convert colors by name to hex

#Load the burn data set
data(burn)

#Data mgmt - general - based on documentation - Use ??burn to learn about the data set
burn_1m <-  burn %>%
  mutate(ID          = Obs,
         Treatment   = if_else(Z1 == 1, "Body Cleansing", "Routine Bath"),
         Sex         = if_else(Z2 == 1, "Female", "Male"),
         Race        = if_else(Z3 == 1, "White", "Other"),
         Type        = case_when(Z11 == 1 ~ "Chem",
                                 Z11 == 2 ~ "Scald",
                                 Z11 == 3 ~ "Electric",
                                 Z11 == 4 ~ "Flame"),
         Head        = if_else(Z5 == 1, "Yes", "No"),
         TSA         = Z4/100,
         Censor      = D3,
         Time        = T3,
         Excise      = D1,
         Excise_Time = T1,
         Prophylaxis = D2,
         Prophylaxis_Time = T2)  %>%
  select(-c(Obs, T3, D3, starts_with("Z")))


#Data mgmt for the swimmer plot
#Provision a place holder for the swimmer plot
#For the secondary events (excision, prophylaxis), set the time equal to NA if the event never happened
#For the primary event (infection, e.g. Time/Censor), create a color and name col that's used in the swimmer plot
burn_1m <- burn_1m %>%
  mutate(Excise_Time      = ifelse(Excise == 1, Excise_Time, NA),
         Prophylaxis_Time = ifelse(Prophylaxis == 1, Prophylaxis_Time, NA),
         Censor_col = ifelse(Censor == 1, col2hex("red"), col2hex("orange")),
         Name_col   = ifelse(Censor == 1, "Infection on Day", "No Infection on Day"),
         Swimmer = NA
  ) %>%
  select(-c(Excise, Prophylaxis, T1, T2, D1, D2))


#For the main survival analysis, (time to infection, e.g. Time/Censor), perform survival calculations
sfit <- survfit(Surv(Time, Censor) ~ Treatment, data = burn_1m)
time_vector <- seq(0,100,25)

#For each ID, construct a series of indicators that record whether they are:
# at risk of the event for a given time
# had an event at the given time
#This can be potentially "expensive", so I compute it here in advance and save the resulting data
for(i in seq_along(time_vector)) {
  if(i==1) {
    #By Definition everyone is at risk at the beginning and no events
    burn_1m[["risk_t0"]] = 1
    burn_1m[["event_t0"]] = 0
  } else {
    burn_1m[[paste0("risk_t", time_vector[i])]]  <- ifelse(burn_1m$Time >= time_vector[i], 1, 0)
    burn_1m[[paste0("event_t", time_vector[i])]] <- ifelse(burn_1m$Censor == 1 & (burn_1m$Time <= time_vector[i] & burn_1m$Time > time_vector[i-1]), 1, 0)
  }
}

#Create the numbers at risk and event summary tables. These ultimately get displayed in the app
nar_summary_table <-  burn_1m %>%
  group_by(Treatment) %>%
  summarise(across(starts_with("risk_t"), sum)) %>%
  ungroup()

eve_summary_table <-  burn_1m %>%
  group_by(Treatment) %>%
  summarise(across(starts_with("event_t"), sum)) %>%
  ungroup()

#Save data needed for the actual shiny app
rm(list=setdiff(ls(), c("sfit","burn_1m","eve_summary_table","nar_summary_table")))
save.image(here::here('data', 'pre_computed_data.Rdata'))
   