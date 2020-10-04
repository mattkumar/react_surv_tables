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

#Load the bmt data set
data(bmt)

#Data mgmt - general - based on documentation - Use ??bmt to learn about the data set
bmt_1m <- bmt %>%
            mutate(ID          = 1:nrow(bmt),
                   Group       = case_when(group == 1 ~ "ALL",
                                           group == 2 ~ "AML (Low Risk)",
                                           group == 3 ~ "AML (High Risk)"),
                   Time        = t1,
                   Censor      = d1,
                   Age         = z1,
                   Sex         = ifelse(z3 == 1, "Male","Female"),
                   Relapse_Ind = ifelse(d2 == 1, "Relapse", "x"),
                   Relapse_Time = t2,
                   GVHD_Ind = ifelse(da == 1, "GVHD", "x"),
                   GVHD_Time = ta,
                   GVHDC_Ind = ifelse(dc == 1, "CGVHD", "x"),
                   GVHDC_Time = tc,
                   Plate_Ind = ifelse(dp == 1, "Plate", "x"),
                   Plate_Time = tp,
                   Transplant_Time = z7) %>%
            select(ID, Group, Time, Censor, Sex, Age, Transplant_Time,
                   starts_with("Relapse"), 
                   starts_with("GVHD"), 
                   starts_with("Plate"))

#Data mgmt for the swimmer plot
#Provision a place holder for the swimmer plot
#For the secondary events (relapse, gvhd, chronic gvhd, platelet recovery), set the time equal to NA if the event never happened
#For the primary event (death, e.g. Time/Censor), create a color and name col that's used in the swimmer plot
bmt_1m <-    bmt_1m %>%
              mutate(Relapse_Time   = ifelse(Relapse_Ind == "Relapse", Relapse_Time, NA),
                     GVHD_Time      = ifelse(GVHD_Ind == "GVHD", GVHD_Time, NA),
                     GVHDC_Time     = ifelse(GVHDC_Ind == "CGVHD", GVHDC_Time, NA),
                     Plate_Time     = ifelse(Plate_Ind == "Plate", Plate_Time, NA),
                     Censor_col     = ifelse(Censor == 1, col2hex("red"), col2hex("orange")),
                     Name_col       = ifelse(Censor == 1, "Death on Day", "Alive on Day"),
                     Swimmer        = NA) %>%
              select(-c(Relapse_Ind, GVHD_Ind, GVHDC_Ind, Plate_Ind))


#For the main survival analysis, (time to death, e.g. Time/Censor), perform survival calculations
sfit <- survfit(Surv(Time, Censor) ~ Group, data = bmt_1m)
time_vector <- seq(0,2500,500)

#For each ID, construct a series of indicators that record whether they are:
# at risk of the event for a given time
# had an event at the given time
#This can be potentially "expensive", so I compute it here in advance and save the resulting data
for(i in seq_along(time_vector)) {
  if(i==1) {
    #By Definition everyone is at risk at the beginning and no events
    bmt_1m[["risk_t0"]] = 1
    bmt_1m[["event_t0"]] = 0
  } else {
    bmt_1m[[paste0("risk_t", time_vector[i])]]  <- ifelse(bmt_1m$Time >= time_vector[i], 1, 0)
    bmt_1m[[paste0("event_t", time_vector[i])]] <- ifelse(bmt_1m$Censor == 1 & (bmt_1m$Time <= time_vector[i] & bmt_1m$Time > time_vector[i-1]), 1, 0)
  }
}

#Create the numbers at risk and event summary tables. These ultimately get displayed in the app
nar_summary_table <-  bmt_1m %>%
                        group_by(Group) %>%
                        summarise(across(starts_with("risk_t"), sum)) %>%
                        ungroup()

eve_summary_table <-  bmt_1m %>%
                        group_by(Group) %>%
                        summarise(across(starts_with("event_t"), sum)) %>%
                        ungroup()

#Save data needed for the actual shiny app
rm(list=setdiff(ls(), c("sfit","bmt_1m","eve_summary_table","nar_summary_table")))
save.image(here::here('data', 'pre_computed_data.Rdata'))
   
