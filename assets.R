#load in pre-computed data
#the script that produced this data is in /code/01_data_prep.R
load(here::here('data', 'pre_computed_data.Rdata'))

#palette for styling Surface Area percent
orange_pal <- function(x) rgb(colorRamp(c("#ffe4cd", "#ffb54e"))(x), maxColorValue = 255)
