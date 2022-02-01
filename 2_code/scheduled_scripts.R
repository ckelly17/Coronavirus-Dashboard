library(taskscheduleR)
library(lubridate)

## HHS data
taskscheduler_delete("Pull federal COVID data")
taskscheduler_create(taskname = "Pull federal COVID data", 
                     rscript = "C:/Users/ckelly/Documents/Covid-Personal - Copy/Coronavirus-Dashboard/2_code/pull_hhs_data.R", 
                     schedule = "HOURLY",
                     startdate = "02-03-2021")

## NYT/CTP data
taskscheduler_delete("pull_CTP_NYT_COVID_data")
taskscheduler_create(taskname = "pull_CTP_NYT_COVID_data", 
                     rscript = "C:/Users/ckelly/Documents/Covid-Personal - Copy/Coronavirus-Dashboard/2_code/covid_script.R", 
                     schedule = "DAILY",
                     startdate = "02-03-2021")

a <- taskscheduler_ls()