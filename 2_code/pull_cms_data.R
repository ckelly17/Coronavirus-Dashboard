
library(jsonlite)
library(tidyverse)
library(janitor)
library(anytime)
library(pracma)
library(lubridate)
library(data.table)

setwd("/Users/conorkelly/Documents/COVID")

cms_raw <- fromJSON("https://data.cms.gov/data-api/v1/dataset/ce0033b2-368c-4746-9538-e16c737668f0/data") %>%
  clean_names

cms_raw <- fread("https://data.cms.gov/api/views/s2uc-8wxp/rows.csv?accessType=DOWNLOAD&api_foundry=true") %>%
  as_tibble() %>%
  clean_names()

cms <- cms_raw %>%
  group_by(provider_state, week_ending) %>%
  summarize(n_residents = sum(total_number_of_occupied_beds, na.rm = TRUE),
            new_hosp_covid = sum(residents_weekly_admissions_covid_19, na.rm = TRUE),
            new_covid_deaths = sum(residents_weekly_covid_19_deaths, na.rm = TRUE),
            new_covid_cases = sum(residents_weekly_confirmed_covid_19, na.rm = TRUE),
            
            total_covid_deaths = sum(residents_total_covid_19_deaths, na.rm = TRUE),
            total_covid_cases = sum(residents_total_confirmed_covid_19, na.rm = TRUE),
            total_covid_hosp = sum(residents_total_admissions_covid_19, na.rm = TRUE))

cms$max_date <- max(mdy(cms$week_ending), na.rm = TRUE)

print(paste0("Data as of: ", max(cms$max_date)))

write_csv(cms, "cms_nursing_homes.csv")

# percent of national deaths
cd <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")

# get one value for each state and date
nrow(cd %>% distinct(state, submission_date)) / nrow(cd) # good

cdc <- cd %>%
  select(tot_death,
         state,
         date = submission_date) %>%
  mutate(date = anydate(date))

cdc_cms <- cms %>%
  select(new_deaths_nh = new_covid_deaths,
         state = provider_state,
         date = week_ending) %>%
  mutate(date = anydate(date)) %>%
  left_join(cdc, by = c("state", "date")) %>%
  
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(n = row_number()) %>%
  mutate(new_deaths_all = ifelse(n > 1, tot_death - lag(tot_death), NA),
         new_deaths_nh = ifelse(n > 1, new_deaths_nh, NA))

write_csv(cdc_cms, "cms_cdc.csv")

