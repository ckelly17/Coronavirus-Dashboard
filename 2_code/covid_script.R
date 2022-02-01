setwd("/Users/conorkelly/Documents/COVID")

## load packages

library(tidyverse)
library(readxl)
library(anytime)
library(pracma)
library(lubridate)
library(jsonlite)
library(janitor)
library(naniar)
library(data.table)
library(dtplyr)
library(plotly)

######################################################
# COUNTIES
######################################################

## set GitHub target
counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
regions <- "https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv"

## read data into R
nyt_covid <- fread(counties, colClasses = c("character", "character", "character", 
                                            "character", "numeric", "numeric")) %>%
  as_tibble() %>%
  mutate(date = ymd(date))

max(nyt_covid$date)
#stopifnot(max(nyt_covid$date) == Sys.Date())

regions <- read_csv(url(regions))
county_pop <- read_csv("co-est2019-alldata.csv", col_names = TRUE, col_types = cols(.default = "c"))
msa <- read_csv("cbsatocountycrosswalk.csv", col_names = TRUE, col_types = cols(.default = "c"))
county_codes <- read_excel("all-geocodes-v2018.xlsx", col_names = TRUE, range = "A5:G43852")
density_raw <- read_csv("https://raw.githubusercontent.com/juliachristensen/ps239T-final-project/master/Data_Geo_Sophistication/County_Density_Census/DEC_10_SF1_GCTPH1.US05PR.csv", col_types = cols(.default = "c")) %>%
  row_to_names(1) %>%
  clean_names()


## population density 
density <- density_raw %>%
  select(id, id2, target_geo_id2, geographic_area_2, population, area_in_square_miles_total_area) %>%
  mutate(fips = as.numeric(target_geo_id2),
         pop_density  = as.numeric(population) / as.numeric(area_in_square_miles_total_area)) %>%
  select(fips, pop_density)

## subset data for population estimates and clean varnames
county_pop <- county_pop %>%
  select(c("SUMLEV", "REGION", "DIVISION","STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE2019")) %>%
  clean_names()

## generate population estimates for counties by adding FIPS code to pop and MSA file
county_pop$fips <- paste0(county_pop$state, county_pop$county)
county_pop$fips <- as.numeric(county_pop$fips)

## metro region
msa <- msa %>%
  select(fipscounty, cbsaname, msaname) %>%
  rename(fips = fipscounty) %>%
  mutate(fips = as.numeric(fips))
nrow(msa)
msa <- distinct(msa)
nrow(msa)

## destring NYT fips
nyt_covid$fips <- as.numeric(nyt_covid$fips) 

## merge county_pop to NYT data
covid_data <- right_join(county_pop, nyt_covid, by = "fips") %>%
  arrange(stname, ctyname, date)
covid_data <- right_join(msa, covid_data, by = "fips") %>%
  arrange(stname, ctyname, date)

## merge regions
regions <- regions %>%
  rename(stname = State)
covid_data <- right_join(regions, covid_data, by = "stname")

## deal with unmerged obs
covid_data <- covid_data %>%
  mutate(ctyname = ifelse(is.na(ctyname), county.y, ctyname),
         stname = ifelse(is.na(stname), state.y, stname),
         msaname = ifelse(ctyname == "New York City", "NEW YORK-NEWARK, NY-NJ-PA", msaname))

## merge to population density
covid_data <- left_join(covid_data, density, by = "fips")

## set population variable
covid_data <- covid_data %>% 
  mutate(pop = as.numeric(popestimate2019),
         caseper1k = cases/pop*1000, 
         deathper1k = deaths/pop*1000)

## get right variable names
varnames <- c("fips", "stname", "ctyname","pop", "cases", "deaths", "caseper1k", "deathper1k", "date", "msaname", "cbsaname", "pop_density")
covid_data <- covid_data[varnames]

## make sure fips codes have zero in front if need be
covid_data$fix_fips <- ifelse(covid_data$fips < 10000, 1, 0)
covid_data$fips <- as.character(covid_data$fips)
covid_data$fips <- ifelse(covid_data$fix_fips == 1, paste0("0", covid_data$fips),covid_data$fips)

## calculate new cases and new deaths
covid_data <- covid_data %>%
  lazy_dt() %>%
  arrange(stname, ctyname, date)%>%
  group_by(stname, ctyname) %>%
  mutate(n_dates = n()) %>%
  filter(n_dates >=8) %>%
  mutate(cases_prior = lag(cases),
         new_cases = ifelse(is.na(cases - cases_prior), cases, cases - cases_prior),
         deaths_prior = lag(deaths),
         new_deaths = ifelse(is.na(deaths - deaths_prior), deaths, deaths - deaths_prior),
         pos_7d_avg = movavg(new_cases, 7, type = "s"),
         lstwk_pos_7d_avg = lag(pos_7d_avg, n = 7),
         death_7d_avg = movavg(new_deaths, 7, type = "s"),
         lstwk_death_7d_avg = lag(death_7d_avg, n = 7),
         deaths_incr_week = ((death_7d_avg - lstwk_death_7d_avg)/lstwk_death_7d_avg),
         pos_incr_week = ((pos_7d_avg - lstwk_pos_7d_avg)/lstwk_pos_7d_avg),
         case_7d_15 = lag(pos_7d_avg, 14)) %>%
  as_tibble() %>%
  mutate(pop = ifelse(ctyname == "New York City", 8491000, pop)) %>%
  mutate(case_yest = lag(pos_7d_avg)) %>%
  mutate(deatg_yest = lag(death_7d_avg))
  
  #mutate(one_day_pct_change = (sum(pos_7d_avg, na.rm = T) - lag(sum(pos_7d_avg, na.rm = T))))

# max cases by msa
covid_data <- covid_data %>%
  group_by(ctyname, stname) %>%
  mutate(recent_cases = ifelse(date == max(date, na.rm = TRUE), pos_7d_avg, NA),
         recent_cases = max(recent_cases, na.rm = TRUE),
         
         recent_cases_1wk = ifelse(date == max(date, na.rm = TRUE), lstwk_pos_7d_avg, NA),
         recent_cases_1wk = max(recent_cases_1wk, na.rm = TRUE)) %>%
  ungroup()
  

# check uniqueness
nrow(covid_data)
nrow(distinct(covid_data))
nrow(distinct(covid_data))/nrow(covid_data)
covid_data <- distinct(covid_data)
nrow(distinct(covid_data))/nrow(covid_data)

## elections
cty_election <- read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv")

cty_election <- cty_election %>% 
  select(combined_fips, votes_dem, votes_gop) %>% 
  rename(fips = combined_fips) %>% 
  mutate(fips = as.character(fips),
         result2016 = if_else(votes_dem > votes_gop, "Blue", "Red"))

nrow(distinct(cty_election))

## elections 2020

prez20_raw <- read_csv("https://raw.githubusercontent.com/kjhealy/us_elections_2020_csv/master/results_current.csv")

prez20 <- prez20_raw %>%
  filter(race %in% "President") %>%
  select(fips5, place, lname, votes) %>%
  filter(!is.na(fips5),
         lname %in% c("Biden", "Trump"))

nrow(distinct(prez20, fips5, lname)) / nrow(prez20) # not unique, need to summarize by FIPS

prez20 <- prez20 %>%
  group_by(fips5, lname) %>%
  summarize(votes = sum(votes, na.rm = TRUE))

nrow(distinct(prez20, fips5, lname)) / nrow(prez20) # not unique, need to summarize by FIPS

# pivot wider
prez20_wide <- prez20 %>%
  pivot_wider(id_cols = fips5,
              names_from = lname,
              values_from = votes) %>%
  arrange(fips5) %>%
  mutate(total_votes = Biden + Trump,
         biden_pct = Biden / total_votes,
         biden_pct_bucket = "Biden + 10 or more",
         biden_pct_bucket = ifelse(biden_pct < .55 & biden_pct > .45, "Biden +/- 10", biden_pct_bucket),
         biden_pct_bucket = ifelse(biden_pct <= .45, "Trump +10 or more", biden_pct_bucket)) %>%
  rename(fips = fips5)

## make sure fips codes have zero in front if need be
#prez20_wide$fix_fips <- ifelse(prez20_wide$fips < 10000, 1, 0)
#prez20_wide$fips <- as.character(prez20_wide$fips)
#prez20_wide$fips <- ifelse(prez20_wide$fix_fips == 1, paste0("0", prez20_wide$fips), prez20_wide$fips)

covid_data <- right_join(prez20_wide, covid_data,  by = "fips")


## make sure fips codes have zero in front if need be
cty_election$fix_fips <- ifelse(cty_election$fips < 10000, 1, 0)
cty_election$fips <- as.character(cty_election$fips)
cty_election$fips <- ifelse(cty_election$fix_fips == 1, paste0("0", cty_election$fips), cty_election$fips)

covid_data$fips <- as.numeric(covid_data$fips)
cty_election$fips <- as.numeric(cty_election$fips)

covid_data <- right_join(cty_election, covid_data,  by = "fips")

## deal with unmerged obs
covid_data <- covid_data %>%
  mutate(result2016 = ifelse(ctyname == "New York City", "Blue", result2016))

## dates  
covid_data <- covid_data %>%
  mutate(max_date = max(date), 
         one_week_date = max_date - 7, 
         two_week_date = max_date - 14,
         max_date_ind = ifelse(max_date == date, "Yes" , "No"),
         weekday = weekdays(date),
         max_weekday = weekdays(max_date),
         weekday_match = ifelse(weekday == max_weekday, "Yes", "No"))

## merge with regions
covid_data <- right_join(regions, covid_data, by = "stname")
print(nrow(covid_data))

x <- fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data")
county_vax <- x[[2]] %>%
  clean_names() 

county_vax <- county_vax %>%
  select(fips, series_complete_pop_pct) %>%
  mutate(series_complete_pop_pct = as.numeric(series_complete_pop_pct)) %>%
  
  mutate(full_vax_bin = ifelse(series_complete_pop_pct <= 50, "0-50", NA),
         full_vax_bin = ifelse(series_complete_pop_pct >= 50, "50-60", full_vax_bin),
         full_vax_bin = ifelse(series_complete_pop_pct >= 60, "60-70", full_vax_bin),
         full_vax_bin = ifelse(series_complete_pop_pct >= 70, "70-100", full_vax_bin)) %>%
  
  mutate(fips = as.numeric(fips)) %>%
  distinct(fips, .keep_all = TRUE)

covid_data <- left_join(covid_data, county_vax, by = c("fips"))

## export to CSV
fwrite(covid_data, "counties-tableau.csv")

`Data as of:` <- max(covid_data$date, na.rm =TRUE)


##########################################
##########################################

`Data as of:`
##########################################
##########################################
