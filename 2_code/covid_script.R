setwd("C:/Users/ckelly/Documents/Covid-Personal - Copy")

## load packages

library(tidyverse)
library(readxl)
library(readr)
library(rio)
library(anytime)
library(pracma)
library(lubridate)
library(jsonlite)
library(janitor)
library(naniar)


######################################################
# COUNTIES
######################################################

## set GitHub target
counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
regions <- "https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv"

## read data into R
nyt_covid <- read_csv(url(counties))
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
  arrange(stname, ctyname, date)%>%
  group_by(stname, ctyname) %>%
  mutate(n_dates = n()) %>%
  filter(n_dates >=8) %>%
  mutate(cases_prior = lag(cases),
         new_cases = ifelse(is.na(cases - cases_prior), cases, cases - cases_prior),
         deaths_prior = lag(deaths),
         new_deaths = ifelse(is.na(deaths - deaths_prior), deaths, deaths - deaths_prior),
         pos_7d_avg = movavg(new_cases, 7, type = "s"),
         lstwk_pos_7d_avg = lag(pos_7d_avg, k = 7),
         death_7d_avg = movavg(new_deaths, 7, type = "s"),
         lstwk_death_7d_avg = lag(death_7d_avg, k = 7),
         deaths_incr_week = ((death_7d_avg - lstwk_death_7d_avg)/lstwk_death_7d_avg),
         pos_incr_week = ((pos_7d_avg - lstwk_pos_7d_avg)/lstwk_pos_7d_avg))

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


## export to CSV
export(covid_data, "counties-tableau.csv")

######################################################
# STATES
######################################################

## set GitHub target
states <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

## read data into R
nyt_covid_states <- read_csv(url(states))
county_pop <- read_csv("co-est2019-alldata.csv", col_names = TRUE, col_types = cols(.default = "c"))

# check uniqueness
nrow(nyt_covid_states)
nrow(distinct(nyt_covid_states))/nrow(nyt_covid_states)

## subset data for population estimates and clean varnames
county_pop <- county_pop %>%
  select(c("SUMLEV", "REGION", "DIVISION","STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE2019")) %>%
  clean_names()
#county_pop <- county_pop[varnames]
#lowernames <- tolower(colnames(county_pop))
#names(county_pop) <- lowernames

## generate population estimates for counties by adding FIPS code to pop file
county_pop$fips <- county_pop$state

## merge county_pop to NYT data
covid_data_states <- right_join(county_pop, nyt_covid_states, by = "fips")

## arrange
covid_data_states <- covid_data_states %>%
  arrange(stname,date)

## drop counties and deaths
covid_data_states <- covid_data_states %>% 
  filter(ctyname == stname)

## set population variables
covid_data_states <- covid_data_states %>% 
  mutate(pop = as.numeric(popestimate2019),
         caseper1k = cases/pop*1000, 
         deathper1k = deaths/pop*1000)

## get right variable names
varnames <- c("fips", "stname","pop", "cases", "deaths", "caseper1k", "deathper1k", "date")
covid_data_states <- covid_data_states[varnames]

# check uniqueness
nrow(covid_data_states)
nrow(distinct(covid_data_states))/nrow(covid_data_states)

# DC is an issue
covid_data_states <- distinct(covid_data_states)

######################################################
# COVID TRACKING PROJECT
######################################################

# read data
tracking <- fromJSON("https://api.covidtracking.com/v1/states/daily.json")

# select columns
tracking <- tracking %>%
  select(date, state, fips, positive, negative, hospitalizedIncrease, hospitalizedCurrently, totalTestResultsIncrease, positiveIncrease, negativeIncrease, deathIncrease, totalTestResults, death, inIcuCumulative, inIcuCurrently, onVentilatorCurrently, onVentilatorCumulative, positiveCasesViral, totalTestEncountersViral)

# deal with dates
tracking$date <- anydate(as.character(tracking$date))

# get rolling averages
tracking <- tracking %>%
  mutate(pct_positive = positiveIncrease/totalTestResultsIncrease) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(positiveCasesViralIncrease = positiveCasesViral - lag(positiveCasesViral),
         pos_7d_avg = movavg(positiveIncrease, 7, type = "s"),
         death_7d_avg = movavg(deathIncrease, 7, type = "s"),
         totalTestResultsIncrease = ifelse(state == "MN" & date == "2020-07-31", 0, totalTestResultsIncrease),
         totalTestResultsIncrease = ifelse(state == "NC" & date == "2020-08-12", 0, totalTestResultsIncrease),
         test_7d_avg = movavg(totalTestResultsIncrease, 7, type = "s"),
         hosp_7d_avg = movavg(hospitalizedCurrently, 7, type = "s"),
         totaltests_7d = test_7d_avg*7,
         totalpos_7d = pos_7d_avg*7,
         lstwk_death_7d_avg = lag(death_7d_avg, n = 7),
         lstwk_pos_7d_avg = lag(pos_7d_avg, n = 7),
         lstwk_test_7d_avg = lag(test_7d_avg, n = 7),
         changeHosp = hospitalizedCurrently -  lag(hospitalizedCurrently, n = 1),
         changeICU = inIcuCumulative - lag(inIcuCumulative, n = 1),
         deaths_incr_week = ((death_7d_avg - lstwk_death_7d_avg)/lstwk_death_7d_avg),
         pos_incr_week = ((pos_7d_avg - lstwk_pos_7d_avg)/lstwk_pos_7d_avg),
         tests_incr_week = ((test_7d_avg - lstwk_test_7d_avg)/lstwk_test_7d_avg),
         thiswk_pp = totalpos_7d / totaltests_7d,
         lstwk_pp = lag(thiswk_pp, n = 7),
         latest_pos_incr = last(pos_incr_week),
         pp_incr_week = pct_positive - lstwk_pp,
         latest_pp_incr = last(pp_incr_week),
         
         # 12/10 - hospitalizations
         lag7_hosp = lag(hospitalizedCurrently, n = 7),
         hosp_incr_week = (hospitalizedCurrently - lag7_hosp) / lag7_hosp,
         hosp_incr_week = ifelse(is.na(hosp_incr_week), 0, hosp_incr_week),
         hosp_incr_week = last(hosp_incr_week))

# merge with NYT states
covid_data_states <- right_join(covid_data_states, tracking, by = c("fips", "date"))

# add state names to tracking, since it tends to be one day ahead of NYT, can't rely on names there
covid_data_states <- covid_data_states %>%
  mutate(state_abb = state,
         state = if_else(state == "AL", "Alabama", state),
         state = if_else(state == "AK", "Alaska", state),
         state = if_else(state == "AZ", "Arizona", state),
         state = if_else(state == "AR", "Arkansas", state),
         state = if_else(state == "CA", "California", state),
         state = if_else(state == "CO", "Colorado", state),
         state = if_else(state == "CT", "Connecticut", state),
         state = if_else(state == "DE", "Delaware", state),
         state = if_else(state == "DC", "District of Columbia", state),
         state = if_else(state == "FL", "Florida", state),
         state = if_else(state == "GA", "Georgia", state),
         state = if_else(state == "HI", "Hawaii", state),
         state = if_else(state == "ID", "Idaho", state),
         state = if_else(state == "IL", "Illinois", state),
         state = if_else(state == "IN", "Indiana", state),
         state = if_else(state == "IA", "Iowa", state),
         state = if_else(state == "KS", "Kansas", state),
         state = if_else(state == "KY", "Kentucky", state),
         state = if_else(state == "LA", "Louisiana", state),
         state = if_else(state == "ME", "Maine", state),
         state = if_else(state == "MD", "Maryland", state),
         state = if_else(state == "MA", "Massachusetts", state),
         state = if_else(state == "MI", "Michigan", state),
         state = if_else(state == "MN", "Minnesota", state),
         state = if_else(state == "MS", "Mississippi", state),
         state = if_else(state == "MO", "Missouri", state),
         state = if_else(state == "MT", "Montana", state),
         state = if_else(state == "NE", "Nebraska", state),
         state = if_else(state == "NV", "Nevada", state),
         state = if_else(state == "NH", "New Hampshire", state),
         state = if_else(state == "NJ", "New Jersey", state),
         state = if_else(state == "NM", "New Mexico", state),
         state = if_else(state == "NY", "New York", state),
         state = if_else(state == "NC", "North Carolina", state),
         state = if_else(state == "ND", "North Dakota", state),
         state = if_else(state == "OH", "Ohio", state),
         state = if_else(state == "OK", "Oklahoma ", state),
         state = if_else(state == "OR", "Oregon", state),
         state = if_else(state == "PA", "Pennsylvania", state),
         state = if_else(state == "PR", "Puerto Rico", state),
         state = if_else(state == "RI", "Rhode Island ", state),
         state = if_else(state == "SC", "South Carolina", state),
         state = if_else(state == "SD", "South Dakota", state),
         state = if_else(state == "TN", "Tennessee", state),
         state = if_else(state == "TX", "Texas", state),
         state = if_else(state == "UT", "Utah", state),
         state = if_else(state == "VT", "Vermont", state),
         state = if_else(state == "VA", "Virginia", state),
         state = if_else(state == "PR", "Puerto Rico", state),
         state = if_else(state == "VI", "Virgin Islands", state),
         state = if_else(state == "WA", "Washington", state),
         state = if_else(state == "WV", "West Virginia", state),
         state = if_else(state == "WI", "Wisconsin", state),
         state = if_else(state == "WY", "Wyoming", state),
         state = if_else(state == "WA", "Washington", state),
         state = if_else(state == "WA", "Washington", state),
         state = if_else(state == "AS", "American Samoa", state),
         state = if_else(state == "GU", "Guam", state),
         state = if_else(state == "MP", "Northern Marianas", state))

## has your state peaked?
covid_data_states <- covid_data_states %>%
  group_by(state) %>%
  mutate(max_deaths = max(death_7d_avg, na.rm = TRUE),
         max_death_date = (ifelse(death_7d_avg == max_deaths, date, NA)),
         max_death_date = max(max_death_date, na.rm = TRUE),
         max_death_date = as.Date.numeric(max_death_date, origin = "1970-01-01"),
         days_since_peak_death = date - max_death_date,
         perc_decl_fr_death_peak = -(death_7d_avg - max_deaths)/ max_deaths)

covid_data_states <- covid_data_states %>%
  group_by(state) %>%
  mutate(max_pos = max(pos_7d_avg, na.rm = TRUE),
         max_pos_date = (ifelse(pos_7d_avg == max_pos, date, NA)),
         max_pos_date = max(max_pos_date, na.rm = TRUE),
         max_pos_date = as.Date.numeric(max_pos_date, origin = "1970-01-01"),
         days_since_peak_pos = date - max_pos_date)

## get dates
covid_data_states <- covid_data_states %>%
  mutate(max_date = max(date), one_week_date = max_date - 7, two_week_date = max_date - 14,
         max_date_ind = ifelse(max_date == date, "Yes" , "No"),
         one_wk_ind = ifelse(one_week_date == date, "Yes" , "No"),
         two_week_ind = ifelse(two_week_date == date, "Yes" , "No"),
         weekday = weekdays(date),
         max_weekday = weekdays(max_date),
         weekday_match = ifelse(weekday == max_weekday, "Yes", "No"))

## mortality rates
covid_data_states <- covid_data_states %>%
  mutate(mortality_rate = death / positive)

## merge with regions
regions <- regions %>%
  rename(state = stname)
covid_data_states <- right_join(regions, covid_data_states, by = "state")

## deal with RI and OK
covid_data_states <- covid_data_states %>%
  mutate(Region = ifelse(state == "Rhode Island ", "Northeast", Region),
         Region = ifelse(state == "Oklahoma ", "South", Region),
         Division = ifelse(state == "Rhode Island ", "New England", Division),
         Division = ifelse(state == "Oklahoma ", "West South Central", Division))

## is there hospitalization data for this state?
covid_data_states <- covid_data_states %>%
  mutate(month_hosp_flag = ifelse(hospitalizedCurrently > 0 & date == (max(date) - 30), 1, 0),
         today_hosp_flag = ifelse(hospitalizedCurrently > 0 & date == max(date), 1, 0),
         hosp_flag_date = ifelse(hospitalizedCurrently > 0, 1, 0),1) %>%
  group_by(state) %>%
  mutate(hosp_include = sum(month_hosp_flag, today_hosp_flag))

# check uniqueness
nrow(covid_data_states)
nrow(distinct(covid_data_states))/nrow(covid_data_states)

## merge to hexmap
hexmap <- read_excel("Periodic Table Map.xlsx", col_names = TRUE)
hexmap <- hexmap %>% select(Row, Column, state)
covid_data_states <- left_join(covid_data_states, hexmap, by = "state")

## deal with OK and RI for some reason
covid_data_states <- covid_data_states %>%
  mutate(Row = ifelse(state_abb == "RI", 2, Row),
         Column = ifelse(state_abb == "RI", 13, Column),
         Row = ifelse(state_abb == "OK", 4, Row),
         Column = ifelse(state_abb == "OK", 5, Column))

## deaths projection based on state CFR to date
covid_data_states <- covid_data_states %>%
  mutate(deaths_proj_2wks = pos_7d_avg * (deaths / lag(positive, n = 14)))

## election results
election <- read_csv("https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionResultsByState.csv")
election <- election %>% select(state, clintonVotes, trumpVotes, totalVotes) %>%
  mutate(result2016 = if_else(clintonVotes > trumpVotes, "Blue", "Red"))

covid_data_states <- left_join(covid_data_states, election, by = "state")

## governors
gov <- read_csv("https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/source/us-governors.csv")
gov <- gov %>% select(state, party)

## need to update the data (which is from 2019) and deal with RI and OK
covid_data_states <- left_join(covid_data_states, gov, by = "state") %>%
  mutate(party = ifelse(str_detect(state, "Oklahoma"), "republican", party),
         party = ifelse(str_detect(state, "Rhode"), "democrat", party),
         party = ifelse(str_detect(state, "Maine"), "democrat", party))

## deaths lagging
covid_data_states <- covid_data_states %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(deaths_in_14days = lead(death_7d_avg, n = 14),
         deaths_in_21days = lead(death_7d_avg, n = 21),
         deaths_in_28days = lead(death_7d_avg, n = 28))

## ranking states by positive cases
ranking <- covid_data_states %>% filter(max_date_ind == "Yes") %>%
  select(state, pos_7d_avg, pop) %>%
  ungroup() %>%
  mutate(pos_7d_perpop = pos_7d_avg / pop,
         cases_rank = rank(-pos_7d_perpop, na.last = TRUE)) %>%
  select(state, cases_rank)

covid_data_states <- left_join(covid_data_states, ranking , by = "state")

# make sure population is always in
covid_data_states <- covid_data_states %>%
  group_by(state) %>%
  mutate(pop = max(pop, na.rm = TRUE)) %>%
  ungroup()

## export to CSV
export(covid_data_states, "states-tableau.csv")

######################################################
# GOOGLE MOBILITY
######################################################

mobility <- read_csv("Global_Mobility_Report.csv", col_types = cols(sub_region_2 = col_character())) %>%
  
  filter(country_region == "United States") %>%
  rename(state = sub_region_1, county = sub_region_2) %>%
  mutate(county = ifelse(is.na(county), "All Counties", county),
         state = ifelse(is.na(state), "All States", state))

states <- covid_data_states %>% 
  select(state, state_abb) %>%
  distinct()

mobility <- left_join(mobility, states, by = "state")

export(mobility, "mobility.csv")