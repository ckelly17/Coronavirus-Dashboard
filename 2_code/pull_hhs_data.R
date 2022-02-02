
setwd("/Users/conorkelly/Documents/COVID")

library(jsonlite)
library(tidyverse)
library(janitor)
library(anytime)
library(pracma)
library(lubridate)
library(scales)
library(data.table)
# library(ggplotly)

start <- now()

age <- T

## pull raw data from URLS, each dataframe stored in this list
list(hosp_raw = "https://beta.healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD",
     hosp_daily_raw = "https://beta.healthdata.gov/api/views/6xf2-c3ie/rows.csv?accessType=DOWNLOAD",
     tests_raw = "https://beta.healthdata.gov/api/views/j8mb-icvb/rows.csv?accessType=DOWNLOAD",
     cd_raw = "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>%
  map(fread) %>%
  map(as_tibble) %>%
  list2env(envir = globalenv())

###############################################################
## Pull testing data from HHS ---------------------------------
###############################################################

#https://healthdata.gov/dataset/covid-19-diagnostic-laboratory-testing-pcr-testing-time-series

# clean
tests <-  tests_raw %>%
  mutate(date = anydate(date)) %>%
  rename(type = overall_outcome,
         new_tests = new_results_reported,
         total_tests = total_results_reported) %>%
  select(state, type, date, new_tests, total_tests)

# reshape
tests_wide <- tests %>%
  pivot_wider(id_cols = c("state", "date"),
              names_from = type,
              values_from = c("new_tests", "total_tests")) %>%
  clean_names()

# 7d avg
test_df <- tests_wide %>%
  mutate(new_tests = new_tests_negative + new_tests_positive) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(new_tests_7d_avg = movavg(new_tests, n = 7),
         new_pos_tests_7d_avg = movavg(new_tests_positive, n = 7),
         new_pos_tests_7d_avg_2wk = lag(new_pos_tests_7d_avg, 14),
         pct_pos = new_tests_positive / new_tests,
         last_report_date = max(date, na.rm = TRUE)) %>%   
  ungroup()%>%
  group_by(date) %>%
  mutate(n_states = row_number(),
         n_states = max(n_states, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_states >= 5) %>%
  mutate(test_date = max(date, na.rm = TRUE))

# get new tests each day
daily_total_tests <- test_df %>%
  select(state, new_tests, date, new_tests_positive) %>%
  group_by(state) %>%
  summarize(total_tests = sum(new_tests, na.rm = TRUE),
            total_pos_tests = sum(new_tests_positive, na.rm = TRUE),
            date = max(date, na.rm = TRUE))

daily_tests <- fread("daily_total_tests.csv") %>%
  as_tibble() %>%
  mutate(date = as.Date(date)) %>%
  bind_rows(daily_total_tests) %>%
  distinct() %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(new_reported_tests = total_tests - lag(total_tests, n = 1),
         new_reported_pos_tests = total_pos_tests - lag(total_pos_tests, n = 1))
  
write_csv(daily_total_tests, "daily_total_tests.csv")


###############################################################
## Hospitalizations -----------------------------
###############################################################

## timeseries 

## new HHS beta
url <- "https://beta.healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD"

# read data
hosp <-  hosp_raw %>%
  clean_names()

# clean
hosp_df <- hosp %>%
  select(state,
         date,
         inpatient_beds_used,
         deaths_covid,
         deaths_covid_coverage1 = deaths_covid_coverage,
         staffed_adult_icu_bed_occupancy,
         currently_hospitalized = inpatient_beds_used_covid,
         previous_day_admission_adult_covid_confirmed,
         previous_day_admission_adult_covid_suspected,
         previous_day_admission_pediatric_covid_confirmed,
         previous_day_admission_pediatric_covid_suspected,
         staffed_icu_adult_patients_confirmed_and_suspected_covid,
         previous_day_admission_adult_covid_confirmed,
         previous_day_admission_adult_covid_suspected,
         previous_day_admission_pediatric_covid_suspected,
         previous_day_admission_pediatric_covid_confirmed,
         
         starts_with("previous_day_admission_adult"),
         starts_with("previous_day_admission_pediatric")) %>%
  
  mutate(date = ymd(date),
         new_admit = previous_day_admission_adult_covid_confirmed + previous_day_admission_adult_covid_suspected +
           previous_day_admission_pediatric_covid_suspected + previous_day_admission_pediatric_covid_confirmed) %>%
  select(-ends_with("coverage"))

# max date and max admit date
max_date <- max(hosp_df$date, na.rm = TRUE)
admit_date <- max_date - 0 ##2
hosp_df <- hosp_df %>%
  mutate(admit_date_ind = ifelse(date == admit_date, "Yes", "No"))

hosp_df <- hosp_df %>%
  filter(date != max_date)

## daily
hosp_daily_raw <-  hosp_daily_raw %>%
  clean_names()

# new HHS beta (daily)
archives <- fread("https://healthdata.gov/api/views/4cnb-m4rz/rows.csv?accessType=DOWNLOAD") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(date = mdy(substr(update_date, 1, 10))) %>%
  arrange(desc(date)) %>%
  head(14)

links <- archives$archive_link

dailies2 <- links %>%
  map_dfr(fread) %>%
  as_tibble() %>%
  bind_rows(hosp_daily_raw) %>%
  mutate(provisional = "Yes") %>%
  select(state, currently_hospitalized = inpatient_beds_used_covid, reporting_cutoff_start,
         starts_with("previous_day_admission_adult"),
         starts_with("previous_day_admission_pediatric"),
         staffed_adult_icu_bed_occupancy,
         inpatient_beds_used,
         staffed_icu_adult_patients_confirmed_and_suspected_covid) %>%
  mutate(date = ymd(reporting_cutoff_start) + 4) %>% # change back to 4 tmrw
  select(-ends_with("coverage"))

daily_date <- max(dailies2$date, na.rm = TRUE)

dailies_new <- dailies2 %>% 
  mutate(new_admit = previous_day_admission_adult_covid_confirmed + previous_day_admission_adult_covid_suspected +
           previous_day_admission_pediatric_covid_suspected + previous_day_admission_pediatric_covid_confirmed) %>%
  filter(date >= max_date)

#url <- "https://beta.healthdata.gov/api/views/6xf2-c3ie/rows.csv?accessType=DOWNLOAD"


#write_csv(hosp_daily, paste0("hosp_daily_backup/hosp_", daily_date, ".csv"))

# read in daily data
# files <- list.files("hosp_daily_backup", full.names = TRUE)
# 
# dailies <- files %>%
#   map_dfr(read_csv) %>%
#   mutate(provisional = "Yes") %>%
#   mutate(date = ymd(date),
#          currently_hospitalized = as.numeric(currently_hospitalized))
# 
# dailies_new <- dailies %>% 
#   mutate(new_admit = previous_day_admission_adult_covid_confirmed + previous_day_admission_adult_covid_suspected +
#            previous_day_admission_pediatric_covid_suspected + previous_day_admission_pediatric_covid_confirmed) %>%
#   filter(date >= max_date)

## append
hosp_df <- bind_rows(hosp_df, dailies_new) %>%
  distinct(state, date, .keep_all = TRUE) %>%
  
  # daily change
  group_by(state) %>%
  arrange(state, date) %>%
  
  # TX fix 4/14 and MA 4/15
  mutate(currently_hospitalized = ifelse(state %in% "TX" & date == "2021-04-11", 
                                         lag(currently_hospitalized, 1), currently_hospitalized)) %>%
  
  mutate(currently_hospitalized = ifelse(state %in% "MA" & date == "2021-04-14", 
                                         lag(currently_hospitalized, 1), currently_hospitalized)) %>%
  
  mutate(currently_hospitalized = ifelse(state %in% "CA" & date == "2021-01-14", 
                                         lag(currently_hospitalized, 1), currently_hospitalized)) %>%
  
  mutate(hosp_change = currently_hospitalized - lag(currently_hospitalized),
         total_icu_change = staffed_adult_icu_bed_occupancy - lag(staffed_adult_icu_bed_occupancy),
         total_hosp_change = inpatient_beds_used - lag(inpatient_beds_used),
         icu_change = staffed_icu_adult_patients_confirmed_and_suspected_covid - 
           lag(staffed_icu_adult_patients_confirmed_and_suspected_covid),
         icu_change = ifelse(icu_change > 500, 500, icu_change),
         icu_change = ifelse(icu_change < -500, -500, icu_change),
         
         # fix ICU for illinois
         staffed_icu_adult_patients_confirmed_and_suspected_covid = ifelse((state %in% "IL" & 
                                                                            date > "2021-10-08" &
                                                                            date < "2021-10-29"),
                                                  386, 
                                                  staffed_icu_adult_patients_confirmed_and_suspected_covid)
         
         ) %>%
  mutate(new_admit_7d_avg = movavg(new_admit, n = 7),
         new_admit_avg_1wk = lag(new_admit_7d_avg, 7),
         currently_hospitalized_1wk = lag(currently_hospitalized, 7),
         
         adm_1_wk = lag(new_admit, 7)) %>%
  ungroup()

# daily admissions by age band

if(age == TRUE){
  hosp_age <- hosp_df %>%
    filter(!is.na(previous_day_admission_adult_covid_confirmed_60_69)) %>%
    select(state, date, starts_with("previous_day_admission_adult"),
           starts_with("previous_day_admission_pediatric"),
           -previous_day_admission_adult_covid_confirmed,
           -previous_day_admission_adult_covid_suspected) %>%
    
    pivot_longer(cols = starts_with(c("previous_day_admission_adult", "previous_day_admission_pediatric"))) %>%
    mutate(name = str_remove(name, "previous_day_admission_adult_covid_"),
           name = str_replace(name, "0_", "0 to "),
           name = str_replace(name, "8_", "8 to "),
           status = ifelse(str_detect(name, "confirmed"), "confirmed", "suspected"),
           name = str_remove(name, "confirmed_"),
           name = str_remove(name, "suspected_"),
           name = ifelse(str_detect(name, "pediatric"), "pediatric", name)) %>%
    rename(age_group = name,
           prev_day_admissions = value) %>%
    
    group_by(state, age_group, status) %>%
    arrange(state, age_group, status, date) %>%
    mutate(adm_7d_avg = movavg(prev_day_admissions, n =7))
  
  write_csv(hosp_age, "admissions_ages.csv")
}
###############################################################
## Pull case and death data from CDC ----------------------------
###############################################################

# API
cd <- cd_raw

# # # as backup, get from covid tracker ajax JSON
# ajax <- fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=us_trend_data")
# 
# cd_tracker <- ajax[[2]] %>%
#   clean_names()
# 
# cd_tracker$submission_date <- anydate(cd_tracker$date)
# 
# us_cdc <- cd_tracker %>%
#   select(tot_cases, tot_death = tot_deaths, new_case, new_death, state_name = state, submission_date) %>%
#   arrange(state_name, submission_date) %>%
#   mutate(new_death_cdc_tot = tot_death - lag(tot_death, n = 1),
#          new_case_tot = tot_cases - lag(tot_cases, n = 1)) %>%
#   mutate(new_case_7d_avg = movavg(new_case, n = 7),
#          new_death_7d_avg = movavg(new_death, n = 7),
#          new_death_7d_avg_cdc = movavg(new_death_cdc_tot, n = 7),
#          new_case_tot_7d_avg_cdc = movavg(new_case_tot, n = 7))
# # 
# write_csv(us_cdc, "cdc_backup.csv")

# get one value for each state and date
nrow(cd %>% distinct(state, submission_date)) / nrow(cd) # good

cd <- cd %>% distinct(state, submission_date, .keep_all = TRUE)

case_death_df <- cd %>%
  select(tot_cases,
         tot_death,
         new_case,
         new_death,
         state,
         date = submission_date) %>%
  group_by(state) %>%
  mutate(date = mdy(date)) %>%
  arrange(state, date) %>%
  mutate(new_death = new_death,
         new_death_cdc = tot_death - lag(tot_death, n = 1),
         new_case_tot = tot_cases - lag(tot_cases, n = 1)) %>%
  mutate(new_case_7d_avg = movavg(new_case, n = 7),
         new_death_7d_avg = movavg(new_death, n = 7),
         new_death_7d_avg_cdc = movavg(new_death_cdc, n = 7),
         new_case_tot_7d_avg_cdc = movavg(new_case_tot, n = 7),
         
         case_7d_avg_1_wks = lag(new_case_7d_avg, n = 7),
         case_7d_avg_2_wks = lag(new_case_7d_avg, n = 14),
         case_7d_avg_3_wks = lag(new_case_7d_avg, n = 21),
         case_7d_avg_4_wks = lag(new_case_7d_avg, n = 28),
         case_1_wk = lag(new_case, 7)) %>%
  ungroup()

# filter dates that are incomplete
case_death_df <- case_death_df %>%
  group_by(date) %>%
  mutate(n_states = row_number(),
         n_states = max(n_states, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_states >= 5) %>%
  mutate(case_death_date = max(date, na.rm = TRUE),
         one_wk_ago = case_death_date - 7,
         two_wks_ago = case_death_date - 14)


###############################################################
## vaccines  -----------------------------
###############################################################

vax_raw <- read_csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/cdc_vaccinations_timeseries_daily.csv",
                col_types = cols(administered_dose1_recip_18plus_pop_pct = "d",
                                 administered_dose1_recip_65plus_pop_pct = "d")) %>%
  clean_names()

vax <- vax_raw %>%
  select(state = location,
         administered_dose1_recip_18plus_pop_pct,
         administered_dose1_recip_65plus_pop_pct,
         administered_dose1_recip_12plus_pop_pct,
         administered_dose1_pop_pct,
         date) %>%
  mutate(date = ymd(date))
  
###############################################################
## merge  -----------------------------
###############################################################

# metrics
merge1 <- full_join(case_death_df, test_df, by = c("state", "date"))
df <- full_join(merge1, hosp_df, by = c("state", "date")) %>%
  mutate(hosp_date = max(date, na.rm = TRUE)) # hosp will always be same date as latest case/death

df <- left_join(df, daily_tests, by = c("state", "date"))

df <- left_join(df, vax, by = c("state", "date"))

# regions and state names
regions <- fread("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv") %>%
  as_tibble() %>%
  select(state_name = State,
         state = `State Code`,
         region = `Region`)

# population
pop_raw <- fread("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv") %>%
  as_tibble()

pop <- pop_raw %>%
  select(state, population) 

df <- left_join(df, regions, by = "state") %>%
  mutate(region = ifelse(state %in% "NYC", "Northeast", region))
df <- left_join(df, pop, by = "state") %>%
  mutate(population = ifelse(is.na(population), 0, population))

# max date indicator
df <- df %>%
  mutate(max_date_ind = ifelse(date == max(date, na.rm = TRUE), "Yes", "No"))

# get latest value of hosp, cases, deaths regardless of date
df <- df %>%
  group_by(state) %>%
  mutate(last_hosp = ifelse(is.na(currently_hospitalized), last(currently_hospitalized[!is.na(currently_hospitalized)]), currently_hospitalized)) %>%
  mutate(last_case_7d = ifelse(is.na(new_case_7d_avg), last(new_case_7d_avg[!is.na(new_case_7d_avg)]), new_case_7d_avg)) %>%
  mutate(last_death_7d = ifelse(is.na(new_death_7d_avg), last(new_death_7d_avg[!is.na(new_death_7d_avg)]), new_death_7d_avg)) %>%
  ungroup()

# fill in testing values as zero if they are inside the testing window
df <- df %>%
  mutate(test_date = max(test_date, na.rm = TRUE),
         new_tests2 = new_tests,
         new_tests = ifelse(is.na(new_tests) & date <= test_date, 0, new_tests),
         new_tests_positive2 = new_tests_positive,
         new_tests_positive = ifelse(is.na(new_tests_positive) & date <= test_date, 0, new_tests_positive))
  df <- df %>%  
  # calculate new 7-day averages for tests
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(new_tests_7d_avg = movavg(new_tests, n = 7),
         new_pos_tests_7d_avg = movavg(new_tests_positive, n = 7),
         pct_pos = ifelse(new_tests == 0, 0, new_tests_positive / new_tests)) %>%
  ungroup() %>%
  
  # last week flag
  mutate(lst_wk_flag = ifelse(date + 7 > max(date, na.rm = TRUE), "Y", "N"))

## hex map
hex <- fread("hex.csv") %>%
  as_tibble()
  
df <- left_join(df, hex, by = "state")

# election results 2020
biden_states <- c("AZ", "CA", "CO", "CT", "DE" ,"GA", "HI", "IL", "ME" ,"MD" ,"MA",
                  "MI" ,"MN" , "NV" ,"NH" ,"NJ" ,"NM" ,"NY" ,"OR" ,"PA", "RI" ,
                  "VT" ,"VA", "WA", "WI")

df <- df %>%
  mutate(elect20 = ifelse(state %in% biden_states & !state %in% "PR", "Biden", "Trump"))
df$elect20 <- ifelse(df$state %in% "PR", "NA", df$elect20 )

# add month, day, and year values
df <- df %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE),
         month_num = lubridate::month(date, label = FALSE),
         day = lubridate::day(date),
         display_date = paste0('2021-', month, '-', day))

# add omicron values
df <- df %>%
  mutate(omi_pct = ifelse(date >= "2021-08-01", 0, NA),
         omi_pct = ifelse(date > ymd("2021-12-04") - 7, 0.006, omi_pct),
         omi_pct = ifelse(date > ymd("2021-12-11") - 7, 0.08, omi_pct),
         omi_pct = ifelse(date > ymd("2021-12-18") - 7, 0.379, omi_pct),
         omi_pct = ifelse(date > ymd("2021-12-25") - 7, 0.77, omi_pct),
         omi_pct = ifelse(date > ymd("2022-01-01") - 7, .889, omi_pct),
         omi_pct = ifelse(date > ymd("2022-01-08") - 7, 0.962, omi_pct),
         omi_pct = ifelse(date > ymd("2022-01-15") - 7, 0.978, omi_pct),
         omi_pct = ifelse(date > ymd("2022-01-22") - 7, 0.998, omi_pct),
         omi_pct = ifelse(date > ymd("2022-01-29") - 7, 0.999, omi_pct),
         
         omi_cases = new_case_7d_avg * omi_pct,
         delta_cases = new_case_7d_avg * (1 - omi_pct))

# did it update?
last_cdc <- df %>% filter(!is.na(new_case))
last_test <- df %>% filter(!is.na(new_tests))
last_hosp <- hosp_df %>% filter(!is.na(currently_hospitalized))

## create plot
plot_pct <- function(filter_date, states = 'All', omi = FALSE){

  title_state <- ifelse(states == 'All', "United States", states)
  my_title <- paste0(title_state, ": COVID-19 Cases, Hospitalizations, and Deaths")[1]
  
  if(states != 'All'){
    gg <- df %>%
      filter(state %in% states)
  }else{
    gg <- df
  }
  
  gg <- gg %>%
    filter(date >= date("2020-04-01")) %>%

    select(date, state, new_case_7d_avg, new_death_7d_avg, currently_hospitalized,
           staffed_icu_adult_patients_confirmed_and_suspected_covid) %>%
    group_by(state) %>%
    arrange(state, date) %>%
    mutate(currently_hospitalized = movavg(currently_hospitalized, 7)) %>%
    mutate(icu_7d = movavg(staffed_icu_adult_patients_confirmed_and_suspected_covid, 7)) %>%
    select(-staffed_icu_adult_patients_confirmed_and_suspected_covid)
  
    gg <- gg %>%
      group_by(date)
  
  gg <- gg %>%  
      summarize(new_death_7d_avg = sum(new_death_7d_avg, na.rm = TRUE),
                new_case_7d_avg = sum(new_case_7d_avg, na.rm = TRUE),
                currently_hospitalized = sum(currently_hospitalized, na.rm = TRUE),
                icu_7d = sum(icu_7d, na.rm = TRUE)) %>%
      ungroup()
  
  case_max <- max(gg$new_case_7d_avg[gg$date <= "2021-12-01"], na.rm = TRUE)
  hosp_max <- max(gg$currently_hospitalized[gg$date <= "2021-12-01"], na.rm = TRUE)
  death_max <- max(gg$new_death_7d_avg[gg$date <= "2021-12-01"], na.rm = TRUE)
  icu_max <- max(gg$icu_7d[gg$date <= "2021-12-01"], na.rm = TRUE)
  
  if(omi == FALSE){
  gg <- gg %>%  
    mutate(max_death_pct = new_death_7d_avg /max(gg$new_death_7d_avg[gg$date <= "2021-12-01"], na.rm = TRUE)) %>%
    mutate(max_case_pct = new_case_7d_avg / max(gg$new_case_7d_avg[gg$date <= "2021-12-01"], na.rm = TRUE)) %>%
    mutate(max_hosp_pct = currently_hospitalized / max(gg$currently_hospitalized[gg$date <= "2021-12-01"], na.rm = TRUE)) %>%
    mutate(max_icu_pct = icu_7d / max(gg$icu_7d[gg$date <= "2021-12-01"], na.rm = TRUE)) %>%
    ungroup()
  } else{
    gg <- gg %>%  
      mutate(max_death_pct = new_death_7d_avg /max(gg$new_death_7d_avg[gg$date >= "2021-12-01"], na.rm = TRUE)) %>%
      mutate(max_case_pct = new_case_7d_avg / max(gg$new_case_7d_avg[gg$date >= "2021-12-01"], na.rm = TRUE)) %>%
      mutate(max_hosp_pct = currently_hospitalized / max(gg$currently_hospitalized[gg$date >= "2021-12-01"], na.rm = TRUE)) %>%
      mutate(max_icu_pct = icu_7d / max(gg$icu_7d[gg$date >= "2021-12-01"], na.rm = TRUE)) %>%
      ungroup()  
  }

  plot_df <- gg %>%
    rename(Cases = max_case_pct,
           Deaths = max_death_pct,
           Hospitalized = max_hosp_pct,
           ICU = max_icu_pct)
plot_df <- plot_df %>%
      select(date, Cases, Deaths, Hospitalized, ICU)
  
  plot_df <- plot_df %>%
    pivot_longer(cols = c(Deaths,
                          Cases,
                          Hospitalized,
                          ICU)) %>%
    filter(!value == 0) 

  pct_plot <- plot_df %>%
    filter(year(date) > 2020) %>%
    filter(date >= filter_date) %>%
    
    # plot
    ggplot() +
    
    geom_line(aes(x = date, y = value, color = name), size = 0.8, alpha = 0.9) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    
    theme_minimal() +
    
    # format
    labs(title = my_title,
         subtitle = "7-day averages shown as percentage of pre-Omicron peak values",
         caption = paste0("Data from HHS and CDC as of ", max(last_cdc$date, na.rm = TRUE))) +
    xlab("") +
    ylab("") +
    scale_y_continuous(labels = percent_format(), 
                       breaks = seq(0.00, 10, 0.25),
                       position = "right") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.title=element_blank()) +
    scale_color_manual(values = c("Deaths" = "darkgray",
                                  "Cases" = "forestgreen",
                                  "Hospitalized" = "#6495ED",
                                  "ICU" = "navy")) +
    theme(plot.title=element_text(face="bold"),
          text = element_text(size = 10),
          plot.caption = element_text(hjust = 0))
  
  fig <- ggplotly(pct_plot)
  htmlwidgets::saveWidget(fig, "pct_fig.html")
  browseURL("pct_fig.html")
}

### plot
plot_pct(filter_date = "2021-07-01", omi = FALSE)
plot_pct(states = 'NY', omi = TRUE, filter_date = "2021-09-01")

source("/Users/conorkelly/Documents/COVID/update_github.R")

# write to disk 
write_csv(df, "federal_state.csv")

# check hosp values
hosp_df %>%
  filter(date >= max(date, na.rm = TRUE) - 7) %>%
  group_by(date) %>%
  summarize(currently_hospitalized = sum(currently_hospitalized, na.rm = TRUE)) %>%
  mutate(change = currently_hospitalized - lag(currently_hospitalized))


# updates
print(paste0("Last HHS Hosp Date: ", max(last_hosp$date, na.rm = TRUE)))
print(paste0("Last CDC Date: ", max(last_cdc$date, na.rm = TRUE)))
print(paste0("Last Testing Date: ", max(last_test$date, na.rm = TRUE)))
print(paste0("Total time: ", now() - start))



