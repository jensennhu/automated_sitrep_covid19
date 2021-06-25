# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(htmlTable) # convert table to html
library(htmltools) # html tools 
library(data.table) # reading data
library(knitr) # kniting 
library(ggplot2) # visualizations
library(matrixStats) # stats
library(plotly) # visualizations
library(purrr) # functional prog
library(expss) # format labeling 
library(zoo) # working w/time series data
library(blastula) #compose and send emails 
library(ggpubr) # setting multiple plots on one page
library(blastula) # sending emails
library(formattable) # tables
library(janitor) # additional agg func
library(ggplot2) # plotting
library(RColorBrewer) # color scheme
library(usmap) # geo
# library(rsconnect) 
# library(TTR)
library(here) # file path management
library(rio) # input output

# --------- Load Data and Functions ----------####

# load github data from ny times
covid_us <- import("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
covid_state <- import("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid_county <- import("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# load census data from 2010 
census <- import(here("raw_data", "nst-est2019-alldata.csv"))
population <- import(here("raw_data", "2019_census_data.csv"), header = TRUE)

# load github data from johns hopkins
jhu_cases <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
jhu_deaths <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
international <- jhu_cases %>% 
  pivot_longer(cols = !c(colnames(jhu_cases)[1:4]), names_to = "date", values_to = "cases") %>% 
  left_join(jhu_deaths %>% 
              pivot_longer(cols = !c(colnames(jhu_cases)[1:4]), names_to = "date", values_to = "deaths") %>% 
              select(-c(Lat, Long)),
            by = c("Province/State" = "Province/State", "Country/Region" = "Country/Region", "date" = "date"))


# vectors for international areas
province <- unique(jhu_cases$`Province/State`)
country <- unique(jhu_cases$`Country/Region`)
country <- country[!country %in% c("Australia", "Canada", "China")]

# statepop
data("statepop")

# vaccine data
vaccine <- import("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv")

# load testing and hospitalization data from covidtrackingproject
ctp_us <- import("https://covidtracking.com/data/download/national-history.csv")

# get functions
source("function.R")

# format dates
covid_us$date <- ymd(covid_us$date)
covid_state$date <- ymd(covid_state$date)
covid_county$date <- ymd(covid_county$date)
ctp_us$date <- ymd(ctp_us$date)
international$date <- mdy(international$date)
# dates captured in dataset
days_monitored <- n_distinct(covid_us$date)
first_day <- min(covid_us$date)
most_recent <- max(covid_us$date)


# --------- Flag for old data ----------####

#Flag error and send email notification if data is not updated
if(most_recent + 1 < Sys.Date())
  compose_email(
    body = md(c(
      "Mornin' data has not been updated yet. The email script did not execute past load"
    ))
  ) %>%
  smtp_send(
    from = "hu.jensenhu@gmail.com",
    to = "jensennhu@gmail.com",
    subject = paste0("NYtimes data not yet loaded: ", add_readable_time()),
    credentials = creds_key(id = "gmail")
  )
if(most_recent != Sys.Date() - 1) stop("NYtimes Data has not been updated")

# --------- Data Prep ----------####

# get states vector
us_states <- unique(covid_state$state)
covid_county$county <- paste(covid_county$county, "County,", covid_county$state)
us_counties <- unique(covid_county$county)

# prepare census data
census <- census %>% select(NAME, REGION, DIVISION, CENSUS2010POP)
census_regions <- census %>% 
  rename(us_area = NAME,
         pop_area = CENSUS2010POP) %>% 
  select(-DIVISION) %>% 
  head(5)
pop_2019 <- population %>% select(V1, "2019") %>% rename(state = V1)
pop_2019$`2019` <- as.numeric(gsub(",", "", pop_2019$`2019`))

# --------- Metrics Calculation and Data Categorization ----------####

# calculate overall us metrics
daily_new <- state_fun(covid_us %>% 
  left_join(ctp_us %>% 
  select(date, hospitalizedIncrease, totalTestResultsIncrease), 
  by = c("date" = "date")))

daily_new$cases_week_mavg <- as.numeric(format(daily_new$cases_week_mavg, scientific = F))

# overall USA metric intro
usa_body <- email_body_func(daily_new) 

# df of all USA states/territories - metrics/timeseries
state_metrics <-
  us_states %>% 
  map_df(state_fun)

# list of all USA states/territories email bodies
state_set_ebody <-
  state_metrics %>%    
  split(.$state) %>% 
  map(~ email_body_func(.))

# categorize All states/territories by thresholds and trends, join census & hospital data
state_metrics_cat <- state_metrics %>%
  filter(date == most_recent) %>% 
  left_join(census, by = c("state" = "NAME")) %>% 
  left_join(statepop, by = c("state" = "full")) %>% 
  left_join(pop_2019, by = c("state" = "state" )) %>% 
  rename(census_2019 = `2019`) %>% 
  mutate(cases_per_capita = cases_week_mavg/census_2019 * 100000) %>% 
  left_join(census_regions, by = c("REGION" = "REGION")) %>% 
  mutate(
    # labeling trend of 7-day mavg ####
    trend = case_when(
      cases_week_pct_mavg > 0 ~ "rising",
      cases_week_pct_mavg == 0 ~ "staying",
      cases_week_pct_mavg < 0 ~ "decreasing"
      ),
    categories = case_when(
      cases_week_pct_mavg > 0 & cases_per_capita >= 15 ~ "Cases higher and rising",
      cases_week_pct_mavg == 0 & cases_per_capita >= 15 ~ "Cases higher and staying",
      cases_week_pct_mavg < 0 & cases_per_capita >= 15 ~ "Cases higher and decreasing",
      cases_week_pct_mavg > 0 & cases_per_capita < 15 ~ "Cases lower and rising",
      cases_week_pct_mavg == 0 & cases_per_capita < 15 ~ "Cases lower and staying",
      cases_week_pct_mavg < 0 & cases_per_capita < 15 ~ "Cases lower and decreasing"
    ),  
    # categories for covid risk level
    covid_risk_level = case_when(
      cases_per_capita >= 25 ~ "Unchecked Community Spread",
      cases_per_capita >= 10 & cases_per_capita < 25 ~ "Escalating Community Spread",
      cases_per_capita >= 1 & cases_per_capita < 10 ~ "Community Spread",
      cases_per_capita < 1 ~ "close to containment"
    ),
    # categories for ph intervention
    ph_intvtn = case_when(
      cases_per_capita >= 25 ~ "Stay-at-Home Orders Necessary",
      cases_per_capita >= 10 & cases_per_capita < 25 ~ "Stay-at-Home Orders And/Or Rigorous Test and Trace Programs Advised",
      cases_per_capita >= 1 & cases_per_capita < 10 ~ "Rigorous Test and Trace Advised",
      cases_per_capita < 1 ~ "Monitor w/testing & tracing"
    )
  ) %>% 
  select(date, state, abbr, fips.x, cases_week_mavg, cases_week_pct_mavg, categories, census_2019, cases_per_capita, us_area, pop_area, trend, covid_risk_level, ph_intvtn)#, collection_date, `Percentage of Staffed Adult ICU Beds Occupied Estimated`, icu_stress, `Percentage of Inpatient Beds Occupied by COVID-19 Patients Estimated`, inpatient_covid_stress) 



# get regions of USA
west <- state_metrics_cat %>% filter(us_area == "West Region") %>% select(state) %>% pull()
south <- state_metrics_cat %>% filter(us_area == "South Region") %>% select(state) %>% pull()
northeast <- state_metrics_cat %>% filter(us_area == "Northeast Region") %>% select(state) %>% pull()
midwest <- state_metrics_cat %>% filter(us_area == "Midwest Region") %>% select(state) %>% pull()


#---------------- 14 day look-back ----------------####
# 
# look_back_fun <- function(area){
#   len <- length(state_fun({area})$sign)
#   fourteen <- state_fun({area})$sign[(len):(len-13)]
#   pos <- length(which(fourteen == 1))
#   neg <- length(which(fourteen == -1))
#   neut <- length(which(fourteen == 0))
#   
#   if (pos > neg){
#     print(paste0({area}, "'s 14-day look-back indicates majority increasing trends"))
#   } else if (pos < neg){
#     print(paste0({area}, "'s 14-day look-back indicates majority decreasing trends"))
#   } else {
#     print(paste0({area}, "'s 14-day look-back indicates a neutral status")) 
#   }
#   
# }
# 
# len <- length(state_fun("California")$sign)
# fourteen <- state_fun("California")$sign[(len-13):(len)]
# magnitude <- state_fun("California")$cases_week_pct_mavg[(len-13):(len)]
# names(magnitude) <- rep(paste0("Day ", 1: length(magnitude)))
# 
# pos <- length(which(fourteen == 1))
# neg <- length(which(fourteen == -1))
# neut <- length(which(fourteen == 0))
# 
# if (pos > neg){
#   print(paste0("Increase"))
# } else if (pos < neg){
#   print(paste0("Decrease"))
# } else {
#   print(paste0("Neutral")) 
# }


states_incr_df <- state_metrics_cat %>% 
  filter(trend == "rising") %>% 
  pull(state) %>% 
  # get covid metrics 
  map(state_fun) %>% 
  # determine # days consecutive increase
  map_dfr(consec_incr)

states_dcr_df <- state_metrics_cat %>% 
  filter(trend == "decreasing") %>% 
  pull(state) %>% 
  # get covid metrics 
  map(state_fun) %>% 
  # determine # days consecutive decrease
  map_dfr(consec_dcr)


# consecutive metrics + state metrics
state_metrics_cat_consec <- states_incr_df %>% 
  bind_rows(states_dcr_df) %>% 
  right_join(state_metrics_cat, by = c("state" = "state")) %>% 
  # add vaccine data
  filter(pop_area != "NA")

state_metrics_cat_consec$cases_per_capita <- round(state_metrics_cat_consec$cases_per_capita, 0)



#---------------- BODY of EMAIL ----------------####
# select states
ca <- state_fun("California")
ny <- state_fun("New York")
il <- state_fun("Illinois")
wa <- state_fun("Washington")
ma <- state_fun("Massachusetts")

# select cities
nyc <- state_fun("New York City County, New York")
la  <- state_fun("Los Angeles County, California")
ck  <- state_fun("Cook County, Illinois")
sf  <- state_fun("Suffolk County, Massachusetts") %>% filter(date > "2020-09-12")
kc  <- state_fun("Wyandotte County, Kansas")

# international locations
hk <- state_fun("Hong Kong")
sg <- state_fun("Shanghai")
tc <- state_fun("Ontario")

# save output as Rdata for faster loading 
vector_set <- ls()
export(vector_set, here("output", "loaded.Rdata"))
# save as RDS for shiny 
saveRDS(us_states, here("master_shiny", "us_states.Rdata"))
saveRDS(covid_state, here("master_shiny", "covid_state.Rdata"))
saveRDS(us_counties, here("master_shiny", "us_counties.Rdata"))
saveRDS(covid_county, here("master_shiny", "covid_county.Rdata"))
saveRDS(international, here("master_shiny", "international.Rdata"))
saveRDS(country, here("master_shiny", "country.Rdata"))
saveRDS(province, here("master_shiny", "province.Rdata"))

