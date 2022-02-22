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
library(rsconnect)
library(TTR)
library(hrbrthemes)

# --------- Load Data and Functions ----------####

# get functions
source("function.R")

# load github data from ny times
covid_us <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
covid_state <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid_county <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# load census data from 2010 
census <- fread("../raw_data/NST-EST2021-alldata.csv")

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

# vaccine data 
vaccine <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")



# format dates
covid_us$date <- ymd(covid_us$date)
covid_state$date <- ymd(covid_state$date)
covid_county$date <- ymd(covid_county$date)
international$date <- mdy(international$date)

# dates captured in dataset
days_monitored <- n_distinct(covid_us$date)
first_day <- min(covid_us$date)
most_recent <- max(covid_us$date)


# --------- Flag for old data ----------####
# Flag error and send email notification if data is not updated
if(most_recent + 1 < Sys.Date())
  compose_email(
    body = md(c(
      "Mornin' data has not been updated yet. The email script did not execute past load"
    ))
  ) %>%
  smtp_send(
    from = "my_email_address",
    to = "my_email_address",
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
census_regions <- census %>% 
  rename(us_area = NAME) %>% 
  select(REGION, us_area) %>% 
  head(5)

pop_2021 <- census %>% 
  select(REGION, NAME, POPESTIMATE2021) %>% 
  left_join(census_regions, by = c("REGION" = "REGION"))


# --------- Metrics Calculation and Data Categorization ----------####

# calculate overall us metrics
daily_new <- state_fun(covid_us)
daily_new$cases_week_mavg <- as.numeric(format(daily_new$cases_week_mavg, scientific = F))

# overall USA metric intro
usa_body <- email_body_func(daily_new) 

# df of all USA states/territories - metrics/timeseries
state_metrics <-
  us_states %>% 
  map_df(state_fun) 

# summarized version of state metrics (most recent date)
summarized_state_metrics <- state_metrics %>%
  split(.$state) %>% 
  map_df(~quick_stats_func(.)) %>% 
  left_join(pop_2021, by = c("state" = "NAME")) %>% 
  left_join(
# summarized version of state metrics (previous day and week)
 state_metrics %>% 
  filter(date == most_recent - 1) %>% 
  select(state, cases_week_pct_mavg) %>% 
  rename(prev_pct_mavg = cases_week_pct_mavg) %>% 
  left_join(state_metrics %>% 
              filter(date == most_recent - 7) %>% 
              select(state, cases_week_pct_mavg) %>% 
              rename(prev_week_pct_mavg = cases_week_pct_mavg), 
            by = c('state' = 'state')) %>%
  mutate_at(
    c("prev_pct_mavg","prev_week_pct_mavg"),
    funs(case_when(
      . > 0 ~ "rising",
      . == 0 ~ "staying",
      . < 0 ~ "decreasing"
    ))), by = c('state' = 'state'))



# list of all USA states/territories email bodies
state_set_ebody <-
  state_metrics %>%    
  split(.$state) %>% 
  map(~ email_body_func(.))


# vaccine
vaccine <- vaccine %>% 
  select(date, location, people_fully_vaccinated_per_hundred, total_boosters_per_hundred) %>% 
  filter(date == most_recent) 
vaccine$location[vaccine$location == "New York State"] <- "New York"

# categorize All states/territories by thresholds and trends, join census & hospital data
summarized_state_metrics <- summarized_state_metrics %>% 
  left_join(vaccine, by = c("state" = "location")) %>% 
  mutate(cases_per_capita = cases_week_mavg/POPESTIMATE2021 * 100000) %>% 
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
    ))


# get regions of USA
west <- summarized_state_metrics %>% filter(us_area == "West Region") %>% select(state) %>% pull()
south <- summarized_state_metrics %>% filter(us_area == "South Region") %>% select(state) %>% pull()
northeast <- summarized_state_metrics %>% filter(us_area == "Northeast Region") %>% select(state) %>% pull()
midwest <- summarized_state_metrics %>% filter(us_area == "Midwest Region") %>% select(state) %>% pull()


# consecutive counts
states_incr_df <- summarized_state_metrics %>% 
  filter(trend == "rising") %>% 
  pull(state) %>% 
  # get covid metrics 
  map(state_fun) %>% 
  # determine # days consecutive increase
  map_dfr(consec_incr)

states_dcr_df <- summarized_state_metrics %>% 
  filter(trend == "decreasing") %>% 
  pull(state) %>% 
  # get covid metrics 
  map(state_fun) %>% 
  # determine # days consecutive decrease
  map_dfr(consec_dcr)

# consecutive metrics + state metrics
state_metrics_cat_consec <- states_incr_df %>% 
  bind_rows(states_dcr_df) %>% 
  right_join(summarized_state_metrics, by = c("state" = "state")) %>% 
  filter(us_area != "NA")

state_metrics_cat_consec$cases_per_capita <- round(state_metrics_cat_consec$cases_per_capita, 0)


# save output as Rdata for faster loading

test <- ls() %>% unique()

save(
  test,
  file = "loaded.Rdata")
