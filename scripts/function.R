# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(htmlTable) # convert table to html
library(data.table) # formatting 
library(knitr) # kniting 
library(ggplot2) # visualizations
library(matrixStats) # stats
library(plotly) # visualizations
library(purrr) # functional prog
library(expss) # format labeling 
library(zoo) # working w/time series data
library(blastula) #compose and send emails 
library(ggpubr) # setting multiple plots on one page
library(gridExtra)
library(TTR)

# create metric views for domestic(state, county) and international 
state_fun <- function(place){
  (if (place %in% us_states){
      filter(covid_state, covid_state$state == {place})
      } else if (place %in% us_counties){
        filter(covid_county, covid_county$county == {place})
        } else if (place %in% province){
          filter(international, international$`Province/State` == {place})
        } else if (place %in% country){
          filter(international, international$`Country/Region` == {place}, international$`Province/State` == "")
        } else {
          place
        }) %>% 
    # row difference w/NA padding for x record(s) depending on look back
    transform(cases_new = c(NA, diff(.$cases)),
              cases_new_week = c(rep(NA, 7), diff(.$cases, lag = 7)),
              cases_new_two_week = c(rep(NA, 14), diff(.$cases, lag = 14)),
              deaths_new = c(NA, diff(.$deaths)),
              deaths_new_week = c(rep(NA, 7), diff(.$deaths, lag = 7)),
              deaths_new_two_week = c(rep(NA, 14), diff(.$deaths, lag =  14)))%>% 
    # percent change in cases from yesterday
    mutate(cases_pct_growth = (cases/lag(cases) - 1) * 100,
           # 7 day moving average for new cases 
           cases_week_mavg = EMA(cases_new, n = 7),
           # percent growth in 7 days
           cases_week_pct_growth = (cases/lag(cases, n = 7) - 1) * 100,
           # percent change in 7 day moving average in new cases (from the previous week)
           cases_week_pct_mavg = (cases_week_mavg/lag(cases_week_mavg, n = 7) - 1) * 100,
           # percent change in 7 day moving average in new cases (from the previous day)
           cases_day_pct_mavg = (cases_week_mavg/lag(cases_week_mavg, n = 1) - 1) * 100,
           # Show sign
           sign = sign(cases_week_pct_mavg),
           # 7 day moving average for new deaths
           deaths_week_mavg = EMA(deaths_new, n = 7),
           # percent change in 7 day moving average in new deaths (from the previous week)
           deaths_week_pct_mavg = (deaths_week_mavg/lag(deaths_week_mavg, n = 7) - 1) * 100
           )
}


## function creating body of email
email_body_func <- function(.data){
  # most recent date
  most_recent <- max(.data$date)
  # determine inflection points for 7 day moving average
  updn <- c(0, diff(sign(.data$cases_week_pct_mavg)))
  ix <- which(updn != 0)
  # date of inflection points
  infl_dates <- .data$date[ix]
  # inflection recent (3 days ago)?
  recent_infl <- infl_dates[length(infl_dates)] == most_recent
  # time since last inflection
  time_since_infl <- most_recent - infl_dates[length(infl_dates)] 
  
  # report on the current status
  total_reported <- .data %>% filter(date == most_recent)
  paste0("As of yesterday",
         ", there has been a total of ", 
         formatC(total_reported$cases, big.mark=","), 
         " cases of COVID-19 in " ,
         # county
         (if ("county" %in% colnames(.data)){
           paste0(.data$county[1], ".")
           # state
           } else if ("state" %in% colnames(.data)){
             paste0(.data$state[1], ".")
             # daily_new overall usa 
             } else if (quote(.data) == "daily_new"){
               "the USA."
               # enter nested if for international dataset
               } else if ("Lat" %in% colnames(.data)){
                 # country only
                 if (.data$Province.State[1] == "") {
                   paste0(.data$Country.Region[1], ".")  
                   # province only
                 } else if (.data$Province.State[1] != "") {
                   paste0(.data$Province.State[1], ", ", .data$Country.Region[1], ".") 
                 }
                   } else {
                     "the USA."
                   }),
         " This is a change of ",
         formatC(total_reported$cases_new, big.mark=","),
         " (", round(total_reported$cases_pct_growth, 2),"%)",
         " new cases since the day prior.",
         " The 7 day moving average is currently ",
         formatC(round(.data %>% filter(date == most_recent) %>% select(cases_week_mavg) %>% pull(), 2), big.mark=",", format = "f", digits = 0),
         " new cases which is a ",
         formatC(round(.data %>% filter(date == most_recent) %>% select(cases_day_pct_mavg) %>% pull(), 2), big.mark=","),
         "% change since the day prior and ",
         formatC(round(.data %>% filter(date == most_recent) %>% select(cases_week_pct_mavg) %>% pull(), 2), big.mark=","),
         "% change since the week prior. ",
         "The immediate trend is heading ",
         ifelse((.data %>% 
                   filter(date == most_recent) %>% 
                   select(sign) %>% pull()) < 0, "downward.", "upward."), 
         " Based on the addition of yesterday's data, ",
         ifelse(recent_infl  == FALSE, "yesterday was not an inflection point", paste0("yesterday (", infl_dates[length(infl_dates)], ") was an inflection point")),
         ifelse(recent_infl  == FALSE, paste0(" (days since last inflection point: ", time_since_infl + 1, " --> ", infl_dates[length(infl_dates)], ")."),
                paste0(" (days since last inflection point: ", most_recent - infl_dates[length(infl_dates)-1] + 1 , " --> ", infl_dates[length(infl_dates) - 1], ").")
         )
  )
}






mavg_plot_func <- function(.data){
  require(scales)
  .data %>% 
    ggplot() +
    geom_bar(
      aes(x = date, y = cases_new),
      stat = "identity",
      width = 1.0,
      color = "grey"
    ) +
    geom_line(aes(x = date, y = cases_week_mavg),
              color = "blue",
              size = 2) +
    #ggtitle("7-day Moving Average of \nNew Cases")+
    xlab("Date") +
    ylab("New Cases") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ",")) + 
    scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  # p2 <- .data %>% 
  #   ggplot() +
  #   geom_bar(
  #     aes(x = date, y = deaths_new),
  #     stat = "identity",
  #     width = 1.0,
  #     color = "grey"
  #   ) +
  #   geom_line(aes(x = date, y = deaths_week_mavg),
  #             color = "dark green",
  #             size = 2) +
  #   ggtitle("7-day Moving Average of \nNew Deaths")+
  #   xlab("Date") +
  #   ylab("New Deaths") +
  #   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  #   theme_classic() +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  # grid.arrange(p1, p2, nrow = 1)
}


body_func <- function(test){
  # print email body for specified state
  print(state_set_ebody[[{test}]])  
  # print plot of 7-day mavg of new cases
  print(mavg_plot_func(state_set[[{test}]]))
}


# functions to determine days of consecutive increases/decreases
consec_incr <- function(.data){
  # grab vector
  vector <- .data$sign
  # grab row # of last sign then subtract by last row where not decreasing
  days <- which(.data$date == max(.data$date)) - max(which(vector == -1 | vector == 0))
  data.frame(state = unique(.data$state), days,  cases_week_mavg = .data$cases_week_mavg[.data$date == max(.data$date)] )
}
consec_dcr <- function(.data){
  # grab vector
  vector <- .data$sign
  # grab row # of last sign then subtract by last row where not increasing
  days <- which(.data$date == max(.data$date)) - max(which(vector == 1 | vector == 0))
  data.frame(state = unique(.data$state), days,  cases_week_mavg = .data$cases_week_mavg[.data$date == max(.data$date)])
}


