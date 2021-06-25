# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(blastula) #compose and send emails 
library(lubridate) # dates
library(rio) # input ouput 
library(here) # file path management


Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

# render Rmd into email-able html format
master_body <- render_email(here("scripts", "family.Rmd"))

# Get a nicely formatted date/time string
date_time <- add_readable_time()

# Config Email Credentials - comment out after setting up.
# create_smtp_creds_key(
#  id = "gmail",
#  user = "hu.jensenhu@gmail.com",
#  provider = "gmail",
#  overwrite = T
# )

# Daily Report
master_body %>%
  smtp_send(
    from = "hu.jensenhu@gmail.com",
    to = "jensennhu@gmail.com",
    subject = paste0("COVID-19 Report: ", date_time),
    credentials = creds_key(id = "gmail")
  )

# Weekly Report - runs every Monday
if (wday(ymd(Sys.Date())) == 2)
  master_body %>% 
  smtp_send(from = "hu.jensenhu@gmail.com",
            to = "jensennhu@gmail.com",
            subject = paste0("Weekly Rona' Digest: ", date_time),
            credentials = creds_key(id = "gmail")
  )
