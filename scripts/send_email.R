# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(blastula) #compose and send emails 

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

# load data
source("load_data.R")
source("format.R")
source("function.R")

# render Rmd into email-able html format
master_body <- render_email(".../master_email.Rmd")

# # Get a nicely formatted date/time string
date_time <- add_readable_time()

# create_smtp_creds_key(
#  id = "gmail",
#  user = "my_email_address",
#  provider = "gmail",
#  overwrite = T
# )

master_body %>% 
  smtp_send(
    from = "my_email_address",
    to = "to_email_address",
    subject = paste0("COVID-19 Report: ", date_time),
    credentials = creds_key(id = "gmail")
  )
