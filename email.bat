@echo off
"C:\Program Files\R\R-4.0.3\bin\R.exe" CMD BATCH "C:\Users\jense\Desktop\Projects\covid_email_report_resume\test_automation\master_email.Rmd"
"C:\Program Files\R\R-4.0.3\bin\R.exe" CMD BATCH "C:\Users\jense\Desktop\Projects\covid_email_report_resume\test_automation\send_email.R"
CD "C:\Users\jense\Desktop\Projects\covid_email_report_resume\test_automation\docs"
CALL "C:\Users\jense\Desktop\Projects\covid_email_report_resume\test_automation\docs\update.bat"
CD "C:\Users\jense\Desktop\Projects\covid_email_report_resume\test_automation\master_shiny"
CALL "C:\Users\jense\Desktop\Projects\covid_email_report_resume\test_automation\master_shiny\update.bat"