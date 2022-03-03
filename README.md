## Project Name: Automated email reporting during the COVID-19 Pandemic

#### Description :  
Aim: Reduce anxiety, stress, and misinformation during the COVID-19 pandemic by providing consistent reporting in an easily digestible and accessible format. Take in feedback from end-users/consumers, identify areas of improvement.

#### Contents :
 - "scripts" folder contains all necessary r code/scripts
   - load_data.R : loads and transforms data pulled from NYTimes and JHU githubs.
   - master_email.Rmd : contains the email body and relevant metrics and is formatted for email distribution using Blastula
   - send_email.R : main script that subsequently executes first load_data.R, then master_email.Rmd, and upon rendering a formatted email, distributes the report to recipients.
   - format.R : contains all functions
   - function.R : contains formatting/styling code
 - "raw_data" folder contains updated (2021) population estimates by state and region.
