# library(devtools)
# install_github("corynissen/fitbitScraper")

library(fitbitScraper)
library(yaml)
library(magrittr)
library(data.table)

creds <- yaml.load_file("creds.yaml")
creds <- creds[["fitbit"]]

cookie <- login(email    = creds$email,
                password = creds$password)

# 15_min_data "what" options: "steps", "distance", "floors", "active-minutes", "calories-burned"
steps <- get_daily_data(cookie, what = "steps",
                        start_date   = "2011-01-01",
                        end_date     = as.character(Sys.Date()-1)) %>%
  data.table %>%
  setnames("time", "report_date")

saveRDS(steps, file = "data/fitbit_steps.Rds")

resting.hr <- get_daily_data(cookie, what = "getRestingHeartRateData",
                                      start_date   = "2015-01-01",
                                      end_date     = as.character(Sys.Date()-1)) %>%
  data.table %>%
  setnames(c("report_date", "resting_hr"))

saveRDS(resting.hr, file = "data/fitbit_resting_hr.Rds")

