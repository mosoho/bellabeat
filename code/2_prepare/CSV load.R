# Loading CSVs that failed to upload in SQL & fix them

# Load package(s)
library(tidyverse) # General cleaning package
library(skimr) # To load some data
library(lubridate) # As we have to clean up some dates

# Get right directory - PC
setwd("E:/Dokumente/Ausbildung + Work/5_Weiterbildungen/2023_Google Data Analytics/Practice/Case Study/2/Fitabase Data 4.12.16-5.12.16")
# Get right directory - Laptop
# setwd("D:/Dokumente/Ausbildung + Work/5_Weiterbildungen/2023_Google Data Analytics/Practice/Case Study/2/Fitabase Data 4.12.16-5.12.16")

here::i_am()

# Load all CSVs
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_intensities <- read_csv("dailyIntensities_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")
heartrate_seconds <- read_csv("heartrate_seconds_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_intensities <- read_csv("hourlyIntensities_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
minute_calories_narrow <- read_csv("minuteCaloriesNarrow_merged.csv")
minute_calories_wide <- read_csv("minuteCaloriesWide_merged.csv")
minute_intensities_narrow <- read_csv("minuteIntensitiesNarrow_merged.csv")
minute_intensities_wide <- read_csv("minuteIntensitiesWide_merged.csv")
minute_mets_narrow <- read_csv("minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("minuteStepsNarrow_merged.csv")
minute_steps_wide <- read_csv("minuteStepsWide_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")
weight_log_info <- read_csv("weightLogInfo_merged.csv")
