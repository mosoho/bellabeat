# Loading CSVs that failed to upload in SQL & fix them

# Load package(s)
library(tidyverse) # General cleaning package
library(skimr) # To load some data
library(lubridate) # As some dates have to be cleaned up
library(scales) # As graph scaling needs to be modified
library(ggpmisc)

# Load inclusive color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                        "#0072B2", "#D55E00", "#CC79A7")

# Load all CSVs
daily_activity <- read_csv("data/dailyActivity_merged.csv")
daily_calories <- read_csv("data/dailyCalories_merged.csv")
daily_intensities <- read_csv("data/dailyIntensities_merged.csv")
daily_steps <- read_csv("data/dailySteps_merged.csv")
heartrate_seconds <- read_csv("data/heartrate_seconds_merged.csv")
hourly_calories <- read_csv("data/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("data/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("data/hourlySteps_merged.csv")
minute_calories_narrow <- read_csv("data/minuteCaloriesNarrow_merged.csv")
minute_calories_wide <- read_csv("data/minuteCaloriesWide_merged.csv")
minute_intensities_narrow <- read_csv("data/minuteIntensitiesNarrow_merged.csv")
minute_intensities_wide <- read_csv("data/minuteIntensitiesWide_merged.csv")
minute_mets_narrow <- read_csv("data/minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("data/minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("data/minuteStepsNarrow_merged.csv")
minute_steps_wide <- read_csv("data/minuteStepsWide_merged.csv")
sleep_day <- read_csv("data/sleepDay_merged.csv")
weight_log_info <- read_csv("data/weightLogInfo_merged.csv")
