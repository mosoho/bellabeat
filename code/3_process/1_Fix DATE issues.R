# Dates are formatted as mm/dd/yyyy h:mm:ss am/pm for all tables (Group B), except
# dailyActivity, dailyCalories, dailyIntensities, dailySteps, where the format
# is mm/dd/yyyy (Group A)

## GROUP A: mm/dd/yyyy
# Create fixed columns: CHARACTER to DATE
daily_activity <- daily_activity %>%
  mutate(ActivityDate = mdy(daily_activity$ActivityDate))
daily_calories <- daily_calories %>%
  mutate(ActivityDay = mdy(daily_calories$ActivityDay))
daily_intensities <- daily_intensities %>%
  mutate(ActivityDay = mdy(daily_intensities$ActivityDay))
daily_steps <- daily_steps %>%
  mutate(ActivityDay = mdy(daily_steps$ActivityDay))


## GROUP B: mm/dd/yyyy hh:mm:ss am/pm
# Create fixed columns: CHARACTER to DATE
# All at once
heartrate_seconds <- heartrate_seconds %>%
  mutate(Time = mdy_hms(heartrate_seconds$Time))
hourly_calories <- hourly_calories %>%
  mutate(ActivityHour = mdy_hms(hourly_calories$ActivityHour))
hourly_intensities <- hourly_intensities %>%
  mutate(ActivityHour = mdy_hms(hourly_intensities$ActivityHour))
hourly_steps <- hourly_steps %>%
  mutate(ActivityHour = mdy_hms(hourly_steps$ActivityHour))
minute_calories_narrow <- minute_calories_narrow %>%
  mutate(ActivityMinute = mdy_hms(minute_calories_narrow$ActivityMinute))
minute_calories_wide <- minute_calories_wide %>%
  mutate(ActivityHour = mdy_hms(minute_calories_wide$ActivityHour))
minute_intensities_narrow <- minute_intensities_narrow %>%
  mutate(ActivityMinute = mdy_hms(minute_intensities_narrow$ActivityMinute))
minute_intensities_wide <- minute_intensities_wide %>%
  mutate(ActivityHour = mdy_hms(minute_intensities_wide$ActivityHour))
minute_mets_narrow <- minute_mets_narrow %>%
  mutate(ActivityMinute = mdy_hms(minute_mets_narrow$ActivityMinute))
minute_sleep <- minute_sleep %>%
  mutate(date = mdy_hms(minute_sleep$date))
minute_steps_narrow <- minute_steps_narrow %>%
  mutate(ActivityMinute = mdy_hms(minute_steps_narrow$ActivityMinute))
minute_steps_wide <- minute_steps_wide %>%
  mutate(ActivityHour = mdy_hms(minute_steps_wide$ActivityHour))
sleep_day <- sleep_day %>%
  mutate(SleepDay = mdy_hms(sleep_day$SleepDay))
weight_log_info <- weight_log_info %>%
  mutate(Date = mdy_hms(weight_log_info$Date))
