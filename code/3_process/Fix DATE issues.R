# Dates are formatted as mm/dd/yyyy h:mm:ss am/pm for all tables (Group B), except
# dailyActivity, dailyCalories, dailyIntensities, dailySteps, where the format
# is mm/dd/yyyy (Group A)

## GROUP A: mm/dd/yyyy
# Create fixed columns: CHARACTER to DATE
daily_activity_v2 <- mutate(daily_activity, ActivityDateFixed = mdy(daily_activity$ActivityDate))
daily_calories_v2 <- mutate(daily_calories, ActivityDayFixed = mdy(daily_calories$ActivityDay))
daily_intensities_v2 <- mutate(daily_intensities, ActivityDayFixed = mdy(daily_intensities$ActivityDay))
daily_steps_v2 <- mutate(daily_steps, ActivityDayFixed = mdy(daily_steps$ActivityDay))

# Remove old CHARACTER Date columns
daily_activity_v2 <- select(daily_activity_v2, -'ActivityDate')
daily_calories_v2 <- select(daily_calories_v2, -'ActivityDay')
daily_intensities_v2 <- select(daily_intensities_v2, -'ActivityDay')
daily_steps_v2 <- select(daily_steps_v2, -'ActivityDay')

# Rename new Date columns
daily_activity_v2 <- rename(daily_activity_v2, 'ActivityDate' = 'ActivityDateFixed')
daily_calories_v2 <- rename(daily_calories_v2, 'ActivityDay' = 'ActivityDayFixed')
daily_intensities_v2 <- rename(daily_intensities_v2, 'ActivityDay' = 'ActivityDayFixed')
daily_steps_v2 <- rename(daily_steps_v2, 'ActivityDay' = 'ActivityDayFixed')

# Put new column in 2nd position 

## GROUP B: mm/dd/yyyy h:mm:ss am/pm
# Create fixed columns: CHARACTER to DATE
heartrate_seconds_v2 <- mutate(heartrate_seconds, TimeFixed = mdy_hms(heartrate_seconds$Time))

hourly_calories_v2 <- mutate(hourly_calories, ActivityHourFixed = mdy_hms(hourly_calories$ActivityHour))

hourly_intensities_v2 <- mutate(hourly_intensities, ActivityHourFixed = mdy_hms(hourly_intensities$ActivityHour))

hourly_steps_v2 <- mutate(hourly_steps, ActivityHourFixed = mdy_hms(hourly_steps$ActivityHour))

minute_calories_narrow_v2 <- mutate(minute_calories_narrow, ActivityMinuteFixed = mdy_hms(minute_calories_narrow$ActivityMinute))

minute_calories_wide_v2 <- mutate(minute_calories_wide, ActivityHourFixed = mdy_hms(minute_calories_wide$ActivityHour))

minute_intensities_narrow_v2 <- mutate(minute_intensities_narrow, ActivityMinuteFixed = mdy_hms(minute_intensities_narrow$ActivityMinute))

minute_intensities_wide_v2 <- mutate(minute_intensities_wide, ActivityHourFixed = mdy_hms(minute_intensities_wide$ActivityHour))

minute_mets_narrow_v2 <- mutate(minute_mets_narrow, ActivityMinuteFixed = mdy_hms(minute_mets_narrow$ActivityMinute))

minute_sleep_v2 <- mutate(minute_sleep, dateFixed = mdy_hms(minute_sleep$date))

minute_steps_narrow_v2 <- mutate(minute_steps_narrow, ActivityMinuteFixed = mdy_hms(minute_steps_narrow$ActivityMinute))

minute_steps_wide_v2 <- mutate(minute_steps_wide, ActivityHourFixed = mdy_hms(minute_steps_wide$ActivityHour))

sleep_day_v2 <- mutate(sleep_day, SleepDayFixed = mdy_hms(sleep_day$SleepDay))

weight_log_info_v2 <- mutate(weight_log_info, DateFixed = mdy_hms(weight_log_info$Date))

# Remove old CHARACTER Date columns

heartrate_seconds_v2 <- select(heartrate_seconds_v2, -'Time')

hourly_calories_v2 <- select(hourly_calories_v2, -'ActivityHour')

hourly_intensities_v2 <- select(hourly_intensities_v2, -'ActivityHour')

hourly_steps_v2 <- select(hourly_steps_v2, -'ActivityHour')

minute_calories_narrow_v2 <- select(minute_calories_narrow_v2, -'ActivityMinute')

minute_calories_wide_v2 <- select(minute_calories_wide_v2, -'ActivityHour')

minute_intensities_narrow_v2 <- select(minute_intensities_narrow_v2, -'ActivityMinute')

minute_intensities_wide_v2 <- select(minute_intensities_wide_v2, -'ActivityHour')

minute_mets_narrow_v2 <- select(minute_mets_narrow_v2, -'ActivityMinute')

minute_sleep_v2 <- select(minute_sleep_v2, -'date')

minute_steps_narrow_v2 <- select(minute_steps_narrow_v2, -'ActivityMinute')

minute_steps_wide_v2 <- select(minute_steps_wide_v2, -'ActivityHour')

sleep_day_v2 <- select(sleep_day_v2, -'SleepDay')

weight_log_info_v2 <- select(weight_log_info_v2, -'Date')

# Rename new Date columns
heartrate_seconds_v2 <- rename(heartrate_seconds_v2, 'Time' = 'TimeFixed')

hourly_calories_v2 <- rename(hourly_calories_v2, 'ActivityHour' = 'ActivityHourFixed')

hourly_intensities_v2 <- rename(hourly_intensities_v2, 'ActivityHour' = 'ActivityHourFixed')

hourly_steps_v2 <- rename(hourly_steps_v2, 'ActivityHour' = 'ActivityHourFixed')

minute_calories_narrow_v2 <- rename(minute_calories_narrow_v2, 'ActivityMinute' = 'ActivityMinuteFixed')

minute_calories_wide_v2 <- rename(minute_calories_wide_v2, 'ActivityHour' = 'ActivityHourFixed')

minute_intensities_narrow_v2 <- rename(minute_intensities_narrow_v2, 'ActivityMinute' = 'ActivityMinuteFixed')

minute_intensities_wide_v2 <- rename(minute_intensities_wide_v2, 'ActivityHour' = 'ActivityHourFixed')

minute_mets_narrow_v2 <- rename(minute_mets_narrow_v2, 'ActivityMinute' = 'ActivityMinuteFixed')

minute_sleep_v2 <- rename(minute_sleep_v2, 'date' = 'dateFixed')

minute_steps_narrow_v2 <- rename(minute_steps_narrow_v2, 'ActivityMinute' = 'ActivityMinuteFixed')

minute_steps_wide_v2 <- rename(minute_steps_wide_v2, 'ActivityHour' = 'ActivityHourFixed')

sleep_day_v2 <- rename(sleep_day_v2, 'SleepDay' = 'SleepDayFixed')

weight_log_info_v2 <- rename(weight_log_info_v2, 'Date' = 'DateFixed')

# Put new column in 2nd position (Just mark how to do it)
daily_activity_v2 <- daily_activity_v2 %>% 
relocate(ActivityDate, .after = Id)

minute_calories_wide_v2 <- minute_calories_wide_v2 %>% 
  relocate(ActivityHour, .after = Id)


# Clean Up: Remove old tables
rm(daily_activity, daily_intensities, daily_calories, daily_steps,
   heartrate_seconds, hourly_calories, hourly_intensities, hourly_steps,
   minute_calories_narrow, minute_calories_wide, minute_intensities_narrow,
   minute_intensities_wide, minute_mets_narrow, minute_sleep,
   minute_steps_narrow, minute_steps_wide, sleep_day, weight_log_info)
