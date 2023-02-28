# Function to look into data
view_into <- function(df){
  view_into_skim <- skim_without_charts(df)
  view_into_head <- head(df)
  view_into_colnames <- colnames(df)
  view_into_glimpse <- glimpse(df)
  view_into_list <- list("skim_without_charts" = view_into_skim,
                         "head" = view_into_head,
                         "colnames" = view_into_colnames,
                         "glimpse" = view_into_glimpse)
  return(view_into_list)
}

# View into all available data
view_into(daily_activity)
view_into(daily_calories)
view_into(daily_intensities)
view_into(daily_steps)
view_into(heartrate_seconds)
view_into(hourly_calories)
view_into(hourly_intensities)
view_into(hourly_steps)
view_into(minute_calories_narrow)
view_into(minute_calories_wide)
view_into(minute_intensities_narrow)
view_into(minute_intensities_wide)
view_into(minute_mets_narrow)
view_into(minute_sleep)
view_into(minute_steps_narrow)
view_into(minute_steps_wide)
view_into(sleep_day)
view_into(weight_log_info)


#1 View into daily_activity how it's built
# 940 rows
# 31 entries for days (ActivityDate)
# Keys: Id, ActivityDate
# Content: Counting Steps, Distance, Minutes and Calories. Distance + Minutes
#          sorted by not active to most active
# Learning: ActivityDate is CHARACTER instead of DATE

#2 View into daily_calories how it's built
# 940 rows
# 31 entries for days days (ActivityDay)
# Keys: Id, ActivityDay
# Content: Calories per day
# Learnings: ActivityDay is CHARACTER instead of DATE
#            Seems to be redundant as data already in daily_activity

#3 View into daily_calories how it's built
# 940 rows
# 31 entries for days (ActivityDay)
# Keys: Id, ActivityDay
# Content: Daily Minutes and Distance, sorted by not active to most active
# Learnings: ActivityDay is CHARACTER instead of DATE
#            Seems to be redundant as data already in daily_activity

#4 View into daily_steps how it's built
# 940 rows
# 31 entries of days (ActivityDay)
# Keys: Id, ActivityDay
# Content: Number of daily steps
# Learning: ActivityDay is CHARACTER instead of DATE
#           StepTotal seems to be similar to daily_acivity$TotalSteps, however
#           numbers are different -> To clear which column should be used in
#           future

# 5 View into heartrate_seconds how it's built
# 2,483,658 rows
# 961,274 entries of seconds (Time)
# Keys: Id, Time
# Content: What heartrate at what specific second
# Learning: Time is CHARACTER instead of DATE

# 6 View into hourly_calories how it's built
# 22,099 rows
# 736 entries of hours (ActivityHour)
# Keys: Id, ActivityHour
# Content: How many calories (burned?) at which hour
# Learning: ActivityHour is CHARACTER instead of DATE

# 7 View into hourly_intensities how it's built
# 22,099 rows
# 736 entries of hours (ActivityHour)
# Keys: Id, ActivityHour
# Content: Intensity (whatever that means?) in total and in average, per hour
# Learning: ActivityHour is CHARACTER instead of DATE
#           Need to find out what intensity means

# 8 View into hourly_steps how it's built
# 22,099 rows
# 736 entries of hours (ActivityHour)
# Keys: Id, ActivityHour
# Content: Steps done per hour
# Learning: ActivityHour is CHARACTER instead of DATE

# 9 View into minute_calories_narrow how it's built
# 1,325,580 rows
# 44160 entries of minutes (ActivityMinutes)
# Keys: Id, ActivityMinute
# Content: Calories per minute
# Learning: ActivityMinute is CHARACTER instead of DATE

# 10 View into minute_calories_wide how it's built
# 21,645 rows
# 729 entries of hours (ActivityHour)
# Keys: Id, ActivityHour
# Content: Calories per hour in rows, minutes in columns
# Learning: ActivityHour is CHARACTER instead of DATE
#           Identify what the difference in Calories## stands for

# 11 View into minute_intensities_narrow how it's built
# 1,325,580 rows
# 44160 entries of minutes (ActivityMinute)
# Keys: Id, ActivityMinute
# Content: Intensity per minute
# Learning: ActivityMinute is CHARACTER instead of DATE
#           Need to find out what intensity means

# 12 View into minute_intensities_wide how it's built
# 21,645 rows
# 729 entries of hours (ActivityHour)
# Keys: Id, ActivityHour
# Content: Intensity per hours in rows, minutes in columns
# Learning: ActivityHour is CHARACTER instead of DATE
#           Need to find out what intensity means

# 13 View into minute_mets_narrow how it's built
# 1,325,580 rows
# 44,160 entries of minutes (ActivityMinute)
# Keys: Id, ActivityMinute
# Content: METs per minute
# Learning: ActivityMinute is CHARACTER instead of DATE
#           Need to find out what METs means

# 14 View into minute_sleep how it's built
# 188,521 rows
# 49,773 entries of hours (date)
# Keys: Id, date, logId
# Content: Some kind of sleep value per minute
# Learning: date is CHARACTER instead of DATE
#           Need to find out what value in minute_sleep means

# 15 View into minute_steps_narrow how it's built
# 1,325,580 rows
# 44,160 entries of hours (ActivityMinute)
# Keys: Id, ActivityMinute
# Content: How many steps per minute
# Learning: ActivityMinute is CHARACTER instead of DATE

# 16 View into minute_steps_wide how it's built
# 21,645 rows
# 729 entries of hours (ActivityHour)
# Keys: Id, ActivityHour
# Content: Steps per hour in rows, minutes in columns
# Learning: ActivityHour is CHARACTER instead of DATE

# 17 View into sleep_day how it's built
# 413 rows
# 31 entries of hours (SleepDay)
# Keys: Id, SleepDay
# Content: Time spent in bed, time spent asleep and # of sleep records, by day
# Learning: SleepDay is CHARACTER instead of DATE

# 18 View into weight_log_info how it's built
# 67 rows
# 56 entries of hours (Date)
# Keys: Id, Date, LogId
# Content: Display of Weight, Fat and BMI per Timestamp
# Learning: Date is CHARACTER instead of DATE
