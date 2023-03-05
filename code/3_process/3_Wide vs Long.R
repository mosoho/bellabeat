# 1. Narrow vs Wide: Difference in data points? ------------------------------


min(minute_calories_narrow_v2$ActivityMinute) # = 2016-04-12 00:00:00 UTC
max(minute_calories_narrow_v2$ActivityMinute) # = 2016-05-12 15:59:00 UTC
# 1 month + 16 hours
# UNIQUE: 2016-04-12 00:00:00 - 2016-04-12 23:59:00

min(minute_calories_wide_v2$ActivityHour) # 2016-04-13 00:00:00 UTC
max(minute_calories_wide_v2$ActivityHour) # 2016-05-13 08:59:00 UTC
# 1 month + 9 hours
# UNIQUE: 2016-05-12 16:00:00 - 2016-05-13 08:59:00

# COMMON: 2016-04-13 00:00:00 - 2016-05-12-15:59:00
# -> So Time points from minute_calories_narrow and _wide need to be combined to
#    give full time frame

min(minute_intensities_narrow_v2$ActivityMinute) # = 2016-04-12 00:00:00
max(minute_intensities_narrow_v2$ActivityMinute) # = 2016-05-12 15:59:00

min(minute_intensities_wide_v2$ActivityHour) # = 2016-04-13 00:00:00
max(minute_intensities_wide_v2$ActivityHour) # = 2016-05-13 08:59:00

# -> Same time frames as above

min(minute_steps_narrow_v2$ActivityMinute) # = 2016-04-12 00:00:00
max(minute_steps_narrow_v2$ActivityMinute) # = 2016-05-12 15:59:00

min(minute_steps_wide_v2$ActivityHour) # = 2016-04-13 00:00:00
max(minute_steps_wide_v2$ActivityHour) # = 2016-05-13 08:59:00

#-> Same time frames as above
#-> All narrow vs. wide tables have the same time frames


# 2. Choose & transform: Into preferred format ------------------------------------


# Lots of data: Long ("narrow") is preferred. So integrate wide data into long
# ("narrow") data

# FORMAT WIDE -> LONG
minute_calories_wide_v3 <- gather(minute_calories_wide_v2, ActivityMinute,
                                  Calories, Calories00:Calories59,
                                  factor_key=TRUE)
# RENAME: Now I could extract all ActivityMinute values to their respective
#         numbers: Last 2 characters. Then remove old column
minute_calories_wide_v3$ActivityMinute_short <- str_sub(minute_calories_wide_v3$ActivityMinute, start = -2)
minute_calories_wide_v3 <- select(minute_calories_wide_v3, -'ActivityMinute')
minute_calories_wide_v3 <- minute_calories_wide_v3 %>%
 rename(ActivityMinute = ActivityMinute_short)
minute_calories_wide_v3 <- relocate(minute_calories_wide_v3, ActivityMinute, .after = ActivityHour)

# TRANSFORM: Change DateType of ActivityHour to Character and only select first
#            14 characters
minute_calories_wide_v3$ActivityHour_CHR <- str_sub(as.character(minute_calories_wide_v3$ActivityHour), 1, 14)

# MERGE: ActivityHour_CHR + ActivityMinute = ActivityMinuteNew
minute_calories_wide_v4 <- minute_calories_wide_v3 %>%
 unite(ActivityMinuteNew, c('ActivityHour_CHR', 'ActivityMinute'), sep = '')

# TRANSFORM: Change back CHARACTER to DATE(TIME)
minute_calories_wide_v5 <- mutate(minute_calories_wide_v4, ActivityMinuteNewNew = ymd_hm(minute_calories_wide_v4$ActivityMinuteNew))

# Clean Up tables and columns
rm(minute_calories_wide_v2, minute_calories_wide_v3, minute_calories_wide_v4)
minute_calories_wide_v5 <- select(minute_calories_wide_v5, -'ActivityMinuteNew')
minute_calories_wide_v5 <- select(minute_calories_wide_v5, -'ActivityHour')
minute_calories_wide_v5 <- rename(minute_calories_wide_v5, ActivityMinute = ActivityMinuteNewNew)
minute_calories_wide_v5 <- arrange(minute_calories_wide_v5, Id, ActivityMinute)


# 3. Validity check: See if matching data points have matching values --------


calories_long <- minute_calories_narrow_v2 %>%
  filter(between(ActivityMinute, as_datetime('2016-04-13 00:00:00'), as_datetime('2016-05-12 15:59:00')))

calories_wide <- minute_calories_wide_v5 %>%
  filter(between(ActivityMinute, as_datetime('2016-04-13 00:00:00'), as_datetime('2016-05-12 15:59:00')))

count(calories_wide) - count(calories_long)
# -> Calories_wide has 5,520 more rows than _long.

# See if/how many NAs there are
sum(is.na(calories_wide$ActivityMinute)) # No NAs
sum(is.na(calories_long$ActivityMinute)) # No NAs
# -> How can 5,520 more rows then be explained?

# Count distinct values
n_distinct(calories_wide$ActivityMinute) # 42,720 distinct values
n_distinct(calories_long$ActivityMinute) # 42,720 distinct values
n_distinct(calories_wide$Calories) # 3870 distinct values
n_distinct(calories_long$Calories) # 3868 distinct values
# -> Same amount of minutes, but _wide has 2 more distinct calorie values

# Skipping quality check as not enough resources to identify what to do with
# 5,520 more rows

#Remove temp tables
rm(calories_long, calories_wide)

# 4. Full Join: narrow_v2 & wide_v5 as minute_calories ---------------------------

minute_calories <- minute_calories_wide_v5 %>%
  full_join(minute_calories_narrow_v2)
# Sort by ID, then ActivityMinute
minute_calories <- arrange(minute_calories, Id, ActivityMinute)

# Remove old/temp tables
rm(minute_calories_narrow_v2,
   minute_calories_wide_v5)

# 5. Recreate 2. + 4. with minute_intensities -------------------------

# FORMAT WIDE -> LONG
minute_intensities_wide_v3 <- gather(minute_intensities_wide_v2, ActivityMinute,
                                  Intensity, Intensity00:Intensity59,
                                  factor_key=TRUE)
# RENAME: Now I could extract all ActivityMinute values to their respective
#         numbers: Last 2 characters. Then remove old column
minute_intensities_wide_v3$ActivityMinute_short <-
  str_sub(minute_intensities_wide_v3$ActivityMinute, start = -2)
minute_intensities_wide_v3 <- select(minute_intensities_wide_v3,
                                     -'ActivityMinute')
minute_intensities_wide_v3 <- minute_intensities_wide_v3 %>%
  rename(ActivityMinute = ActivityMinute_short)
minute_intensities_wide_v3 <- relocate(minute_intensities_wide_v3,
                                       ActivityMinute, .after = ActivityHour)

# TRANSFORM: Change DateType of ActivityHour to Character and only select first
#            14 characters
minute_intensities_wide_v3$ActivityHour_CHR <-
  str_sub(as.character(minute_intensities_wide_v3$ActivityHour), 1, 14)

# MERGE: ActivityHour_CHR + ActivityMinute = ActivityMinuteNew
minute_intensities_wide_v4 <- minute_intensities_wide_v3 %>%
  unite(ActivityMinuteNew, c('ActivityHour_CHR', 'ActivityMinute'), sep = '')

# TRANSFORM: Change back CHARACTER to DATE(TIME)
minute_intensities_wide_v5 <- mutate(minute_intensities_wide_v4,
                                     ActivityMinuteNewNew =
                                       ymd_hm(minute_intensities_wide_v4$ActivityMinuteNew))

# Clean Up tables and columns
rm(minute_intensities_wide_v2, minute_intensities_wide_v3,
   minute_intensities_wide_v4)
minute_intensities_wide_v5 <- select(minute_intensities_wide_v5,
                                  -'ActivityMinuteNew')
minute_intensities_wide_v5 <- select(minute_intensities_wide_v5,
                                  -'ActivityHour')
minute_intensities_wide_v5 <- rename(minute_intensities_wide_v5,
                                  ActivityMinute = ActivityMinuteNewNew)
minute_intensities_wide_v5 <- arrange(minute_intensities_wide_v5, Id,
                                      ActivityMinute)

# Full join
minute_intensities <- minute_intensities_wide_v5 %>%
  full_join(minute_intensities_narrow_v2)
# Sort by ID, then ActivityMinute
minute_intensities <- arrange(minute_intensities, Id, ActivityMinute)

# Remove old/temp tables
rm(minute_intensities_narrow_v2,
   minute_intensities_wide_v5)

# 6. Recreate 2. + 4. with minute_steps ----------------------------------------------

# FORMAT WIDE -> LONG
minute_steps_wide_v3 <- gather(minute_steps_wide_v2, ActivityMinute,
                               Steps, Steps00:Steps59,
                               factor_key=TRUE)
# RENAME: Now I could extract all ActivityMinute values to their respective
#         numbers: Last 2 characters. Then remove old column
minute_steps_wide_v3$ActivityMinute_short <-
  str_sub(minute_steps_wide_v3$ActivityMinute, start = -2)
minute_steps_wide_v3 <- select(minute_steps_wide_v3,
                               -'ActivityMinute')
minute_steps_wide_v3 <- minute_steps_wide_v3 %>%
  rename(ActivityMinute = ActivityMinute_short)
minute_steps_wide_v3 <- relocate(minute_steps_wide_v3,
                                 ActivityMinute, .after = ActivityHour)

# TRANSFORM: Change DateType of ActivityHour to Character and only select first
#            14 characters
minute_steps_wide_v3$ActivityHour_CHR <-
  str_sub(as.character(minute_steps_wide_v3$ActivityHour), 1, 14)

# MERGE: ActivityHour_CHR + ActivityMinute = ActivityMinuteNew
minute_steps_wide_v4 <- minute_steps_wide_v3 %>%
  unite(ActivityMinuteNew, c('ActivityHour_CHR', 'ActivityMinute'), sep = '')

# TRANSFORM: Change back CHARACTER to DATE(TIME)
minute_steps_wide_v5 <- mutate(minute_steps_wide_v4,
                               ActivityMinuteNewNew =
                                 ymd_hm(minute_steps_wide_v4$ActivityMinuteNew))

# Clean Up tables and columns
rm(minute_steps_wide_v2, minute_steps_wide_v3,
   minute_steps_wide_v4)
minute_steps_wide_v5 <- select(minute_steps_wide_v5,
                               -'ActivityMinuteNew')
minute_steps_wide_v5 <- select(minute_steps_wide_v5,
                               -'ActivityHour')
minute_steps_wide_v5 <- rename(minute_steps_wide_v5,
                               ActivityMinute = ActivityMinuteNewNew)
minute_steps_wide_v5 <- arrange(minute_steps_wide_v5, Id,
                                ActivityMinute)

# Full join
minute_steps <- minute_steps_wide_v5 %>%
  full_join(minute_steps_narrow_v2)
# Sort by ID, then ActivityMinute
minute_steps <- arrange(minute_steps, Id, ActivityMinute)

# Remove old/temp tables
rm(minute_steps_narrow_v2,
   minute_steps_wide_v5)


# General reformatting as conclusion  -------------------------------------

daily_activity <- daily_activity_v2
heartrate_seconds <- heartrate_seconds_v2
hourly_calories <- hourly_calories_v2
hourly_intensities <- hourly_intensities_v2
hourly_steps <- hourly_steps_v2
minute_mets_narrow <- minute_mets_narrow_v2
minute_sleep <- minute_sleep_v2
sleep_day <- sleep_day_v2
weight_log_info <- weight_log_info_v2

rm(daily_activity_v2, heartrate_seconds_v2, hourly_calories_v2,
   hourly_intensities_v2, hourly_steps_v2, minute_mets_narrow_v2,
   minute_sleep_v2, sleep_day_v2, weight_log_info_v2)
