# 1. Narrow vs Wide: Difference in data points? ------------------------------


min(minute_calories_narrow$ActivityMinute) # = 2016-04-12 00:00:00 UTC
max(minute_calories_narrow$ActivityMinute) # = 2016-05-12 15:59:00 UTC
# 1 month + 16 hours
# UNIQUE: 2016-04-12 00:00:00 - 2016-04-12 23:59:00

min(minute_calories_wide$ActivityHour) # 2016-04-13 00:00:00 UTC
max(minute_calories_wide$ActivityHour) # 2016-05-13 08:59:00 UTC
# 1 month + 9 hours
# UNIQUE: 2016-05-12 16:00:00 - 2016-05-13 08:59:00

# COMMON: 2016-04-13 00:00:00 - 2016-05-12-15:59:00
# -> So Time points from minute_calories_narrow and _wide need to be combined to
#    give full time frame

min(minute_intensities_narrow$ActivityMinute) # = 2016-04-12 00:00:00
max(minute_intensities_narrow$ActivityMinute) # = 2016-05-12 15:59:00

min(minute_intensities_wide$ActivityHour) # = 2016-04-13 00:00:00
max(minute_intensities_wide$ActivityHour) # = 2016-05-13 08:59:00

# -> Same time frames as above

min(minute_steps_narrow$ActivityMinute) # = 2016-04-12 00:00:00
max(minute_steps_narrow$ActivityMinute) # = 2016-05-12 15:59:00

min(minute_steps_wide$ActivityHour) # = 2016-04-13 00:00:00
max(minute_steps_wide$ActivityHour) # = 2016-05-13 08:59:00

#-> Same time frames as above
#-> All narrow vs. wide tables have the same time frames


# 2. Choose & transform: Into preferred format ------------------------------------


# Lots of data: Long ("narrow") is preferred. So integrate wide data into long
# ("narrow") data

# FORMAT WIDE to LONG
minute_calories_wide <- gather(minute_calories_wide, ActivityMinute,
                               Calories, Calories00:Calories59,
                               factor_key=TRUE)
# RENAME: Now I could extract all ActivityMinute values to their respective
#         numbers: Last 2 characters. Then remove old column
minute_calories_wide$ActivityMinute <- str_sub(minute_calories_wide$ActivityMinute, start = -2)

# TRANSFORM: Change DateType of ActivityHour to Character and only select first
#            14 characters
minute_calories_wide$ActivityHour <- str_sub(as.character(minute_calories_wide$ActivityHour), 1, 14)

# MERGE: ActivityHour_CHR + ActivityMinute = ActivityMinute
minute_calories_wide <- minute_calories_wide %>%
  unite(ActivityMinute, c('ActivityHour', 'ActivityMinute'), sep = '')

# TRANSFORM: Change back CHARACTER to DATE(TIME)
minute_calories_wide <- mutate(minute_calories_wide, ActivityMinute = ymd_hm(minute_calories_wide$ActivityMinute))


# 3. Validity check: See if matching data points have matching values --------


calories_narrow <- minute_calories_narrow %>%
  filter(between(ActivityMinute, as_datetime('2016-04-13 00:00:00'), as_datetime('2016-05-12 15:59:00')))

calories_wide <- minute_calories_wide %>%
  filter(between(ActivityMinute, as_datetime('2016-04-13 00:00:00'), as_datetime('2016-05-12 15:59:00')))

count(calories_wide) - count(calories_narrow)
# -> Calories_wide has 5,520 more rows than _long.

# See if/how many NAs there are
sum(is.na(calories_wide$ActivityMinute)) # No NAs
sum(is.na(calories_narrow$ActivityMinute)) # No NAs
# -> How can 5,520 more rows then be explained?

# Count distinct values
n_distinct(calories_wide$ActivityMinute) # 42,720 distinct values
n_distinct(calories_narrow$ActivityMinute) # 42,720 distinct values
n_distinct(calories_wide$Calories) # 3870 distinct values
n_distinct(calories_narrow$Calories) # 3868 distinct values
# -> Same amount of minutes, but _wide has 2 more distinct calorie values

# Skipping quality check as not enough resources to identify what to do with
# 5,520 more rows

#Remove temp tables
rm(calories_narrow, calories_wide)

# 4. Full Join: narrow_v2 & wide_v5 as minute_calories ---------------------------

minute_calories <- minute_calories_wide %>%
  full_join(minute_calories_narrow)

# Remove old/temp tables
rm(minute_calories_narrow,
   minute_calories_wide)

# 5. Recreate 2. + 4. with minute_intensities -------------------------
# FORMAT WIDE to NARROW
minute_intensities_wide <- gather(minute_intensities_wide, ActivityMinute,
                                  Intensity, Intensity00:Intensity59,
                                  factor_key=TRUE)
# RENAME: Now I could extract all ActivityMinute values to their respective
#         numbers: Last 2 characters. Then remove old column
minute_intensities_wide$ActivityMinute <- str_sub(minute_intensities_wide$ActivityMinute, start = -2)

# TRANSFORM: Change DateType of ActivityHour to Character and only select first
#            14 characters
minute_intensities_wide$ActivityHour <- str_sub(as.character(minute_intensities_wide$ActivityHour), 1, 14)

# MERGE: ActivityHour + ActivityMinute = ActivityMinute
minute_intensities_wide <- minute_intensities_wide %>%
  unite(ActivityMinute, c('ActivityHour', 'ActivityMinute'), sep = '')

# TRANSFORM: Change back CHARACTER to DATE(TIME)
minute_intensities_wide <- mutate(minute_intensities_wide, ActivityMinute = ymd_hm(minute_intensities_wide$ActivityMinute))

# MERGE
minute_intensities <- minute_intensities_wide %>%
  full_join(minute_intensities_narrow)

# Remove old/temp tables
rm(minute_intensities_narrow,
   minute_intensities_wide)

# 6. Recreate 2. + 4. with minute_steps ----------------------------------------------
# FORMAT WIDE to NARROW
minute_steps_wide <- gather(minute_steps_wide, ActivityMinute,
                            Steps, Steps00:Steps59,
                            factor_key=TRUE)
# RENAME: Now I could extract all ActivityMinute values to their respective
#         numbers: Last 2 characters. Then remove old column
minute_steps_wide$ActivityMinute <- str_sub(minute_steps_wide$ActivityMinute, start = -2)

# TRANSFORM: Change DateType of ActivityHour to Character and only select first
#            14 characters
minute_steps_wide$ActivityHour <- str_sub(as.character(minute_steps_wide$ActivityHour), 1, 14)

# MERGE: ActivityHour + ActivityMinute = ActivityMinute
minute_steps_wide <- minute_steps_wide %>%
  unite(ActivityMinute, c('ActivityHour', 'ActivityMinute'), sep = '')

# TRANSFORM: Change back CHARACTER to DATE(TIME)
minute_steps_wide <- mutate(minute_steps_wide, ActivityMinute = ymd_hm(minute_steps_wide$ActivityMinute))

# MERGE
minute_steps <- minute_steps_wide %>%
  full_join(minute_steps_narrow)

# Remove old/temp tables
rm(minute_steps_narrow,
   minute_steps_wide)
