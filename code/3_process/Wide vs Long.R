# See if narrow vs wide: If one table has more data points than the other

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

# -> Same time frames as above

## --> All narrow vs. wide tables have the same time frames

# Choose preferred format
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

# TRANSFORM: Change back to DATE(TIME)
minute_calories_wide_v5 <- mutate(minute_calories_wide_v4, ActivityMinuteNewNew = ymd_hm(minute_calories_wide_v4$ActivityMinuteNew))

# Clean Up tables and columns
rm(minute_calories_wide_v3, minute_calories_wide_v4)
minute_calories_wide_v5 <- select(minute_calories_wide_v5, -'ActivityMinuteNew')
minute_calories_wide_v5 <- select(minute_calories_wide_v5, -'ActivityHour')
minute_calories_wide_v5 <- rename(minute_calories_wide_v5, ActivityMinute = ActivityMinuteNewNew)
minute_calories_wide_v5 <- arrange(minute_calories_wide_v5, Id, ActivityMinute)

# VALID? See if existing data points are matching
# Would have to create unique id from customer + timepoint and then check values
# of both tables against each other

# Then Left Join former wide (now long) into "narrow"

left_join(x, y, by = c("df1ColName" = "df2ColName")).

# Remove wide tables
rm()

