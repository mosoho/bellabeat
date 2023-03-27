# daily: merged -----------------------------------------------------------
## MERGING tables


# daily_activity + sleep_day = daily_merged

daily_merged <-
  daily_activity %>%
  full_join(sleep_day, by = c('Id'='Id', 'ActivityDate'='SleepDay'))

# Clean up old tables
rm(daily_activity, sleep_day)

# Filling days + weekdays of daily_activity rows for newly added rows
#-> days
daily_merged$Day <- weekdays(daily_merged$ActivityDate)
#-> weekdays
# Add Days to group "weekend" or "week"
daily_merged$Weekday <- case_when(
  daily_merged$Day == "Samstag" ~ "Weekend",
  daily_merged$Day == "Sonntag" ~ "Weekend",
  daily_merged$Day == "Montag" ~ "Week",
  daily_merged$Day == "Dienstag" ~ "Week",
  daily_merged$Day == "Mittwoch" ~ "Week",
  daily_merged$Day == "Donnerstag" ~ "Week",
  daily_merged$Day == "Freitag" ~ "Week",
)

# Adding date to column
daily_merged$Date <- date(daily_merged$ActivityDate)

# Add column for week of the year
daily_merged$WeekISO <- isoweek(daily_merged$Date)

# Rearranging table for readability
daily_merged <- daily_merged %>%
  relocate(Day, .after = ActivityDate) %>%
  relocate(Weekday, .after = Day) %>%
  relocate(Date, .after = ActivityDate)

daily_merged <- arrange(daily_merged, Id, ActivityDate)

#->Relocate
daily_merged <- daily_merged %>%
  relocate(WeekISO, .after = Date)

# hourly ------------------------------------------------------------------
## MERGE

hourly_merged <- hourly_calories %>%
  full_join(hourly_intensities, by = c('Id', 'ActivityHour')) %>%
  full_join(hourly_steps, by = c('Id', 'ActivityHour'))

# Clean up

rm(hourly_calories, hourly_intensities, hourly_steps)


## Prep: Hour & Date
#-> Identify and store Hour of Day
hourly_merged$Hour <- format(as.POSIXct(hourly_merged$ActivityHour),
                             format = "%H")
#-> Identify weekdays
hourly_merged$Day <- weekdays(hourly_merged$ActivityHour)

#-> Group by week & weekend
# Add Days to group "weekend" or "week"
hourly_merged$Week <- case_when(
  hourly_merged$Day == "Samstag" ~ "Weekend",
  hourly_merged$Day == "Sonntag" ~ "Weekend",
  hourly_merged$Day == "Montag" ~ "Week",
  hourly_merged$Day == "Dienstag" ~ "Week",
  hourly_merged$Day == "Mittwoch" ~ "Week",
  hourly_merged$Day == "Donnerstag" ~ "Week",
  hourly_merged$Day == "Freitag" ~ "Week",
)

#-> Identify and store date
hourly_merged$Date <- date(hourly_merged$ActivityHour)
hourly_merged <- hourly_merged %>%
  relocate(Date, .after = ActivityHour) %>%
  relocate(Week, .after = Id) %>%
  relocate(Day, .after = Week) %>%
  relocate(Hour, .after = Date) %>%
  relocate(Calories, .after = Hour)

# minute ------------------------------------------------------------------
## MERGE

# Prepare minute_sleep for merging
minute_sleep$ActivityMinute <- ymd_hms(minute_sleep$ActivityMinute)

# Merge
minute_merged <- minute_calories %>%
  full_join(minute_intensities, by = c('Id', 'ActivityMinute')) %>%
  full_join(minute_mets_narrow, by = c('Id', 'ActivityMinute')) %>%
  full_join(minute_sleep, by = c('Id', 'ActivityMinute')) %>%
  full_join(minute_steps, by = c('Id', 'ActivityMinute'))

# Clean up
rm(minute_calories, minute_intensities, minute_mets_narrow,
   minute_sleep, minute_steps)


## Rename columns from Sleep table for easier readability

minute_merged <- minute_merged %>%
  rename(SleepValue = value, SleepLogId = logId)


## Add Hour + Day + Week(end) + Date columns
#-> Identify and Store Hour of Day
minute_merged$Hour <- format(as.POSIXct(minute_merged$ActivityMinute),
                             format = "%H")
#-> Identify weekdays
minute_merged$Day <- weekdays(minute_merged$ActivityMinute)

# Add Days to group "weekend" or "week"
minute_merged$Week <- case_when(
  minute_merged$Day == "Samstag" ~ "Weekend",
  minute_merged$Day == "Sonntag" ~ "Weekend",
  minute_merged$Day == "Montag" ~ "Week",
  minute_merged$Day == "Dienstag" ~ "Week",
  minute_merged$Day == "Mittwoch" ~ "Week",
  minute_merged$Day == "Donnerstag" ~ "Week",
  minute_merged$Day == "Freitag" ~ "Week",
)

# New column Date
minute_merged$Date <- date(minute_merged$ActivityMinute)
# Re-organize columns
minute_merged <- minute_merged %>%
  relocate(Date, .after = ActivityMinute) %>%
  relocate(Week, .after = Id) %>%
  relocate(Day, .after = Week) %>%
  relocate(Hour, .after = Date) %>%
  relocate(Calories, .after = Hour)
