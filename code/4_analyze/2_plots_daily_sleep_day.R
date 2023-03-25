#### PLOTS for identifying trends

# Check Relation between 'TotalMinutesAsleep' vs 'TotalTimeInBed' ---------


ggplot(data = sleep_day, mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +geom_point()
# Outliers. Highlight them for better visualization
# -> Filter everything thats above a certain ratio
sleep_day$RatioBedVsAsleep <- sleep_day$TotalTimeInBed / sleep_day$TotalMinutesAsleep
# -> Set new column next to TotalTimeInBed
sleep_day <- sleep_day %>%
  relocate(RatioBedVsAsleep, .after = TotalTimeInBed)
# -> Sort by ratio in descending order
sleep_day <- sleep_day %>%
  arrange(desc(RatioBedVsAsleep))
# -> Check median to find useful threshold -> 1.2
summary(sleep_day$RatioBedVsAsleep)
# -> Filter by 1.2
sleep_day_ratio_above1.2 <- sleep_day %>%
  filter(RatioBedVsAsleep>=1.2)
# Display highlight on ggplot to check if all outliers are identified
sleep_day %>%
  ggplot(aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
  geom_point(alpha=0.3) +
  geom_point(data = sleep_day_ratio_above1.2,
             aes(x = TotalMinutesAsleep, y = TotalTimeInBed),
             color = 'red')
# -> All these people spend at least 20% time in bed awake. How many are affected?
nrow(sleep_day_ratio_above1.2)
#-> 32 entries
nrow(sleep_day_ratio_above1.2 %>%
       distinct(Id))
#-> 3 people. Out of...
nrow(distinct(sleep_day, Id))
#-> 24 = 12,5%
# Clean up
rm(sleep_day_ratio_above1.2)



# Check relation between 'SleepDay' and 'TotalMinutesAsleep' --------------


sleep_day %>%
  ggplot(aes(x = SleepDay, y = TotalMinutesAsleep)) +
  geom_point()
#-> Identify weekdays
sleep_day$SleepDay_Day <- weekdays(mdy_hms(sleep_day$SleepDay))
#-> Update plot with new variable
sleep_day %>%
  ggplot(aes(x = SleepDay_Day, y = TotalMinutesAsleep)) +
  geom_point()
#-> Arrange plot to sort by days of week (Sunday to Saturday)
sleep_day %>%
  ggplot(aes(x = factor(SleepDay_Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalMinutesAsleep)) +
  geom_boxplot() +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.5), size=3) +
  xlab('Wochentage')
#-> Specifically filter by Saturday + Sunday
sleep_day %>%
  filter(SleepDay_Day %in% c("Samstag", "Sonntag")) %>%
  ggplot(aes(x = SleepDay_Day,
             y = TotalMinutesAsleep)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.5, width = 0.3) +
  xlab('Wochentage')
#-> Group by week & weekend
# Add Days to group "weekend" or "week"
sleep_day$SleepDay_Group <- case_when(
  sleep_day$SleepDay_Day == "Samstag" ~ "Weekend",
  sleep_day$SleepDay_Day == "Sonntag" ~ "Weekend",
  sleep_day$SleepDay_Day == "Montag" ~ "Week",
  sleep_day$SleepDay_Day == "Dienstag" ~ "Week",
  sleep_day$SleepDay_Day == "Mittwoch" ~ "Week",
  sleep_day$SleepDay_Day == "Donnerstag" ~ "Week",
  sleep_day$SleepDay_Day == "Freitag" ~ "Week",
)
# Display ggplot differentiating between week & weekend
sleep_day %>%
  ggplot(aes(x = SleepDay_Group,
             y = TotalMinutesAsleep)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Minutes Asleep')
# Now with minutes in bed
sleep_day %>%
  ggplot(aes(x = SleepDay_Group,
             y = TotalTimeInBed)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Minutes in Bed')

## Check outliers in Total Time In Bed
sleep_day <- sleep_day %>%
  arrange(desc(TotalTimeInBed))
