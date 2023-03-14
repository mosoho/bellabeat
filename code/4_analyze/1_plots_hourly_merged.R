# MERGE -------------------------------------------------------------------

hourly_merged <- hourly_calories %>%
  full_join(hourly_intensities, by = c('Id', 'ActivityHour')) %>%
  full_join(hourly_steps, by = c('Id', 'ActivityHour'))

# Clean up

rm(hourly_calories, hourly_intensities, hourly_steps)


# by Hour of Day: TotalIntensity ----------------------------------------------------------

#-> Identify and Store Hour of Day
hourly_merged$Hour <- format(as.POSIXct(hourly_merged$ActivityHour),
                             format = "%H")

#-> Add plot, sorted by Hours of day
hourly_merged %>%
  ggplot(aes(x = Hour,
             y = TotalIntensity)) +
  geom_col() +
  xlab('Hours of Day')

# Users are most active in intense moments between 5 and 7pm. Least active
# between 12pm and 4am.

#Check mean of TotalIntensity by Hour
intensity_by_hour <- hourly_merged %>%
  group_by(Hour) %>%
  summarise(Mean = mean(TotalIntensity), SD = sd(TotalIntensity))

intensity_by_hour %>%
  ggplot(aes(x = Hour,
             y = Mean)) +
  geom_col()

# Highest intensity collected daily is reached between 5 and 7pm with # 21 - 22
# Lowest from 12pm to 4am with 0.5 - 2

#cleanup
rm(intensity_by_hour)


# by Hour of Day: Calories------------------------------------------------------

#-> Add plot, sorted by Hours of day
hourly_merged %>%
  ggplot(aes(x = Hour,
             y = Calories)) +
  geom_col() +
  xlab('Hours of Day')

# Skip to next graph

#Check mean of Calories by Hour
calories_by_hour <- hourly_merged %>%
  group_by(Hour) %>%
  summarise(Mean = mean(Calories), SD = sd(Calories))

calories_by_hour %>%
  ggplot(aes(x = Hour,
             y = Mean)) +
  geom_col()

# Lowest calories 67.5 burned is at 4am. Highest at 6pm with 123.5. So even in
# sleep users burn half of maximum calories that can be burned.

#cleanup
rm(calories_by_hour)



# by Hour of Day: StepTotal----------------------------------------------------

#-> Add plot, sorted by Hours of day
hourly_merged %>%
  ggplot(aes(x = Hour,
             y = StepTotal)) +
  geom_col() +
  xlab('Hours of Day')

# Similar findings as by hour, so skip

#Check mean of StepTotal by Hour
steptotal_by_hour <- hourly_merged %>%
  group_by(Hour) %>%
  summarise(Mean = mean(StepTotal), SD = sd(StepTotal))

steptotal_by_hour %>%
  ggplot(aes(x = Hour,
             y = Mean)) +
  geom_col()

# Highest StepTotal collected daily is reached between 12 and 7pm with
# 538 to 599, however with a dip at 3 and 4pm up to 406
# Lowest from 12pm to 4am with 6.4 - 43.8

#cleanup
rm(steptotal_by_hour)



# by Day & Week(end): TotalIntensity --------------------------------------------------

#-> Identify weekdays
hourly_merged$Day <- weekdays(hourly_merged$ActivityHour)
#-> Add plot, sorted by days of week (Sunday to Saturday)
hourly_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalIntensity)) +
  geom_col() +
  xlab('Wochentage')

# Tuesday is day with highest intensity in total

#Check mean of TotalIntensity by Day
intensity_by_day <- hourly_merged %>%
  group_by(Day) %>%
  summarise(Mean = mean(TotalIntensity), SD = sd(TotalIntensity))

intensity_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  xlab('Woche')

# In general similar levels of high intensity. Pattern of increasing levels
# from Sunday to Tuesday, then drop on Wednesday, followed by increase until
# Saturday. Then drop to Sunday.
# Highest Saturday 12.9, lowest Sunday 10.9

#cleanup
rm(intensity_by_day)

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
# Display ggplot differentiating between week & weekend
#-> Check mean of TotalIntensity by Week
intensity_by_week <- hourly_merged %>%
  group_by(Week) %>%
  summarise(Mean = mean(TotalIntensity), SD = sd(TotalIntensity))

intensity_by_week %>%
  ggplot(aes(x = Week,
             y = Mean)) +
  geom_col() +
  xlab('Week')

# No significant difference in intensity based on week or weekend

#cleanup
rm(intensity_by_week)



# by Day & Week(end): Calories --------------------------------------------
library(scales)
hourly_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Calories)) +
  geom_col() +
  scale_y_continuous(labels = label_comma()) +
  xlab('Wochentage')


# Most data sum is Tuesday. Then decreasing until Sunday/Monday

#-> Group by week & weekend
# Display ggplot differentiating between week & weekend
#-> Check mean of TotalIntensity by Week
calories_by_week <- hourly_merged %>%
  group_by(Week) %>%
  summarise(Mean = mean(Calories), SD = sd(Calories))

calories_by_week %>%
  ggplot(aes(x = Week,
             y = Mean)) +
  geom_col() +
  xlab('Week')

# No significant difference in Calories based on week or weekend

#cleanup
rm(calories_by_week)

# by Day & Week(end): StepTotal -------------------------------------------

hourly_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = StepTotal)) +
  geom_col() +
  xlab('Wochentage')

# Identify sum, mean and sd of StepTotal by Day
steps_by_day <- hourly_merged %>%
  group_by(Day) %>%
  summarise(Sum = sum(StepTotal), Mean = mean(StepTotal), SD = sd(StepTotal))

# Overall steps collected highest on Tuesday (1,204k) and lowest on
# Sunday (835k)

steps_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  ylab('TotalSteps') +
  xlab('Woche')

# Saturday highest with 344 and Sunday lowest with 288


#-> Group by week & weekend
# Identify sum, mean and sd of StepTotal by Week
steps_by_week <- hourly_merged %>%
  group_by(Week) %>%
  summarise(Sum = sum(StepTotal), Mean = mean(StepTotal), SD = sd(StepTotal))


# Display ggplot differentiating between week & weekend
steps_by_week %>%
  ggplot(aes(x = Week,
             y = Mean)) +
  geom_col() +
  xlab('Week vs Weekend') +
  ylab('StepsTotal')

#-> No difference in StepTotal based on Day in Week or at Weekend

#Clean Up
rm(steps_by_day, steps_by_week)

# Calories ------------------- ---------------------------------------------

## vs. TotalIntensity
hourly_merged %>%
  ggplot(aes(x = TotalIntensity, y = Calories)) +
  geom_point() +
  geom_smooth()

cor(hourly_merged$Calories, hourly_merged$TotalIntensity)
#Progressive increase of Calories burned by each TotalInensity increased


## vs. AverageIntensity

hourly_merged %>%
  ggplot(aes(x = AverageIntensity, y = Calories)) +
  geom_point() +
  geom_smooth()
#Progressive increase of Calories burned by each AverageIntensity increased


## vs. StepTotal

hourly_merged %>%
  ggplot(aes(x = StepTotal, y = Calories)) +
  geom_point() +
  geom_smooth()

cor(hourly_merged$Calories, hourly_merged$StepTotal)
#Increase of Calories burned by each StepTotal increased



# Steptotal ---------------------------------------------------------------

#TotalIntensity
hourly_merged %>%
  ggplot(aes(x = TotalIntensity, y = StepTotal)) +
  geom_point() +
  geom_smooth()

#AverageIntensity
hourly_merged %>%
  ggplot(aes(x = AverageIntensity, y = StepTotal)) +
  geom_point() +
  geom_smooth()

#Seems like a linear connection that all increases

# Intensities -------------------------------------------------------------




hourly_merged %>%
  ggplot(aes(x = TotalIntensity, y = AverageIntensity)) +
  geom_point() +
  geom_smooth()
# AverageIntensity is TotalIntensity / 60. So its the intensity by minute
