# by Hour of Day: TotalIntensity ----------------------------------------------------------


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
  summarise(Mean = mean(TotalIntensity), SD = sd(TotalIntensity), Median = median(TotalIntensity))

intensity_by_hour %>%
  ggplot(aes(x = Hour,
             y = Mean)) +
  geom_col() +
  labs(x = "TotalIntensity by Hour")

intensity_by_hour %>%
  ggplot(aes(x = Hour,
             y = Median)) +
  geom_col() +
  labs(x = "TotalIntensity by Hour")


# MEAN
# Highest intensity collected daily is reached between 5 and 7pm with means
# 21 - 22. Lowest from 12pm to 4am with means 0.5 - 2
# MEDIAN
# Highest intensity collected daily is reached between 5 and 6pm with medians
# 16. Lowest from 11pm to 6am with median 0.

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

#Check AVG of Calories by Hour
calories_by_hour <- hourly_merged %>%
  group_by(Hour) %>%
  summarise(Mean = mean(Calories), SD = sd(Calories), Median = median(Calories))

calories_by_hour %>%
  ggplot(aes(x = Hour,
             y = Median)) +
  geom_col() +
  labs(x = 'Hours', y = 'Median of Calories')

# Finding: Lowest calories with 65 to 75 between 11pm and 7am. Highest with
#          98 to 103 at 4 to 6pm. So even in sleep users burn half of maximum
#          calories that can be burned.

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

#Check stats of StepTotal by Hour
steptotal_by_hour <- hourly_merged %>%
  group_by(Hour) %>%
  summarise(Mean = mean(StepTotal), SD = sd(StepTotal), Median = median(StepTotal))

steptotal_by_hour %>%
  ggplot(aes(x = Hour,
             y = Mean)) +
  geom_col() +
  labs(y = 'Mean of StepTotal')

# Highest StepTotal collected daily is reached between 12 and 7pm with
# 538 to 599, however with a dip at 3 and 4pm up to 406
# Lowest from 12pm to 4am with 6.4 - 43.8

steptotal_by_hour %>%
  ggplot(aes(x = Hour,
             y = Median)) +
  geom_col() +
  labs(y = 'Median of StepTotal')

# First local maxima of StepTotal collected daily is reached between 12 and 1pm
# with X, second local and global maxima is between 4-7pm with X. And median of
# 0 StepTotal from 11pm to 6am.


#cleanup
rm(steptotal_by_hour)



# by Day & Week(end): TotalIntensity --------------------------------------------------


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
  summarise(Mean = mean(TotalIntensity), SD = sd(TotalIntensity), Median = median(TotalIntensity))

intensity_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  labs(x = 'Week', y = 'Mean of TotalIntensity')

intensity_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Median)) +
  geom_col() +
  labs(x = 'Week', y = 'Median of TotalIntensity')

# In general similar levels of high intensity. Pattern of increasing levels
# from Sunday to Tuesday, then drop on Wednesday, followed by increase until
# Saturday. Then drop to Sunday.
# Highest Saturday 12.9, lowest Sunday 10.9

#cleanup
rm(intensity_by_day)

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

## Check median of TotalIntensity, aggregated on day level
hourly_total_intensity <- hourly_merged %>%
  group_by(Id, Date) %>%
  summarise(IntensitySum = sum(TotalIntensity))
#-> Median
median(hourly_total_intensity$IntensitySum)
#-> 300

# Clean Up
rm(hourly_total_intensity)
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

# Generate boxplot per Day of Week
hourly_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Calories)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.5), size=2.5) +
  geom_jitter(color="red", size = 1, alpha = 0.1, width = 0.3) +
  xlab('Weekdays')

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
  summarise(Sum = sum(StepTotal), Mean = mean(StepTotal), SD = sd(StepTotal),
            Median = median(StepTotal))

# Overall steps collected highest on Tuesday (1,204k) and lowest on
# Sunday (835k)

steps_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  ylab('Mean of TotalSteps') +
  xlab('Week')
# MEAN: Saturday highest with 344 and Sunday lowest with 288

steps_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Median)) +
  geom_col() +
  ylab('Median of TotalSteps') +
  xlab('Week')
# MEDIAN: Sunday lowest, Friday highest

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

# Calories vs... ---------------------------------------------------------

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



# Steptotal vs... ----------------------------------------------------------

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

# Intensities vs... --------------------------------------------------------

## Check how it behaves against Average Intensity
hourly_merged %>%
  ggplot(aes(x = TotalIntensity, y = AverageIntensity)) +
  geom_point() +
  geom_smooth()
# AverageIntensity is TotalIntensity / 60. So its the intensity by minute

## Analyse internal value distributions
hourly_merged %>%
  ggplot(aes(y = TotalIntensity)) +
  geom_boxplot(outlier.alpha = 0.1)

quantile(hourly_merged$TotalIntensity, na.rm = TRUE)

ncol(t(subset(hourly_merged, !is.na(TotalIntensity))))
#Finding: 16,574.25 of 22,099 TotalIntensity collected (75%) ranged from 0 to 16

hourly_merged %>%
  ggplot(aes(x = TotalIntensity)) +
  geom_histogram(binwidth = 20) +
  stat_bin(binwidth = 20, geom="text", aes(label = ..count..),
           vjust = -1)
#Finding: 21,097 out of 22,099 TotalIntensity collected (95,47%) ranged from 0 to 50

# Calories: Singular analysis ---------------------------------------------

hourly_merged %>%
  ggplot(aes(x = Calories)) +
  geom_histogram()
# Strongly skewed to right -> Use median as average

quantile(hourly_merged$Calories)

# StepTotal: Singular analysis --------------------------------------------

hourly_merged %>%
  ggplot(aes(x = StepTotal)) +
  geom_histogram()
# Strongly skewed to right -> Use median as average

## Analyse internal value distributions
hourly_merged %>%
  ggplot(aes(y = StepTotal)) +
  geom_boxplot(outlier.alpha = 0.1)

quantile(hourly_merged$StepTotal, na.rm = TRUE)

ncol(t(subset(hourly_merged, !is.na(StepTotal))))

