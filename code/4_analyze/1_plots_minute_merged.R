# MERGE -------------------------------------------------------------------

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


# Rename columns from Sleep table for easier readability----------------------

minute_merged <- minute_merged %>%
  rename(SleepValue = value, SleepLogId = logId)


# Linear attributes vs. attributes ----------------------------------------

minute_merged %>%
  ggplot(aes(x = Intensity, y = Calories)) +
  geom_col()

# Finding:

minute_merged %>%
  ggplot(aes(x = Calories, y = METs)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = Calories, y = SleepValue)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = Calories, y = Steps)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = Intensity, y = METs)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = Intensity, y = SleepValue)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = Intensity, y = Steps)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = METs, y = SleepValue)) +
  geom_point()

# Finding:

minute_merged %>%
    ggplot(aes(x = METs, y = Steps)) +
  geom_point()

# Finding:

minute_merged %>%
  ggplot(aes(x = SleepValue, y = Steps)) +
  geom_point()

# Finding:


# Attributes by hour ---------------------------------------------------------

## Prep
#-> Identify and Store Hour of Day
minute_merged$Hour <- format(as.POSIXct(minute_merged$ActivityMinute),
                             format = "%H")

## Calories
minute_merged %>%
  ggplot(aes(x = Hour, y = Calories)) +
  geom_col()

# Finding: Gives me aggregated values of how many calories collected per hour of
# day. As it seems similar to Calories collected in hourly_merged, skipping results



## Intensity level
minute_merged %>%
  ggplot(aes(x = Hour, y = Intensity)) +
  geom_col()

# Finding: Same distribution as in hourly



## METs
minute_merged %>%
  ggplot(aes(x = Hour, y = METs)) +
  geom_col()

# Finding: Looks similar to Calories




## SleepValue
minute_merged %>%
  ggplot(aes(x = Hour, y = SleepValue)) +
  geom_col()

# Finding: People start going to bed at 7pm and start waking up at 4 and 5am.
# Most people sleep at 3am, and least on 5pm.



# Attributes by day -------------------------------------------------------

#-> Identify weekdays
minute_merged$Day <- weekdays(minute_merged$ActivityMinute)

## Calories
#-> Add plot, sorted by days of week (Sunday to Saturday)
minute_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Calories)) +
  geom_col() +
  xlab('Wochentage')

# Get precise values according to graph
minute_merged_calories <- minute_merged %>%
  group_by(Day) %>%
  summarise(Mean = mean(Calories, na.rm = TRUE), Sum = sum(Calories, na.rm = TRUE))

# Finding: Highest point of sum of data collection Tuesday with 3,5k. Lowest
# Sunday with 2,7k.

# Get statistics about mean per day
minute_merged_calories %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  xlab('Wochentage')

# Finding: Very close levels. No difference by day distinguishable.

# Clean up
rm(minute_merged_calories)




## METs
# Get precise values according to graph
minute_merged_METs <- minute_merged %>%
  group_by(Day) %>%
  summarise(Mean = mean(METs, na.rm = TRUE), Sum = sum(METs, na.rm = TRUE))

# Get statistics about mean per day
minute_merged_METs %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  xlab('Wochentage')

# Finding: Very close levels. No difference by day distinguishable.

# Clean up
rm(minute_merged_METs)



## Intensity Value
# Get precise values according to graph
# Finding:

# Clean up
rm(minute_merged_intensity)



## SleepValue
# Get precise values according to graph
# Finding:

# Clean up
rm(minute_merged_sleep)

# Attributes by week(end) ----------------------------------------------------
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

## METs
# Display ggplot differentiating between week & weekend
#-> Check mean of METs by Week
METs_by_week <- minute_merged %>%
  group_by(Week) %>%
  summarise(Mean = mean(Calories), SD = sd(Calories))

METs_by_week %>%
  ggplot(aes(x = Week,
             y = Mean)) +
  geom_col() +
  xlab('Week')

# No significant difference in Calories based on week or weekend

#cleanup
rm(METs_by_week)



# Ordinal graphs: Intensity + SleepValue -------------------------------------

## Prepare
# Get statistics about count and share per Intensity + compute labels positions
ordinal_analysis_intensity <- minute_merged %>%
  filter(!is.na(Intensity)) %>%
  group_by(Intensity) %>%
  summarise(Count = n()) %>%
  mutate(Share = Count / sum(ordinal_analysis_intensity$Count) *100) %>%
  mutate(ypos = cumsum(Share)- 0.5*Share)

## Intensity
#Bar chart
library(scales)
ordinal_analysis_intensity %>%
  ggplot(aes(x = Intensity, y = Count)) +
  geom_col() +
  scale_y_continuous(labels = label_comma())

#Finding: Vast majority of Intensity tracked is 0

#Generate pie chart
ordinal_analysis_intensity %>%
  ggplot(aes(x = "", y = Share, fill = Intensity)) +
  geom_col(color = "black") +
  theme_void() +
  geom_text(aes(label = Share),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")


# Finding: 84% are with Intensity 0, 14% with Intensity 1. Only 2% Intensity
# set to 2 or 3

#Cleanup
rm(ordinal_analysis_intensity)

## SleepValue
# Get statistics about count and share per SleepValue
ordinal_analysis_sleep <- minute_merged %>%
  filter(!is.na(SleepValue)) %>%
  group_by(SleepValue) %>%
  summarise(Count = n()) %>%
  mutate(Share = Count / sum(ordinal_analysis_sleep$Count) *100) %>%
  mutate(ypos = cumsum(Share)- 0.5*Share)

#Bar chart
ordinal_analysis_sleep %>%
  ggplot(aes(x = SleepValue, y = Count)) +
  geom_col() +
  scale_y_continuous(labels = label_comma())

#Finding: Vast majority of Sleep Value tracked is 1

#Generate pie chart
ordinal_analysis_sleep %>%
  ggplot(aes(x = "", y = Share, fill = SleepValue)) +
  geom_col(color = "black") +
  theme_void() +
  geom_text(aes(label = Share),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")


# Finding: 92% of Sleep Value is 1, 7% is Value 2 and only 1% with SleepValue
# of 3


#Cleanup
rm(ordinal_analysis_sleep)
