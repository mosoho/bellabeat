# Linear attributes vs. attributes ----------------------------------------

### Calories
## Compare with Intensity + METs + SleepValue + Steps
# Calories vs. Intensity
calories_aggregated_by_intensity <- subset(minute_merged, !is.na(Intensity)) %>%
  select(Intensity, Calories) %>%
  group_by(Intensity) %>%
  summarise(MedianCalories = median(Calories))

calories_aggregated_by_intensity %>%
  ggplot(aes(x = Intensity,
             y = MedianCalories)) +
  geom_col() +
  ylab("Calories per Minute")

summary(calories_aggregated_by_intensity)

cor(calories_aggregated_by_intensity$Intensity,
     calories_aggregated_by_intensity$MedianCalories)

# Finding: Mean of calories increases by intensity level with correlation of
#          0.9895707

# Clean up
rm(calories_aggregated_by_intensity)


## Calories vs. METs
minute_merged %>%
  ggplot(aes(x = Calories, y = METs)) +
  geom_point()

cor(minute_merged$Calories, minute_merged$METs, use = "complete.obs")
# Finding: METs increases when Calories increase


## Calories vs. SleepValue
minute_merged %>%
  ggplot(aes(x = SleepValue, y = Calories)) +
  geom_count(aes(color = ..n..)) +
  scale_size_continuous(range = c(.5, 10))

cor(minute_merged$Calories, minute_merged$SleepValue, use = "complete.obs")
# Finding: Majority of data points between SleepValue and Calories are
# SleepValue = 1 and Calories up to value of 2
# Low positive to no correlation between Calories & SleepValue (0.16)


## Calories vs. Steps
minute_merged %>%
  ggplot(aes(x = Calories, y = Steps)) +
  geom_point()

cor(minute_merged$Calories, minute_merged$Steps, use = "complete.obs")

# Finding: Positive correlation between Calories & Steps (0.83)

### Intensity
# Summarise Intensity category by
# METs + SleepValue + Steps

## Intensity vs. METs
METs_aggregated_by_intensity <- minute_merged %>%
  select(Intensity, METs) %>%
  group_by(Intensity) %>%
  summarise(MeanMETs = mean(METs, na.rm = TRUE))

METs_aggregated_by_intensity %>%
  ggplot(aes(x = Intensity,
             y = MeanMETs)) +
  geom_col()

summary(METs_aggregated_by_intensity)

cor(minute_merged$Intensity, minute_merged$METs, use = "complete.obs")
# Finding: Positive correlation between Intensity & METs (0.94)
# Clean up
rm(METs_aggregated_by_intensity)



## Intensity vs. SleepValue
subset(minute_merged, !is.na(SleepValue)) %>%
  ggplot(aes(x = Intensity, y = as.character(SleepValue))) +
  geom_count(aes(color = ..n..)) +
  scale_size_continuous(range = c(.5, 10))

cor(minute_merged$Intensity, minute_merged$SleepValue, use = "complete.obs")
#Finding: Majority of data points at Intensity = 0 and SleepValue = 1
# Positive correlation of 0.31.

subset(minute_merged, !is.na(SleepValue)) %>%
  ggplot(aes(x = Intensity, y = as.character(SleepValue))) +
  geom_bar(x = Intensity)





# Get count. Summarized per Intensity, per SleepValue
minute_intensity_vs_sleep <- subset(minute_merged, !is.na(Intensity))
minute_intensity_vs_sleep <- subset(minute_merged, !is.na(SleepValue))
minute_intensity_vs_sleep_count <- minute_intensity_vs_sleep %>%
  group_by(Intensity, SleepValue) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / sum(Count))*100)

#Plot: Stacked Barchart
minute_intensity_vs_sleep_count %>%
  ggplot(aes(x = Intensity,
             y = Count,
             fill = factor(as.character(SleepValue),
                           levels = c("3", "2", "1")))) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="Sleep Value")) +
  ylab("Count of Intensity Level") +
  xlab("Intensity Level") +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous(labels = comma)
# Finding: Most intensity level data collected on Tuesdays, then declining
#          until Monday

#Plot: Filled Barchart
minute_intensity_vs_sleep_count %>%
  ggplot(aes(x = Intensity,
             y = Percent,
             fill = factor(as.character(SleepValue),
                           levels = c("3", "2", "1")))) +
  geom_col() +
  geom_text(size = 4, aes(label = round(Percent, 0)), position = position_stack(vjust = .5)) +
  scale_y_continuous("Percent", labels = function(x) scales::percent(x, scale = 1)) +
  labs(fill = "SleepValue") +
  scale_fill_manual(values=cbPalette) +
  xlab("Intensity Level")
# Finding: Distribution seems stable


# Clean up
rm(minute_intensity_vs_sleep, minute_intensity_vs_sleep_count,
   minute_intensity_vs_sleep_sum)







## Intensity vs. Steps
minute_merged %>%
  ggplot(aes(x = Intensity, y = Steps)) +
  geom_col() +
  scale_y_continuous(name="Steps", labels = comma)

count_steps_by_intensity <- subset(minute_merged, !is.na(Intensity)) %>%
  group_by(Intensity) %>%
  summarise(Median = median(Steps))

# Finding: Most steps collected are on intensity level 1 with 4,5k steps,
# intensity level 2 with 0,8k steps, intensity level 3 with 1,9k steps.

subset(minute_merged, !is.na(Intensity)) %>%
  ggplot(aes(x = factor(Intensity), y = Steps)) +
  geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.45), size=3.5)
# Finding: Intensity level 0 usually contains 0 steps, whereas number of steps
# increases by Intensity level. Medians. Intensity level 1 = 18,
# Intensity level 2 = 63, Intensity level 3 = 105

# Check correlation
cor(count_steps_by_intensity$Intensity,
    count_steps_by_intensity$Median)
# Finding: 0.9857 very high correlation

# Clean up
rm(count_steps_by_intensity)

### METs
# Summarise METs category by
# SleepValue + Steps

subset(minute_merged, !is.na(SleepValue))  %>%
  ggplot(aes(x = factor(SleepValue), y = METs)) +
  geom_col()
# Finding: Vast majority of MET data collected is with SleepValue 1

subset(minute_merged, !is.na(SleepValue))  %>%
  ggplot(aes(x = factor(SleepValue), y = METs)) +
  geom_boxplot() +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.45), size=3.5)
cor(minute_merged$SleepValue, minute_merged$METs, use = "complete.obs")
# Finding: Positive correlation of 0.33. Medians. SleepValue 1 = 10, SleepValue 2 = 10, SleepValue 3 = 11

### SleepValue
# Summarise SleepValue category by Steps

subset(minute_merged, !is.na(SleepValue))  %>%
  ggplot(aes(x = factor(SleepValue), y = Steps)) +
  geom_col()
# Finding: Most steps are collected with SleepValue 2 and 3 (almost 15,000
# total each)

subset(minute_merged, !is.na(SleepValue))  %>%
  ggplot(aes(x = factor(SleepValue), y = Steps)) +
  geom_boxplot() +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.45), size=3.5)
cor(minute_merged$SleepValue, minute_merged$Steps, use = "complete.obs")
# Finding: Positive correlation of 0.32. Steps increase when SleepValue
# increases. For SleepValue 1, 2 and 3: Median of Steps = 0. However
# 75% Percentile of SleepValue 3 with 9 Steps

# Overview: Sleep Value ---------------------------------------------
# Get statistics about count and share per SleepValue
ordinal_analysis_sleep <- minute_merged %>%
  filter(!is.na(SleepValue)) %>%
  group_by(SleepValue) %>%
  summarise(Count = n()) %>%
  mutate(Share = Count / sum(Count) *100) %>%
  mutate(ypos = cumsum(Share)- 0.5*Share)

quantile(minute_merged$SleepValue, na.rm = TRUE)

ncol(t(subset(minute_merged, !is.na(SleepValue))))

mean(minute_merged$SleepValue, na.rm = TRUE)

# Histogram
minute_merged %>%
  ggplot(aes(x = SleepValue)) +
  geom_histogram(binwidth = 1) +
  stat_bin(binwidth = 1, geom="text", aes(label = ..count..),
           vjust = -1)
#Finding: Vast majority of Sleep Value tracked is 1
#         92% (172,480) of Sleep Value is 1, 7% (14,023) is Value 2 and
#         only 1% (2,018) with SleepValue of 3


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



## Aggregate by User ID and compare with daily_merged data set findings about
##sleep
#-> Separate SleepValue into 3 columns
minute_sleepvalue <- subset(minute_merged, !is.na(SleepValue)) %>%
  mutate(SleepValue1 = case_when(SleepValue == 1 ~ 1)) %>%
  mutate(SleepValue2 = case_when(SleepValue == 2 ~ 1)) %>%
  mutate(SleepValue3 = case_when(SleepValue == 3 ~ 1))

#-> Group by User ID & Count SleepValue occurences
minute_sleepvalue_by_user <- minute_sleepvalue %>%
  group_by(Id) %>%
  summarise(SleepValue1Count = sum(SleepValue1, na.rm= TRUE),
            SleepValue2Count = sum(SleepValue2, na.rm= TRUE),
            SleepValue3Count = sum(SleepValue3, na.rm= TRUE))
#-> Calculate individual shares
minute_sleepvalue_by_user <- minute_sleepvalue_by_user %>%
  group_by(Id) %>%
  summarise(SleepValue1Percent = SleepValue1Count / sum(SleepValue1Count+SleepValue2Count+SleepValue3Count),
            SleepValue2Percent = SleepValue2Count / sum(SleepValue1Count+SleepValue2Count+SleepValue3Count),
            SleepValue3Percent = SleepValue3Count / sum(SleepValue1Count+SleepValue2Count+SleepValue3Count))
# Changing Id to character for daily_users_sleep and minute_sleepvalue_by_user
minute_sleepvalue_by_user <- minute_sleepvalue_by_user %>%
  mutate(Id = as.character(Id))
daily_users_sleep_merge <- daily_users_sleep %>%
  mutate(Id = as.character(Id))
# Now combine with daily_users_sleep_merge
daily_users_sleep_value <- daily_users_sleep_merge %>% full_join(minute_sleepvalue_by_user, by = join_by(Id == Id))

#GGplot:
# Display bars with AverageHours, filled in % by SleepValue1, 2 und 3
#-> Reorder function
daily_users_sleep_value <- daily_users_sleep_value %>%
  mutate(Id = fct_reorder(as.character(Id), AverageHours))
#-> Prepare data for GGplot: Create df of area
rect <- data.frame(xmin=-Inf, xmax=Inf, ymin=7, ymax=Inf)
#--> GGPlot, only with users 9+ days of data available
daily_users_sleep_value <- daily_users_sleep_value[daily_users_sleep_value$Count >8, ]
#--> GGplot
daily_users_sleep_value %>%
  ggplot(aes(x = Id, y = AverageHours)) +
  scale_color_manual(values = cbPalette) +
  scale_fill_gradientn(limits = c(0,1),
                       colours=c("black", "grey", "orange")) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#05C4BC",
            color="white",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_bar(stat = "identity", aes(fill = SleepValue1Percent)) +
  theme_minimal() +
  xlab("") +
  ylab("Hours") +
  guides(fill = guide_legend(title = "% of SleepValue = 1", reverse = TRUE)) +
  geom_hline(yintercept = 7, linetype = "dashed", size = 1, color = "#05C4BC") +
  annotate("text", x = 1.2, y = 7.95, label = "Healthy?", size = 9,
           color = "#60bbc3", fontface = "bold") +
  scale_y_continuous(breaks = c(0, 3, 6, 7, 9, 11),
                     labels = c(0, 3, 6, 7, 9, 11)) +
  coord_flip()


#Cleanup
rm(ordinal_analysis_sleep, minute_sleepvalue, minute_sleepvalue_by_user,
   daily_users_sleep_merge, daily_users_sleep_value, rect)

## SleepLogId - SKIPPING
# I could check for patterns, based on Sleep Log ID. But trying to finish project
# so leaving that open.


# Overview: Intensity -----------------------------------------------
# Get statistics about count and share per Intensity + compute labels positions
ordinal_analysis_intensity <- minute_merged %>%
  filter(!is.na(Intensity)) %>%
  group_by(Intensity) %>%
  summarise(Count = n()) %>%
  mutate(Share = Count / sum(Count) *100) %>%
  mutate(ypos = cumsum(Share)- 0.5*Share)

quantile(minute_merged$Intensity, na.rm = TRUE)

ncol(t(subset(minute_merged, !is.na(Intensity))))

minute_merged %>%
  ggplot(aes(x = Intensity)) +
  geom_histogram(binwidth = 1) +
  stat_bin(binwidth = 1, geom="text", aes(label = ..count..),
           vjust = -1)
#Finding: 84% (1,130,091) are with Intensity 0, 14% (183,384) with Intensity 1.
#         Only 2% (33,288) Intensity set to 2 or 3

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


# Overview: Calories ------------------------------------------------------
# Get statistics about count and share per Calories + compute labels positions
quantile(minute_merged$Calories, na.rm = TRUE)

ncol(t(subset(minute_merged, !is.na(Calories))))
# Histogram with labels
minute_merged %>%
  ggplot(aes(x = Calories)) +
  geom_histogram(binwidth = 1.5) +
  stat_bin(binwidth = 1.5, geom="text", aes(label = ..count..),
           vjust = -1) +
  scale_y_continuous(label=comma)
# Boxplot
minute_merged %>%
  ggplot(aes(y = Calories)) +
  geom_boxplot()
# Finding: Median is at 1.22 Calories / Minute and data is skewed to right: Median
#          best measurement for average.

# Overview: Steps ---------------------------------------------------------
# Get statistics about count and share per Steps + compute labels positions
quantile(minute_merged$Steps, na.rm = TRUE)

ncol(t(subset(minute_merged, !is.na(Steps))))
# Histogram with labels
minute_merged %>%
  ggplot(aes(x = Steps)) +
  geom_histogram(binwidth = 15) +
  stat_bin(binwidth = 15, geom="text", aes(label = ..count..),
           vjust = -1) +
  scale_y_continuous(label=comma)
# Boxplot
minute_merged %>%
  ggplot(aes(y = Steps)) +
  geom_boxplot()
# Finding: Data range from 0 to 220. 0% to 75% quantiles with value of 0.
#          Median at 0.

# Compare difference to Steps of daily data set
# This data set: x Steps / Minute = x Steps / Day
((7439 - x) / 7439) * 100
# Finding: yyy % difference

# Overview: METs-------------------
quantile(minute_merged$METs, na.rm = TRUE)

ncol(t(subset(minute_merged, !is.na(METs))))

minute_merged %>%
  ggplot(aes(x = METs)) +
  geom_histogram(binwidth = 10) +
  stat_bin(binwidth = 10, geom="text", aes(label = ..count..),
           vjust = -1) +
  scale_y_continuous(labels = label_comma())
#Finding: 83,90% (1,112,644) of 1,326,123 METs are from 0-9
# Skewed to right


# By Hour: Device Usage ---------------------------------------------------
# Anzahl Tage per Stunde in der Woche. Pro user ID
device_usage_hours <- minute_merged %>%
  group_by(Id, Date, Hour) %>%
  summarise(Count = sum(n())) %>%
  group_by(Id, Date, Hour) %>%
  summarise(Count = sum(n())) %>%
  group_by(Id, Hour) %>%
  summarise(Count = sum(n()))

# Distribution with relative numbers
device_usage_hours_graph <- device_usage_hours %>%
  group_by(Hour) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Overall = sum(Count)) %>%
  group_by(Hour) %>%
  summarise(Percent = Count / Overall) %>%
  mutate(PercentCharacter = scales::percent(Percent))

## GGplot: Horizontal lines
device_usage_hours_graph %>%
  mutate(HourLabel = fct_relevel(Hour,
                            "23", "22", "21", "20", "19", "18", "17", "16",
                            "15", "14", "13", "12", "11", "10", "09", "08",
                            "07", "06", "05", "04", "03", "02", "01", "00")) %>%
  ggplot(aes(x = HourLabel, y = Percent)) +
  geom_segment(aes(xend=HourLabel, yend=0), size = .8) +
  geom_point(size = 4, color = "orange") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()

# Clean Up
rm(device_usage_hours_graph, device_usage_hours)

# By Hour: Calories -------------------------------------------------------
# Get count & percentage. Summarized per SleepValue, per hour
minute_calories_by_hour <- subset(minute_merged, !is.na(Calories)) %>%
  group_by(Hour, Calories) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count)*100)

# Plot: Stacked Barchart: All 3 Values: Total
minute_calories_by_hour %>%
  ggplot(aes(x = Hour, y = Count)) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="Calories")) +
  ylab("Count of Calories") +
  xlab("Day") +
  scale_fill_manual(values = cbPalette)

# Plot: Boxplot Median of Calories/Minute by Hour
minute_merged %>%
  ggplot(aes(x = Hour, y = Calories)) +
  geom_boxplot()


# Average of Calories by Hour
minute_calories_avg <- subset(minute_merged, !is.na(Calories)) %>%
  group_by(Hour) %>%
  summarise(Mean = mean(Calories), Median = median(Calories))

# Plot Median
minute_calories_avg %>%
  ggplot(aes(x = Hour,
             y = Median)) +
  geom_col() +
  labs(y = 'Median of Calories')
# Finding: Median of Calories/Hour. Highest 5pm - 7pm with 1.37540 to 1.40080
#          calories / hour. Lowest 11pm - 6am with 1.08120 to 1.14600 calories / hour


### SUM calories/minute to calories/hour
minute_calories_hourly <- subset(minute_merged, !is.na(Calories)) %>%
  group_by(Id, Date, Hour) %>%
  summarise(CaloriesFromMinute = sum(Calories))
# Finding: After checking with data from hourly_merged: hourly_merged seems to
#          be the rounded version of aggregated minute_merged.

# Merge tables
#-> Create new temporary table with renamed column
hourly_merged_prep <- hourly_merged %>%
  rename(CaloriesFromHourly = Calories)
#-> Merge tables
minute_calories_hourly_merged <- minute_calories_hourly %>%
  full_join(hourly_merged_prep, by = join_by(Id == Id, Date == Date,
                                             Hour == Hour))
#-> Clean up temporary tables
rm(minute_calories_hourly, hourly_merged_prep)
# Remove unwanted columns
minute_calories_hourly_merged <- minute_calories_hourly_merged %>%
  select(c(Id:CaloriesFromMinute,CaloriesFromHourly))
# Add Difference column
minute_calories_hourly_merged <- minute_calories_hourly_merged %>%
  mutate(CaloriesDifference = CaloriesFromHourly - CaloriesFromMinute)
# Finding: User 4702921684 has 10 observations where Calories from minute level
#          data is almost double to hourly level data. Rest of data
#          differentiates between -0.5 and +0.5 differences
# Display all rows with NA
minute_calories_hourly_merged_na <-
  minute_calories_hourly_merged[is.na(minute_calories_hourly_merged$CaloriesDifference),]
#-> 338 NA

# Check overall distribution + median
minute_calories_hourly_merged %>%
  ggplot(aes(x = CaloriesFromMinute)) +
  geom_histogram()
# Strongly skewed to right -> Use median as average

quantile(minute_calories_hourly_merged$CaloriesFromMinute)
# Median is 82.524


# Check AVG of Calories/Hour per Hour
calories_by_hour <- minute_calories_hourly_merged %>%
  group_by(Hour) %>%
  summarise(Median = median(CaloriesFromMinute))

calories_by_hour %>%
  ggplot(aes(x = Hour,
             y = Median)) +
  geom_col() +
  labs(x = 'Hours', y = 'Median of CaloriesFromMinute per Hour')

# Finding: Lowest calories with 65 to 75 between 11pm and 7am. Highest with
#          98 to 103 at 4 to 6pm. So even in sleep users burn half of maximum
#          calories that can be burned.


# Check AVG of Calories/Hour per Day
# Add day of week
minute_calories_hourly_merged$Day <- weekdays(
  minute_calories_hourly_merged$Date)
# Relocate columns
minute_calories_hourly_merged <- minute_calories_hourly_merged %>%
  relocate(Day, .after = Date)

calories_by_day <- minute_calories_hourly_merged %>%
  group_by(Day) %>%
  summarise(Median = median(CaloriesFromMinute))

calories_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Median)) +
  geom_col() +
  labs(x = 'Day', y = 'Median of CaloriesFromMinute per Day')

# Finding: Distribution of Median of Calories/ hour identical for Monday,
#          Wednesday, Thursday, Friday and Saturday with 82.52. Tuesday is
#          highest with 83.53 and Sunday lowest with 79.29

# Clean up
rm(minute_calories_by_hour, minute_calories_avg, minute_calories_hourly_merged,
   minute_calories_hourly_merged_na, calories_by_hour, calories_by_day)



# By Hour: Intensity level ------------------------------------------------
# Total data collected
minute_merged %>%
  ggplot(aes(x = Hour, y = Intensity)) +
  geom_col()
# Finding: People start going to bed at 7pm and start waking up at 4 and 5am.
# Most people sleep at 3am, and least on 5pm.

# Get count & percentage. Summarized per SleepValue, per hour
mmih <- subset(minute_merged, !is.na(Intensity)) %>%
  group_by(Hour, Intensity) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count)*100)

#Plot: Stacked Barchart: All 3 Values: Total
mmih %>%
  ggplot(aes(x = Hour, y = Count, fill = factor(as.character(Intensity),
                                                levels = c("3", "2", "1", "0")))) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="Intensity Level")) +
  ylab("Count of Intensity") +
  xlab("Day") +
  scale_fill_manual(values = cbPalette)

# Graph is going down throughout the day. Why? Group mmih by Hour to see sums
mmih_sum <- mmih %>%
  group_by(Hour) %>%
  summarise(Count = sum(Count))
# Plot: Zooming in
mmih_sum %>%
  ggplot(aes(x = Hour, y = Count)) +
  geom_point()
# Calculate difference by %
(1 - (min(mmih_sum$Count) / max(mmih_sum$Count))) * 100

#Plot: Filled Barchart: All 3 Values: Percent
mmih %>%
  ggplot(aes(x=Hour, y = Percent, fill = factor(as.character(Intensity),
                                                levels = c("3", "2", "1", "0")))) +
  geom_col() +
  geom_text(size = 4, aes(label = round(Percent, 0)), position = position_stack(vjust = .5)) +
  scale_y_continuous("Percent", labels = function(x) scales::percent(x, scale = 1)) +
  labs(fill = "Intensity Level") +
  scale_fill_manual(values=cbPalette)

#Clean up
rm(mmih, mmih_sum)



# By Hour: Steps ----------------------------------------------------------
# Get count & percentage. Summarized per SleepValue, per hour
minute_steps_count <- subset(minute_merged, !is.na(Steps)) %>%
  group_by(Hour, Steps) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count)*100)

# Plot: Stacked Barchart: All 3 Values: Total
minute_steps_count %>%
  ggplot(aes(x = Hour, y = Count)) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="Steps")) +
  ylab("Count of Steps") +
  xlab("Day") +
  scale_fill_manual(values = cbPalette)
# Finding: Data collected declines throughout the day, as with Calories

# Plot: Boxplot Median of Steps/Minute by Hour
minute_merged %>%
  ggplot(aes(x = Hour, y = Steps)) +
  geom_boxplot()


# Average of Steps by Hour
minute_steps_avg <- subset(minute_merged, !is.na(Steps)) %>%
  group_by(Hour) %>%
  summarise(Mean = mean(Steps), Median = median(Steps))

# Plot Median
minute_steps_avg %>%
  ggplot(aes(x = Hour,
             y = Median)) +
  geom_col() +
  labs(y = 'Median of Steps')
# Finding: Median of Steps/Hour. Highest 4pm - 6pm with 98 to 103
#          calories / hour. Lowest 11pm - 6am with 65 to 75 calories / hour


### SUM steps/minute to steps/hour
minute_steps_hourly <- subset(minute_merged, !is.na(Steps)) %>%
  group_by(Id, Date, Hour) %>%
  summarise(StepsFromMinute = sum(Steps))
# Finding: First impression seems to be that tables are identical. However, more
#          throughout check is needed.

# Merge tables
minute_steps_hourly_merged <- minute_steps_hourly %>%
  full_join(hourly_merged, by = join_by(Id == Id, Date == Date, Hour == Hour))
# Remove unwanted columns
minute_steps_hourly_merged <- minute_steps_hourly_merged %>%
  select(c(Id:StepsFromMinute,StepTotal))
# Rename column
minute_steps_hourly_merged <- minute_steps_hourly_merged %>%
  rename(StepsFromHourly = StepTotal)
# Add Difference column
minute_steps_hourly_merged <- minute_steps_hourly_merged %>%
  mutate(StepsDifference = StepsFromHourly - StepsFromMinute)
# Finding: 1 row is different: StepsFromMinute has 8 more steps on 2016-05-07 at
#          hour = 0 of user 4702921684

# Display all rows with NA
minute_steps_hourly_merged_na <-
  minute_steps_hourly_merged[is.na(minute_steps_hourly_merged$StepsDifference),]


# Check overall distribution + median
minute_steps_hourly_merged %>%
  ggplot(aes(x = StepsFromMinute)) +
  geom_histogram(binwidth = 380) +
  stat_bin(binwidth = 380, geom="text", aes(label = ..count..),
           vjust = -1)
# Strongly skewed to right -> Use median as average

quantile(minute_steps_hourly_merged$StepsFromMinute)
# Median is 40

# Clean up
rm(minute_steps_count, minute_steps_avg, minute_steps_hourly,
   minute_steps_hourly_merged, minute_steps_hourly_merged_na)


# By Hour: METs -----------------------------------------------------------
minute_merged %>%
  ggplot(aes(x = Hour, y = METs)) +
  geom_col()

# Finding: Looks similar to Calories




# By Hour: SleepValue -----------------------------------------------------
# Total data collected
minute_merged %>%
  ggplot(aes(x = Hour, y = SleepValue)) +
  geom_col()
# Finding: People start going to bed at 7pm and start waking up at 4 and 5am.
# Most people sleep at 3am, and least on 5pm.

# Get count & percentage. Summarized per SleepValue, per hour
mmsh <- subset(minute_merged, !is.na(SleepValue)) %>%
  group_by(Hour, SleepValue) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count)*100)

#Plot: Stacked Barchart: All 3 Values: Total
mmsh %>%
  ggplot(aes(x = Hour, y = Count, fill = factor(as.character(SleepValue),
                                                levels = c("3", "2", "1")))) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="SleepValue")) +
  ylab("Count of SleepValue") +
  xlab("Day") +
  scale_fill_manual(values = cbPalette)

#Plot: Filled Barchart: All 3 Values: Percent
mmsh %>%
  ggplot(aes(x=Hour, y = Percent, fill = factor(as.character(SleepValue),
                                                levels = c("3", "2", "1")))) +
  geom_col() +
  geom_text(size = 4, aes(label = round(Percent, 0)), position = position_stack(vjust = .5)) +
  scale_y_continuous("Percent", labels = function(x) scales::percent(x, scale = 1)) +
  labs(fill = "Sleep Value") +
  scale_fill_manual(values=cbPalette)


#Stacked Barchart: Focus on SleepValue 3 in Percent
minute_merged_sleepvalue3_hour <- subset(minute_merged, !is.na(SleepValue)) %>%
  filter(SleepValue == 3) %>%
  group_by(SleepValue, Hour) %>%
  summarise(Count = n()) %>%
  mutate(Percentage =  100 * Count / sum(Count)) %>%
  ungroup

#Plot:
minute_merged_sleepvalue3_hour %>%
  ggplot(aes(x = Hour, y = Percentage, fill = as.integer(SleepValue))) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="SleepValue")) +
  ylab("Percentage of SleepValue") +
  xlab("Day")

# Get Mean and Median. Summarized per hour
minute_merged_sleepvalue_hour_m <- subset(minute_merged, !is.na(SleepValue)) %>%
  group_by(Hour) %>%
  summarise(Mean = mean(SleepValue), Median = median(SleepValue))
#Plot
minute_merged_sleepvalue_hour_m %>%
  ggplot(aes(x = Hour, y = Mean)) +
  geom_col()

#Clean up
rm(mmsh, minute_merged_sleepvalue3_hour)

# By Day: Device usage: Count ----------------------------------------------------
device_usage_count <- minute_merged %>%
  group_by(Id, Date) %>%
  summarise(Count = sum(n())) %>%
  group_by(Id, Date) %>%
  summarise(Count = sum(n())) %>%
  group_by(Id) %>%
  summarise(CountDay = sum(Count))
# Proxy for how often device used: Avilable data per User per day
# Ranges from 4 to 33 -> Set in 3-4 groups
#-> 26 - 33: Almost daily
#-> 17 - 25: At least every 2nd day
#-> 0 - 16: Occasionally




device_usage_count <- device_usage_count %>%
  mutate(CountCategory = case_when(
    CountDay == 33 ~ "33 (Daily)",
    CountDay >= 26 ~ "26-32 (Almost daily)",
    CountDay >= 17 ~ "17-25 (At least every 2nd day)",
    CountDay < 17 ~ "<17 (Occasionally)"
  ))
# Count & Identify shares of categories collected
device_usage_count_graph <- device_usage_count %>%
  group_by(CountCategory) %>%
  summarise(Count = sum(CountDay)) %>%
  mutate(Overall = sum(Count)) %>%
  group_by(CountCategory) %>%
  summarise(Percent = Count / Overall) %>%
  mutate(PercentCharacter = scales::percent(Percent))

## GGplot
device_usage_count_graph %>%
  ggplot(aes(x="",
             y=Percent,
             fill=CountCategory)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = .5, size=18, face = "bold")) +
  scale_fill_manual(values = cbPalette) +
  scale_fill_discrete(breaks = c("33 (Daily)", "26-32 (Almost daily)",
                                 "17-25 (At least every 2nd day)",
                                 "<17 (Occasionally)")) +
  geom_text(aes(label = PercentCharacter),
            position = position_stack(vjust = .35))+
  labs(title="Device usage") +
  guides(fill=guide_legend(title="Days with data"))

# Clean Up
rm(device_usage_count_graph, device_usage_count)


# By Day: Device usage per days of week ---------------------------------------
# Anzahl Tage per Stunde in der Woche. Pro user ID
device_usage_days <- minute_merged %>%
  group_by(Id, Date, Day) %>%
  summarise(Count = sum(n())) %>%
  group_by(Id, Date, Day) %>%
  summarise(Count = sum(n())) %>%
  group_by(Id, Day) %>%
  summarise(Count = sum(n()))

# Distribution with relative numbers
device_usage_days_graph <- device_usage_days %>%
  group_by(Day) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Overall = sum(Count)) %>%
  group_by(Day) %>%
  summarise(Percent = Count / Overall) %>%
  mutate(PercentCharacter = scales::percent(round(Percent, digits = 3)))


## GGplot: Horizontal lines
device_usage_days_graph %>%
  mutate(DaySorted = fct_relevel(Day, "Sonntag", "Samstag", "Freitag",
                                 "Donnerstag", "Mittwoch", "Dienstag",
                                 "Montag")) %>%
  ggplot(aes(x = DaySorted, y = Percent, label = PercentCharacter)) +
  geom_segment(aes(xend=DaySorted, yend=0), size = .8) +
  geom_text(nudge_x = .25) +
  geom_point(size = 4, color = "orange") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()

# Clean Up
rm(device_usage_days_graph, device_usage_days)

# By Day: Calories --------------------------------------------------------
# SUM over the week
minute_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Calories)) +
  geom_col() +
  xlab('Wochentage') +
  scale_y_continuous(labels = comma)

# COUNT over the week
minute_calories_by_day <- subset(minute_merged, !is.na(Calories)) %>%
  group_by(Day, Calories) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count)*100)
# Finding: Most calories collected on Tuesday, least on Mondays

# Plot: Stacked Barchart: All 3 Values: Total
minute_calories_by_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)), y = Count)) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="Calories")) +
  ylab("Count of Calories") +
  xlab("Week") +
  scale_fill_manual(values = cbPalette) +
  scale_y_continuous(labels = comma)
# Finding: Most calories are burned Tuesday, then declining until Sunday

# Get precise values according to graph
minute_merged_calories <- minute_merged %>%
  group_by(Day) %>%
  summarise(Mean = mean(Calories, na.rm = TRUE),
            Median = median(Calories, na.rm = TRUE),
            Sum = sum(Calories, na.rm = TRUE))

# Finding: Highest point of sum of data collection Tuesday with 3,5k. Lowest
# Sunday with 2,7k.

# Get statistics about median per day
minute_merged_calories %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Median)) +
  geom_col() +
  xlab('Wochentage') +
  ylab('Calories per minute')

# Finding: Distribution of Median of Calories/ minute does almost not differ
#          by Day of Week with Median of 1.2141 to 1.2232


# Plot: Boxplot Median of Calories/Minute by Day
minute_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)), y = Calories)) +
  geom_boxplot()


### SUM calories/minute to calories/day
minute_calories_daily <- subset(minute_merged, !is.na(Calories)) %>%
  group_by(Id, Date, Day) %>%
  summarise(CaloriesFromMinute = sum(Calories))
# Finding: After checking with data from hourly_merged: hourly_merged seems to
#          be the rounded version of aggregated minute_merged.

# Merge tables
#-> Create new temporary table with renamed column
daily_merged_prep <- daily_merged %>%
  rename(CaloriesFromDaily = Calories)
#-> Merge tables
minute_calories_daily_merged <- minute_calories_daily %>%
  full_join(daily_merged_prep, by = join_by(Id == Id, Date == Date, Day == Day))
#-> Clean up temporary tables
rm(minute_calories_daily, daily_merged_prep)
# Remove unwanted columns
minute_calories_daily_merged <- minute_calories_daily_merged %>%
  select(c(Id:CaloriesFromMinute,CaloriesFromDaily))
# Add Difference column
minute_calories_daily_merged <- minute_calories_daily_merged %>%
  mutate(CaloriesDifference = CaloriesFromDaily - CaloriesFromMinute)
# Finding: User 4702921684 has 10 observations where Calories from minute level
#          data is almost double to hourly level data. Rest of data
#          differentiates between -0.5 and +0.5 differences

# SUM absolute difference
minute_calories_daily_merged_abs <- subset(minute_calories_daily_merged,
                                           !is.na(CaloriesDifference))%>%
  mutate(DifferenceAbsolute = abs(CaloriesDifference))
sum(minute_calories_daily_merged_abs$DifferenceAbsolute)
nrow(minute_calories_daily_merged_abs)
#-> 34,509.45 overall difference at 939 rows

# Display all rows with NA
minute_calories_daily_merged_na <-
  minute_calories_daily_merged[is.na(
    minute_calories_daily_merged$CaloriesDifference),]
#-> 338 NA


# Check overall distribution + median
minute_calories_daily_merged %>%
  ggplot(aes(x = CaloriesFromMinute)) +
  geom_histogram(binwidth = 250) +
  stat_bin(binwidth = 250, geom="text", aes(label = ..count..),
           vjust = -1)
# Strongly skewed to right -> Use median as average

quantile(minute_calories_daily_merged$CaloriesFromMinute, na.rm = TRUE)
mean(minute_calories_daily_merged$CaloriesFromMinute, na.rm = TRUE)
# Median is 2.124,28



# Check AVG of Calories per Day of Week
calories_by_day <- subset(minute_calories_daily_merged,
                          !is.na(CaloriesFromMinute)) %>%
  group_by(Day) %>%
  summarise(Median = median(CaloriesFromMinute))

calories_by_day %>%
  ggplot(aes(x = Day,
             y = Median)) +
  geom_col() +
  labs(x = 'Day', y = 'Median of CaloriesFromMinute per Day')

# Finding: Most calories are being burned on Tuesday with median 2,211.122
#         and fewest on Sunday with 2,063.231


# Clean up
rm(minute_calories_by_day, minute_merged_calories, minute_calories_daily,
   minute_calories_daily_merged_abs, minute_calories_daily_merged,
   minute_calories_daily_merged_na, calories_by_day)



# By Day: Steps -----------------------------------------------------------

### SUM steps/minute to steps/day
minute_steps_daily <- subset(minute_merged, !is.na(Steps)) %>%
  group_by(Id, Date, Day) %>%
  summarise(StepsFromMinute = sum(Steps))
# Finding: Minute level data differs from day level data.

# Merge tables
#-> Create new temporary table with renamed column
daily_merged_prep <- daily_merged %>%
  rename(StepsFromDaily = TotalSteps)
#-> Merge tables
minute_steps_daily_merged <- minute_steps_daily %>%
  full_join(daily_merged_prep, by = join_by(Id == Id, Date == Date, Day == Day))
#-> Clean up temporary tables
rm(minute_steps_daily, daily_merged_prep)
# Remove unwanted columns
minute_steps_daily_merged <- minute_steps_daily_merged %>%
  select(c(Id:StepsFromMinute,StepsFromDaily))
# Add Difference column
minute_steps_daily_merged <- minute_steps_daily_merged %>%
  mutate(StepsDifference = StepsFromDaily - StepsFromMinute)
# Finding: User 4702921684 has 10 observations where Calories from minute level
#          data is almost double to hourly level data. Rest of data
#          differentiates between -0.5 and +0.5 differences

# SUM absolute difference
minute_steps_daily_merged_abs <- subset(minute_steps_daily_merged,
                                           !is.na(StepsDifference))%>%
  mutate(DifferenceAbsolute = abs(StepsDifference))
sum(minute_steps_daily_merged_abs$DifferenceAbsolute)
nrow(minute_steps_daily_merged_abs)
#-> 185.195 overall difference at 939 rows

# Display all rows with NA
minute_calories_hourly_merged_na <-
  minute_steps_daily_merged[is.na(minute_steps_daily_merged$StepsDifference),]
#-> 20 NA

# Check overall distribution + median
minute_steps_daily_merged %>%
  ggplot(aes(x = StepsFromMinute)) +
  geom_histogram(binwidth = 1000) +
  stat_bin(binwidth = 1000, geom="text", aes(label = ..count..),
           vjust = -1)
# Skewed to right? -> Use median as average

quantile(minute_steps_daily_merged$StepsFromMinute, na.rm = TRUE)
mean(minute_steps_daily_merged$StepsFromMinute, na.rm = TRUE)
# Median is 7.359, Mean is 7.679,024


# Check AVG of Steps per Day of Week
steps_by_day <- subset(minute_steps_daily_merged,
                          !is.na(StepsFromMinute)) %>%
  group_by(Day) %>%
  summarise(Median = median(StepsFromMinute))

steps_by_day %>%
  ggplot(aes(x = factor(Day,
                        weekdays(as.Date('1970-01-03') + 1:7)),
             y = Median)) +
  geom_col() +
  labs(x = 'Day', y = 'Median of StepsFromMinute per Day')

# Finding: Most calories are being burned on Tuesday with median 2,211.122
#         and fewest on Sunday with 2,063.231


# Clean up
rm(minute_steps_daily, minute_calories_hourly_merged_na,
   minute_steps_daily_merged_abs)


# By Day: METs ------------------------------------------------------------
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


# By Day: Intensity Value -------------------------------------------------
# Get count. Summarized per Intensity, per day
mmid <- subset(minute_merged, !is.na(Intensity)) %>%
  group_by(Day, Intensity) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / sum(Count))*100)

#Plot: Stacked Barchart
mmid %>%
  ggplot(aes(x = factor(Day,
                        weekdays(as.Date('1970-01-03') + 1:7)),
             y = Count,
             fill = factor(as.character(Intensity),
             levels = c("3", "2", "1", "0")))) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="Intensity Level")) +
  ylab("Count of Intensity Level") +
  xlab("Day") +
  scale_fill_manual(values=cbPalette)
# Finding: Most intensity level data collected on Tuesdays, then declining
#          until Monday

# See % difference from min to max
mmid_sum <- mmid %>%
  group_by(Day) %>%
  summarise(Sum = sum(Count))
# Calculation
((min(mmid_sum$Sum) / max(mmid_sum$Sum))*100 - 100)*-1
# Finding: 20.53% more Intensity level data collected on Tuesdays than on
#          Mondays
#Plot: Filled Barchart
mmid %>%
  ggplot(aes(x=factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Percent,
             fill = factor(as.character(Intensity),
                           levels = c("3", "2", "1", "0")))) +
  geom_col() +
  geom_text(size = 4, aes(label = round(Percent, 0)), position = position_stack(vjust = .5)) +
  scale_y_continuous("Percent", labels = function(x) scales::percent(x, scale = 1)) +
  labs(fill = "Intensity Level") +
  scale_fill_manual(values=cbPalette) +
  xlab("Week")
# Finding: Distribution seems stable

# Clean up
rm(mmid, mmid_sum)


# By Day: SleepValue ------------------------------------------------------
# Get count. Summarized per SleepValue, per day
minute_merged_sleepvalue_day <- subset(minute_merged, !is.na(SleepValue)) %>%
  group_by(SleepValue, Day) %>%
  summarise(Count = n())

#Plot: Stacked Barchart
minute_merged_sleepvalue_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)), y = Count, fill = as.integer(SleepValue))) +
  geom_bar(position="stack", stat="identity") +
  guides(fill=guide_legend(title="SleepValue")) +
  ylab("Count of SleepValue") +
  xlab("Day")
# Finding: Vast majority of Sleep level data collected for Sleep level 1.
# Highest day is Wednesday

#Plot: Filled Barchart
minute_merged_sleepvalue_day %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)), y = Count, fill = as.integer(SleepValue))) +
  geom_bar(position="fill", stat="identity") +
  guides(fill=guide_legend(title="SleepValue")) +
  ylab("Count of SleepValue") +
  xlab("Day")
# Finding: Distribution seems stable

# Get statistics. Summarized per day
minute_merged_sleep <- minute_merged %>%
  group_by(Day) %>%
  summarise(Count = n(),
            Sum = sum(SleepValue, na.rm = TRUE),
            Mean = mean(SleepValue, na.rm = TRUE),
            Median = median(SleepValue, na.rm = TRUE))

#Plot: Mean
minute_merged_sleep %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Mean)) +
  geom_col() +
  xlab('Wochentage') +
  ylab('Sleep Value')
# Finding: Very close levels. Almost no difference by day distinguishable.

#Plot: Median
minute_merged_sleep %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Median)) +
  geom_col() +
  xlab('Wochentage') +
  ylab('Sleep Value')
# Finding: Identical. No difference by day distinguishable.


# Clean up
rm(minute_merged_sleep)

# By Week(end): METs ------------------------------------------------------
# Display ggplot differentiating between week & weekend
#-> Check mean of METs by Week
METs_by_week <- subset(minute_merged, !is.na(Calories)) %>%
  group_by(Week) %>%
  summarise(Mean = mean(Calories), SD = sd(Calories))
#-> Plot
METs_by_week %>%
  ggplot(aes(x = Week,
             y = Mean)) +
  geom_col() +
  xlab('Week')
# Finding: No significant difference in METs based on week or weekend

#cleanup
rm(METs_by_week)


# By Week(end): Intensity Level -------------------------------------------
#Create new df summarised & grouped by Week(end)
mmiw <- subset(minute_merged, !is.na(Intensity)) %>%
  group_by(Week, Intensity) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# Create filled graph to show distribution
mmiw %>%
  ggplot(aes(x=Week, y = Percent, fill = factor(as.character(Intensity),
                           levels = c("3", "2", "1", "0")))) +
  geom_col() +
  geom_text(size = 4, aes(label = round(Percent, 0)), position = position_stack(vjust = .5)) +
  scale_y_continuous("Percent", labels = function(x) scales::percent(x, scale = 1)) +
  labs(fill = "Intensity Level") +
  scale_fill_manual(values=cbPalette) +
  xlab("Week vs. Weekend")
# Finding: No difference
#cleanup
rm(mmiw)


# By Week(end): Sleep level -----------------------------------------------
# Display ggplot differentiating between week & weekend
#-> Check mean of Sleep Level by Week
Sleep_by_week <- subset(minute_merged, !is.na(SleepValue)) %>%
  group_by(Week) %>%
  summarise(Mean = mean(SleepValue), SD = sd(SleepValue))
#-> Plot
Sleep_by_week %>%
  ggplot(aes(x = Week,
             y = Mean)) +
  geom_col() +
  xlab('Week')
# Finding: No significant difference in Sleep based on week or weekend

#cleanup
rm(Sleep_by_week)



# By Week(end): Calories --------------------------------------------------
# Get precise calories/minute Averages
minute_merged_calories <- minute_merged %>%
  group_by(Week) %>%
  summarise(Mean = mean(Calories, na.rm = TRUE),
            Median = median(Calories, na.rm = TRUE),
            Sum = sum(Calories, na.rm = TRUE))

# Get statistics about median per day
minute_merged_calories %>%
  ggplot(aes(x = Week,
             y = Median)) +
  geom_col() +
  xlab('Wochentage') +
  ylab('Calories per minute')
# Finding: No difference between week & weekend

# Plot: Boxplot Median of Calories/Minute by Week(end)
minute_merged %>%
  ggplot(aes(x = Week, y = Calories)) +
  geom_boxplot()
# Finding: No difference between week & weekend

# Average of Calories by Day
minute_calories_day_avg <- hourly_merged %>%
  group_by(Week) %>%
  summarise(Mean = mean(Calories), Median = median(Calories))

# Plot Median
minute_calories_day_avg %>%
  ggplot(aes(x = Week,
             y = Median)) +
  geom_col() +
  labs(y = 'Median of Calories')
# Finding: No difference between week & weekend

# Clean up
rm(minute_calories_day_avg, minute_merged_calories)

# By Week(end): Steps -----------------------------------------------------
# Add Days to group "weekend" or "week"
minute_steps_daily_merged$Week <- case_when(
  minute_steps_daily_merged$Day == "Samstag" ~ "Weekend",
  minute_steps_daily_merged$Day == "Sonntag" ~ "Weekend",
  minute_steps_daily_merged$Day == "Montag" ~ "Week",
  minute_steps_daily_merged$Day == "Dienstag" ~ "Week",
  minute_steps_daily_merged$Day == "Mittwoch" ~ "Week",
  minute_steps_daily_merged$Day == "Donnerstag" ~ "Week",
  minute_steps_daily_merged$Day == "Freitag" ~ "Week",
)
minute_steps_daily_merged <- minute_steps_daily_merged %>%
  relocate(Week, .after = Day)

#-> GGPlot: Week vs. Weekend
minute_steps_daily_merged %>%
  ggplot(aes(x = Week,
             y = StepsFromMinute)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Median of StepsFromMinute per Day')
#-> Users take on average 7,691 steps during week and 6,724 steps on weekends

rm(minute_steps_daily_merged)

