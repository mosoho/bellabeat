#### PLOTS for identifying trends + MERGING tables

# MERGING tables --------
# daily_activity + sleep_day = daily_activity_and_sleep

daily_activity_and_sleep <-
  daily_activity %>%
  full_join(sleep_day, by = c('Id'='Id', 'ActivityDate'='SleepDay'))

# Renaming columns for readability
daily_activity_and_sleep <-
  daily_activity_and_sleep %>%
  rename(Day = SleepDay_Day) %>%
  rename(Weekday = SleepDay_Group)

# Filling days + weekdays of daily_activity rows for newly added rows
#-> days
daily_activity_and_sleep$Day <- weekdays(daily_activity_and_sleep$ActivityDate)
#-> weekdays
# Add Days to group "weekend" or "week"
daily_activity_and_sleep$Weekday <- case_when(
  daily_activity_and_sleep$Day == "Samstag" ~ "Weekend",
  daily_activity_and_sleep$Day == "Sonntag" ~ "Weekend",
  daily_activity_and_sleep$Day == "Montag" ~ "Week",
  daily_activity_and_sleep$Day == "Dienstag" ~ "Week",
  daily_activity_and_sleep$Day == "Mittwoch" ~ "Week",
  daily_activity_and_sleep$Day == "Donnerstag" ~ "Week",
  daily_activity_and_sleep$Day == "Freitag" ~ "Week",
)

#-> Translate to English
daily_activity_and_sleep$Day <- case_when(
  daily_activity_and_sleep$Day == "Samstag" ~ "Saturday",
  daily_activity_and_sleep$Day == "Sonntag" ~ "Sunday",
  daily_activity_and_sleep$Day == "Montag" ~ "Monday",
  daily_activity_and_sleep$Day == "Dienstag" ~ "Tuesday",
  daily_activity_and_sleep$Day == "Mittwoch" ~ "Wednesday",
  daily_activity_and_sleep$Day == "Donnerstag" ~ "Thursday",
  daily_activity_and_sleep$Day == "Freitag" ~ "Friday",
)

# Rearranging table for readability
daily_activity_and_sleep <- daily_activity_and_sleep %>%
  relocate(Day, .after = ActivityDate)

daily_activity_and_sleep <- daily_activity_and_sleep %>%
  relocate(Weekday, .after = Day)



# TotalSteps vs. Total Distance -------------------------------------------


daily_activity_and_sleep %>%
  ggplot(aes(x = TotalSteps,
             y = TotalDistance)) +
  geom_point(alpha = 0.3)
#--> Filter everything thats above a certain ratio
daily_activity_and_sleep$RatioStepsVsDistance <-
  daily_activity_and_sleep$TotalDistance /
  daily_activity_and_sleep$TotalSteps
#--> Arrange after TotalDistance
daily_activity_and_sleep <- daily_activity_and_sleep %>%
  relocate(RatioStepsVsDistance, .after = TotalDistance)
#--> Sort by ratio in descending order
daily_activity_and_sleep <- daily_activity_and_sleep %>%
  arrange(desc(RatioStepsVsDistance))
#--> Show stat. summary to see where to set cut-off
summary(daily_activity_and_sleep$RatioStepsVsDistance)
#--> Filter by > 0.00082
RatioStepsVsDistance_temp <- daily_activity_and_sleep %>%
  filter(RatioStepsVsDistance>0.00082)
# Display highlight on ggplot to check if all outliers are identified
daily_activity_and_sleep %>%
  ggplot(aes(x = TotalSteps, y = TotalDistance)) +
  geom_point(alpha=0.3) +
  geom_point(data = RatioStepsVsDistance_temp,
             aes(x = TotalSteps, y = TotalDistance),
             color = 'red', alpha = 0.6) +
  geom_quantile(color = "black")
  #geom_abline(intercept = 0, slope = 0.00082, linetype = "dotted", color = "red")
#-> How many entries are affected?
nrow(RatioStepsVsDistance_temp) # 20 entries
#-> How many people?
nrow(RatioStepsVsDistance_temp %>%
       distinct(Id)) # 5 people
#-> Out of...
nrow(distinct(daily_activity_and_sleep, Id)) # 33 people
#-> = 15,15%
# Clean up
rm(RatioStepsVsDistance_temp)



# TotalSteps vs. TrackerDistance ------------------------------------------


daily_activity_and_sleep %>%
  ggplot(aes(x = TrackerDistance,
             y = TotalSteps)) +
  geom_point(alpha = 0.5)
#-> Same as TotalSteps vs. Total Distance





# TotalDistance vs. TrackerDistance ---------------------------------------


daily_activity_and_sleep %>%
  ggplot(aes(x = TrackerDistance,
             y = TotalDistance)) +
  geom_point(alpha = 0.5)

cor(daily_activity_and_sleep$TrackerDistance, daily_activity_and_sleep$TotalDistance)
#-> Assuming TrackerDistance means, how much distance the tracker measures,
# and TotalDistance measures how much distance they have gone.
# Should be correlation of 1, but there are some outliers
#-> Identify outliers by implementing ratio
#--> Filter everything thats above a certain ratio
daily_activity_and_sleep$RatioTrackerVsTotal <-
  daily_activity_and_sleep$TotalDistance /
  daily_activity_and_sleep$TrackerDistance
#--> Arrange after TrackerDistance
daily_activity_and_sleep <- daily_activity_and_sleep %>%
  relocate(RatioTrackerVsTotal, .after = TrackerDistance)
#--> Sort by ratio in descending order
daily_activity_and_sleep <- daily_activity_and_sleep %>%
  arrange(desc(RatioTrackerVsTotal))
#--> Filter by > 1
RatioTrackerVsTotal_temp <- daily_activity_and_sleep %>%
  filter(RatioTrackerVsTotal>1)

# Display highlight on ggplot to check if all outliers are identified
daily_activity_and_sleep %>%
  ggplot(aes(x = TrackerDistance, y = TotalDistance)) +
  geom_point(alpha=0.3) +
  geom_point(data = RatioTrackerVsTotal_temp,
             aes(x = TrackerDistance, y = TotalDistance),
             color = 'red')
#-> How many entries are affected?
nrow(RatioTrackerVsTotal_temp) # 15 entries
#-> How many people?
nrow(RatioTrackerVsTotal_temp %>%
       distinct(Id)) # 2 people
#-> Out of...
nrow(distinct(daily_activity_and_sleep, Id)) # 33 people
#-> = 6%
# Clean up
rm(RatioTrackerVsTotal_temp)



# Day vs. TotalSteps ------------------------------------------------------
#-> Arrange plot to sort by days of week (Sunday to Saturday)
#--> First translate back to German
daily_activity_and_sleep$Day <- case_when(
  daily_activity_and_sleep$Day == "Saturday" ~ "Samstag",
  daily_activity_and_sleep$Day == "Sunday" ~ "Sonntag",
  daily_activity_and_sleep$Day == "Monday" ~ "Montag",
  daily_activity_and_sleep$Day == "Tuesday" ~ "Dienstag",
  daily_activity_and_sleep$Day == "Wednesday" ~ "Mittwoch",
  daily_activity_and_sleep$Day == "Thursday" ~ "Donnerstag",
  daily_activity_and_sleep$Day == "Friday" ~ "Freitag",
)
#-> GGPlot: Every day
daily_activity_and_sleep %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalSteps)) +
  geom_boxplot(outlier.shape = NA) +
  # stat_summary(geom="text",fun.y=quantile,
  #             aes(label=sprintf("%1.1f", ..y..)),
  #             position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Weekdays')
#-> GGPlot: Week vs. Weekend
daily_activity_and_sleep %>%
  ggplot(aes(x = Weekday,
             y = TotalSteps)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Total Steps')
#-> There doesn't seem to be a difference between Total Steps taken on Weekend
#   days vs. Weekdays



# Day vs. Total Distance --------------------------------------------------
#-> GGPlot: Every day
daily_activity_and_sleep %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalDistance)) +
  geom_boxplot() +
  xlab('Weekdays')
#-> GGPlot: Week vs. Weekend
daily_activity_and_sleep %>%
  ggplot(aes(x = Weekday,
             y = TotalDistance)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Total Distance')
#-> There doesn't seem to be a difference between Total Distance taken on Weekend
#   days vs. Weekdays


# Distance ----------------------------------------------------------------
summary(daily_activity_and_sleep[c("VeryActiveDistance",
                                   "ModeratelyActiveDistance",
                                   "LightActiveDistance",
                                   "SedentaryActiveDistance")])


# Overall share of distance activity distribution
#1. SUM each column
VeryActiveDistance_sum <- sum(daily_activity_and_sleep$VeryActiveDistance)
ModeratelyActiveDistance_sum <- sum(daily_activity_and_sleep$ModeratelyActiveDistance)
LightActiveDistance_sum <- sum(daily_activity_and_sleep$LightActiveDistance)
SedentaryActiveDistance_sum <- sum(daily_activity_and_sleep$SedentaryActiveDistance)

distance_sums <- data.frame(
  ActiveDistance = c("Very", "Moderately", "Light", "Sedentary"),
  Value = c(VeryActiveDistance_sum, ModeratelyActiveDistance_sum,
            LightActiveDistance_sum, SedentaryActiveDistance_sum)
)
#clean up
rm(VeryActiveDistance_sum, ModeratelyActiveDistance_sum, SedentaryActiveDistance_sum, LightActiveDistance_sum)
#2. Generate pie chart
#-> Compute labels positions
distance_sums <- distance_sums %>%
  arrange(desc(ActiveDistance)) %>%
  mutate(prop = Value / sum(distance_sums$Value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
#-> Generate ggplot2
distance_sums %>%
  ggplot(aes(x = "", y = Value, fill = ActiveDistance)) +
  geom_col(color = "black") +
  theme_void() +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
#clean up
rm(distance_sums)
# Group by ID and then show distribution of average activity
# ToDo


# Minutes -----------------------------------------------------------------
summary(daily_activity_and_sleep[c("VeryActiveMinutes", "FairlyActiveMinutes",
                                   "LightlyActiveMinutes", "SedentaryMinutes")])


# Overall share of distance activity distribution
#1. SUM each column
VeryActiveMinutes_sum <- sum(daily_activity_and_sleep$VeryActiveMinutes)
FairlyActiveMinutes_sum <- sum(daily_activity_and_sleep$FairlyActiveMinutes)
LightlyActiveMinutes_sum <- sum(daily_activity_and_sleep$LightlyActiveMinutes)
SedentaryMinutes_sum <- sum(daily_activity_and_sleep$SedentaryMinutes)

minutes_sums <- data.frame(
  ActiveMinutes = c("Very", "Fairly", "Lightly", "Sedentary"),
  Value = c(VeryActiveMinutes_sum, FairlyActiveMinutes_sum,
            LightlyActiveMinutes_sum, SedentaryMinutes_sum)
)
#clean up
rm(VeryActiveMinutes_sum, FairlyActiveMinutes_sum, LightlyActiveMinutes_sum, SedentaryMinutes_sum)

#2. Generate pie chart
#-> Compute labels positions
minutes_sums <- minutes_sums %>%
  arrange(desc(ActiveMinutes)) %>%
  mutate(prop = Value / sum(minutes_sums$Value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
#-> Generate ggplot2
minutes_sums %>%
  ggplot(aes(x = "", y = Value, fill = ActiveMinutes)) +
  geom_col(color = "black") +
  theme_void() +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

# Group by ID and then show distribution of average activity
# ToDo

#clean up
rm(minutes_sums)


# SedentaryMinutes vs. TotalTimeInBed -------------------------------------

daily_activity_and_sleep %>%
  ggplot(aes(x = SedentaryMinutes,
             y = TotalTimeInBed)) +
  geom_point(alpha = 0.5) +
  geom_smooth()

cor(daily_activity_and_sleep$SedentaryMinutes, daily_activity_and_sleep$TotalTimeInBed, use="complete.obs")


# Calories ----------------------------------------------------------------

summary(daily_activity_and_sleep$Calories)

daily_activity_and_sleep %>%
  ggplot(aes(x = "", y = Calories)) +
  geom_boxplot()

#-> GGPlot: Every day
daily_activity_and_sleep %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Calories)) +
  geom_boxplot(outlier.shape = NA) +
  # stat_summary(geom="text",fun.y=quantile,
  #             aes(label=sprintf("%1.1f", ..y..)),
  #             position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Weekdays')
#-> GGPlot: Week vs. Weekend
daily_activity_and_sleep %>%
  ggplot(aes(x = Weekday,
             y = Calories)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Total Steps')


# Calories vs. TotalMinutesAsleep ---------------------------------------------

daily_activity_and_sleep %>%
  ggplot(aes(x = TotalMinutesAsleep, y = Calories)) +
  geom_point(alpha = 0.5)

cor(daily_activity_and_sleep$TotalMinutesAsleep,
    daily_activity_and_sleep$Calories, use = "complete.obs")
# No correlation
