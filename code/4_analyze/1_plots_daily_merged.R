#### PLOTS for identifying trends + MERGING tables

# MERGING tables --------
# daily_activity + sleep_day = daily_merged

daily_merged <-
  daily_activity %>%
  full_join(sleep_day, by = c('Id'='Id', 'ActivityDate'='SleepDay'))

# Clean up old tables
rm(daily_activity, sleep_day)

# Renaming columns for readability
daily_merged <-
  daily_merged %>%
  rename(Day = SleepDay_Day) %>%
  rename(Weekday = SleepDay_Group)

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

#-> Translate to English
daily_merged$Day <- case_when(
  daily_merged$Day == "Samstag" ~ "Saturday",
  daily_merged$Day == "Sonntag" ~ "Sunday",
  daily_merged$Day == "Montag" ~ "Monday",
  daily_merged$Day == "Dienstag" ~ "Tuesday",
  daily_merged$Day == "Mittwoch" ~ "Wednesday",
  daily_merged$Day == "Donnerstag" ~ "Thursday",
  daily_merged$Day == "Freitag" ~ "Friday",
)

# Rearranging table for readability
daily_merged <- daily_merged %>%
  relocate(Day, .after = ActivityDate)

daily_merged <- daily_merged %>%
  relocate(Weekday, .after = Day)



# TotalSteps vs. Total Distance -------------------------------------------


daily_merged %>%
  ggplot(aes(x = TotalSteps,
             y = TotalDistance)) +
  geom_point(alpha = 0.3)
#--> Filter everything thats above a certain ratio
daily_merged$RatioStepsVsDistance <-
  daily_merged$TotalDistance /
  daily_merged$TotalSteps
#--> Arrange after TotalDistance
daily_merged <- daily_merged %>%
  relocate(RatioStepsVsDistance, .after = TotalDistance)
#--> Sort by ratio in descending order
daily_merged <- daily_merged %>%
  arrange(desc(RatioStepsVsDistance))
#--> Show stat. summary to see where to set cut-off
summary(daily_merged$RatioStepsVsDistance)
#--> Filter by > 0.00082
RatioStepsVsDistance_temp <- daily_merged %>%
  filter(RatioStepsVsDistance>0.00082)
# Display highlight on ggplot to check if all outliers are identified
daily_merged %>%
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
nrow(distinct(daily_merged, Id)) # 33 people
#-> = 15,15%
# Clean up
rm(RatioStepsVsDistance_temp)



# TotalSteps vs. TrackerDistance ------------------------------------------


daily_merged %>%
  ggplot(aes(x = TrackerDistance,
             y = TotalSteps)) +
  geom_point(alpha = 0.5)
#-> Same as TotalSteps vs. Total Distance





# TotalDistance vs. TrackerDistance ---------------------------------------


daily_merged %>%
  ggplot(aes(x = TrackerDistance,
             y = TotalDistance)) +
  geom_point(alpha = 0.5)

cor(daily_merged$TrackerDistance, daily_merged$TotalDistance)
#-> Assuming TrackerDistance means, how much distance the tracker measures,
# and TotalDistance measures how much distance they have gone.
# Should be correlation of 1, but there are some outliers
#-> Identify outliers by implementing ratio
#--> Filter everything thats above a certain ratio
daily_merged$RatioTrackerVsTotal <-
  daily_merged$TotalDistance /
  daily_merged$TrackerDistance
#--> Arrange after TrackerDistance
daily_merged <- daily_merged %>%
  relocate(RatioTrackerVsTotal, .after = TrackerDistance)
#--> Sort by ratio in descending order
daily_merged <- daily_merged %>%
  arrange(desc(RatioTrackerVsTotal))
#--> Filter by > 1
RatioTrackerVsTotal_temp <- daily_merged %>%
  filter(RatioTrackerVsTotal>1)

# Display highlight on ggplot to check if all outliers are identified
daily_merged %>%
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
nrow(distinct(daily_merged, Id)) # 33 people
#-> = 6%
# Clean up
rm(RatioTrackerVsTotal_temp)



# Day vs. TotalSteps ------------------------------------------------------
#-> Arrange plot to sort by days of week (Sunday to Saturday)
#--> First translate back to German
daily_merged$Day <- case_when(
  daily_merged$Day == "Saturday" ~ "Samstag",
  daily_merged$Day == "Sunday" ~ "Sonntag",
  daily_merged$Day == "Monday" ~ "Montag",
  daily_merged$Day == "Tuesday" ~ "Dienstag",
  daily_merged$Day == "Wednesday" ~ "Mittwoch",
  daily_merged$Day == "Thursday" ~ "Donnerstag",
  daily_merged$Day == "Friday" ~ "Freitag",
)
#-> GGPlot: Every day
daily_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalSteps)) +
  geom_boxplot(outlier.shape = NA) +
  # stat_summary(geom="text",fun.y=quantile,
  #             aes(label=sprintf("%1.1f", ..y..)),
  #             position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Weekdays')
#-> GGPlot: Week vs. Weekend
daily_merged %>%
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
daily_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalDistance)) +
  geom_boxplot() +
  xlab('Weekdays')
#-> GGPlot: Week vs. Weekend
daily_merged %>%
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
summary(daily_merged[c("VeryActiveDistance",
                       "ModeratelyActiveDistance",
                       "LightActiveDistance",
                       "SedentaryActiveDistance")])


# Overall share of distance activity distribution
#1. SUM each column
VeryActiveDistance_sum <- sum(daily_merged$VeryActiveDistance)
ModeratelyActiveDistance_sum <- sum(daily_merged$ModeratelyActiveDistance)
LightActiveDistance_sum <- sum(daily_merged$LightActiveDistance)
SedentaryActiveDistance_sum <- sum(daily_merged$SedentaryActiveDistance)

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
summary(daily_merged[c("VeryActiveMinutes", "FairlyActiveMinutes",
                       "LightlyActiveMinutes", "SedentaryMinutes")])


# Overall share of distance activity distribution
#1. SUM each column
VeryActiveMinutes_sum <- sum(daily_merged$VeryActiveMinutes)
FairlyActiveMinutes_sum <- sum(daily_merged$FairlyActiveMinutes)
LightlyActiveMinutes_sum <- sum(daily_merged$LightlyActiveMinutes)
SedentaryMinutes_sum <- sum(daily_merged$SedentaryMinutes)

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

daily_merged %>%
  ggplot(aes(x = SedentaryMinutes,
             y = TotalTimeInBed)) +
  geom_point(alpha = 0.5) +
  geom_smooth()

cor(daily_merged$SedentaryMinutes, daily_merged$TotalTimeInBed, use="complete.obs")


# Calories ----------------------------------------------------------------

summary(daily_merged$Calories)

daily_merged %>%
  ggplot(aes(x = "", y = Calories)) +
  geom_boxplot()

#-> GGPlot: Every day
daily_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = Calories)) +
  geom_boxplot(outlier.shape = NA) +
  # stat_summary(geom="text",fun.y=quantile,
  #             aes(label=sprintf("%1.1f", ..y..)),
  #             position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Weekdays')
#-> GGPlot: Week vs. Weekend
daily_merged %>%
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

daily_merged %>%
  ggplot(aes(x = TotalMinutesAsleep, y = Calories)) +
  geom_point(alpha = 0.5)

cor(daily_merged$TotalMinutesAsleep,
    daily_merged$Calories, use = "complete.obs")
# No correlation
