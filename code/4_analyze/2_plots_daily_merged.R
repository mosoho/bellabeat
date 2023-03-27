#### PLOTS for identifying trends

# 'TotalMinutesAsleep' vs 'TotalTimeInBed'  ----------------------------------
ggplot(data = daily_merged, mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +geom_point()
# Outliers. Highlight them for better visualization
# -> Filter everything thats above a certain ratio
daily_merged$RatioBedVsAsleep <- daily_merged$TotalTimeInBed / daily_merged$TotalMinutesAsleep
# -> Sort by ratio in descending order
daily_merged <- daily_merged %>%
  arrange(desc(RatioBedVsAsleep))
# -> Check median to find useful threshold -> 1.2
summary(daily_merged$RatioBedVsAsleep)
# -> Filter by 1.2
daily_merged_ratio_above1.2 <- daily_merged %>%
  filter(RatioBedVsAsleep>=1.2)
# Display highlight on ggplot to check if all outliers are identified
daily_merged %>%
  ggplot(aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
  geom_point(alpha=0.3) +
  geom_point(data = daily_merged_ratio_above1.2,
             aes(x = TotalMinutesAsleep, y = TotalTimeInBed),
             color = 'red')
# -> All these people spend at least 20% time in bed awake. How many are affected?
nrow(daily_merged_ratio_above1.2)
#-> 32 entries
nrow(daily_merged_ratio_above1.2 %>%
       distinct(Id))
#-> 3 people. Out of...
nrow(distinct(daily_merged, Id))
#-> 24 = 12,5%
# Clean up
rm(daily_merged_ratio_above1.2)

# 'SleepDay' vs. 'TotalMinutesAsleep' ---------------------------------------


daily_merged %>%
  ggplot(aes(x = ActivityDate, y = TotalMinutesAsleep)) +
  geom_point()

#-> Update plot with new variable
daily_merged %>%
  ggplot(aes(x = Day, y = TotalMinutesAsleep)) +
  geom_point()
#-> Arrange plot to sort by days of week (Sunday to Saturday)
daily_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalMinutesAsleep)) +
  geom_boxplot() +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.5), size=3) +
  xlab('Wochentage')
#-> Specifically filter by Saturday + Sunday
daily_merged %>%
  filter(Day %in% c("Samstag", "Sonntag")) %>%
  ggplot(aes(x = Day,
             y = TotalMinutesAsleep)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.5, width = 0.3) +
  xlab('Wochentage')



#-> Group by week & weekend
# Display ggplot differentiating between week & weekend
daily_merged %>%
  ggplot(aes(x = Weekday,
             y = TotalMinutesAsleep)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Minutes Asleep')
# Now with minutes in bed
daily_merged %>%
  ggplot(aes(x = Weekday,
             y = TotalTimeInBed)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Minutes in Bed')

## Check outliers in Total Time In Bed
daily_merged <- daily_merged %>%
  arrange(desc(TotalTimeInBed))



#-> Group by week & weekend
# Display ggplot differentiating between week & weekend
daily_merged %>%
  ggplot(aes(x = Weekday,
             y = TotalMinutesAsleep)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Minutes Asleep')
# Now with minutes in bed
daily_merged %>%
  ggplot(aes(x = Weekday,
             y = TotalTimeInBed)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.362), size=3.5) +
  geom_jitter(color="red", size = 1, alpha = 0.3, width = 0.3) +
  xlab('Week vs Weekend') +
  ylab('Minutes in Bed')

## Check outliers in Total Time In Bed
daily_merged <- daily_merged %>%
  arrange(desc(TotalTimeInBed))

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
#-> During week is 7804 and during weekends 6716, so more steps during week



# Day vs. Total Distance --------------------------------------------------
#-> GGPlot: Every day
daily_merged %>%
  ggplot(aes(x = factor(Day, weekdays(as.Date('1970-01-03') + 1:7)),
             y = TotalDistance)) +
  geom_boxplot() +
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.47), size=3) +
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

## Identify user distribution by activity level
# Allocation of Vigorous
daily_merged_healthy_minutes <- daily_merged %>%
  select(Id, WeekISO, VeryActiveMinutes:FairlyActiveMinutes) %>%
  mutate(VigorousMinutes = VeryActiveMinutes) %>%
  mutate(ModerateMinutes = FairlyActiveMinutes) %>%
  select(Id, WeekISO, VigorousMinutes:ModerateMinutes)

# Now group by Id + WeekISO & summarise by addition
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  group_by(Id, WeekISO) %>%
  summarise(VigorousMinutes = sum(VigorousMinutes), ModerateMinutes =
              sum(ModerateMinutes))

# Categorize by healthy & non-healthy activity
#-> Add Equivalent Combination as column
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  mutate(EquivalentMinutes = VigorousMinutes + 0.5*ModerateMinutes)

## Calculate mean of all weeks
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  group_by(Id) %>%
  summarise(VigorousMean = mean(VigorousMinutes),
         ModerateMean = mean(ModerateMinutes),
         EquivalentMean = mean(EquivalentMinutes))

#-> Categorize by how healthy
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  mutate(HealthyCategory = case_when(
    VigorousMean >= 150 | ModerateMean >= 300 | EquivalentMean >= 150 ~ "Very Healthy",
    ModerateMean >= 75 | ModerateMean >= 150 | EquivalentMean >= 75 ~ "Healthy",
    .default = "Not healthy"))

# Calculate distribution of how healthy users are on average
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  group_by(HealthyCategory) %>%
  summarise(Count = n()) %>%
  mutate(Overall = sum(Count)) %>%
  group_by(HealthyCategory) %>%
  summarise(Percent = Count / Overall) %>%
  mutate(PercentCharacter = scales::percent(Percent))


daily_merged_healthy_minutes %>%
  ggplot(aes(x="",y=Percent, fill=HealthyCategory)) +
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
  scale_fill_manual(values = c("#44AA99", "#CC6677", "#6FD233")) +
  geom_text(aes(label = PercentCharacter),
            position = position_stack(vjust = 0.5))+
  labs(title="User distribution") +
  guides(fill=guide_legend(title="Average weekly activity"))

# Clean up
rm(daily_merged_healthy_minutes)



## Identify activity distribution on weekly level
# Allocation of Vigorous
daily_merged_healthy_minutes <- daily_merged %>%
  select(Id, WeekISO, VeryActiveMinutes:FairlyActiveMinutes) %>%
  mutate(VigorousMinutes = VeryActiveMinutes) %>%
  mutate(ModerateMinutes = FairlyActiveMinutes) %>%
  select(Id, WeekISO, VigorousMinutes:ModerateMinutes)

# Now group by Id + WeekISO & summarise by addition
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  group_by(Id, WeekISO) %>%
  summarise(VigorousMinutes = sum(VigorousMinutes), ModerateMinutes =
              sum(ModerateMinutes))

# Categorize by healthy & non-healthy activity
#-> Add Equivalent Combination as column
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  mutate(EquivalentMinutes = VigorousMinutes + 0.5*ModerateMinutes)

#-> Categorize by how healthy
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  mutate(HealthyCategory = case_when(
    VigorousMinutes >= 150 | ModerateMinutes >= 300 | EquivalentMinutes >= 150 ~ "Very Healthy",
    VigorousMinutes >= 75 | ModerateMinutes >= 150 | EquivalentMinutes >= 75 ~ "Healthy",
    .default = "Not healthy"))

# Calculate distribution of how healthy users are on average
daily_merged_healthy_minutes <- daily_merged_healthy_minutes %>%
  group_by(HealthyCategory) %>%
  summarise(Count = n()) %>%
  mutate(Overall = sum(Count)) %>%
  group_by(HealthyCategory) %>%
  summarise(Percent = Count / Overall) %>%
  mutate(PercentCharacter = scales::percent(Percent))


daily_merged_healthy_minutes %>%
  ggplot(aes(x="",y=Percent, fill=HealthyCategory)) +
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
  scale_fill_manual(values = c("#44AA99", "#CC6677", "#6FD233")) +
  geom_text(aes(label = PercentCharacter),
            position = position_stack(vjust = 0.5))+
  labs(title="Activity distribution") +
  guides(fill=guide_legend(title="Average weekly acitivty"))

# Clean up
rm(daily_merged_healthy_minutes)


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
  stat_summary(geom="text",fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.5), size=2.5) +
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

# Single-Variable Analyses ----------------------------------------------
## TotalMinutesAsleep

daily_merged %>%
  ggplot(aes(y = TotalMinutesAsleep)) +
  geom_boxplot()

quantile(daily_merged$TotalMinutesAsleep, na.rm = TRUE)

ncol(t(subset(daily_merged, !is.na(TotalMinutesAsleep))))
#Finding: 50% of 413 users (207) sleep between 361 (6,02h) and 490 8,17h) minutes

daily_merged %>%
  ggplot(aes(x = TotalMinutesAsleep)) +
  geom_histogram(binwidth = 30) +
  stat_bin(binwidth = 30, geom="text", aes(label = ..count..),
           vjust = -1)
# Finding: 333 out of 413 users (80,63%) sleep between X1 and Y2. Normally
#          distributed: Taking mean as average

daily_merged %>%
  ggplot(aes(x = TotalMinutesAsleep)) +
  geom_histogram(
    breaks = seq(50, 800, 50)) +
  stat_bin(binwidth = 25, geom="text", aes(label = ..count..),
           vjust = -1) +
  scale_x_continuous(breaks = seq(50, 800, 50))
# Trying to get precise numbers on x axis + y axis




#-------> Identify how many users get recommended amount of 7+ minutes of sleep
daily_users_sleep <- subset(daily_merged, !is.na(TotalMinutesAsleep)) %>%
  group_by(Id) %>%
  summarise(Count = n(), AverageMinutes = mean(TotalMinutesAsleep)) %>%
  mutate(AverageHours = AverageMinutes/60)

#--> GGPlot with area starting at 7 hours
#-> Prepare table for GGplot: Fix Id type + Reorder function
#-> + indicate low quality
daily_users_sleep <- daily_users_sleep %>%
  mutate(Id = fct_reorder(as.character(Id), AverageHours)) %>%
  mutate(Quality = case_when(
   Count >8 ~ "9 to 31 Days",
   Count <=8 ~ "1 to 8 Days"
  ))
#-> Prepare data for GGplot: Create df of area
rect <- data.frame(xmin=-Inf, xmax=Inf, ymin=7, ymax=Inf)
#--> GGplot
daily_users_sleep %>%
  ggplot(aes(x = Id, y = AverageHours, color = Quality)) +
  scale_color_manual(values = cbPalette) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#05C4BC",
            color="white",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_segment(aes(x=Id, xend=Id, y=0,
                   yend=AverageHours), size = 1.5) +
  geom_point(size = 4, color = "grey") +
  theme_minimal() +
  xlab("") +
  ylab("Hours") +
  guides(color = guide_legend(title = "Data available")) +
  geom_hline(yintercept = 7, linetype = "dashed", size = 1, color = "#05C4BC") +
  annotate("text", x = 1.2, y = 9.3, label = "Healthy", size = 9,
           color = "#60bbc3", fontface = "bold") +
  scale_y_continuous(breaks = c(0, 3, 6, 7, 9, 11),
                     labels = c(0, 3, 6, 7, 9, 11)) +
  coord_flip()
#--> GGPlot, only with users 9+ days of data available
daily_users_sleep <- daily_users_sleep[daily_users_sleep$Count >8, ]
#--> GGplot
daily_users_sleep %>%
  ggplot(aes(x = Id, y = AverageHours)) +
  scale_color_manual(values = cbPalette) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#05C4BC",
            color="white",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_segment(aes(x=Id, xend=Id, y=0,
                   yend=AverageHours), size = 1.5, color = "orange") +
  geom_point(size = 4, color = "grey") +
  theme_minimal() +
  xlab("") +
  ylab("Hours") +
  guides(color = guide_legend(title = "Data available")) +
  geom_hline(yintercept = 7, linetype = "dashed", size = 1, color = "#05C4BC") +
  annotate("text", x = 1.2, y = 8, label = "Healthy", size = 9,
           color = "#60bbc3", fontface = "bold") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 7, 8),
                     labels = c(0, 2, 4, 6, 7, 8)) +
  coord_flip()


# Clean up
rm(daily_users_sleep, rect)

## TotalSteps
daily_merged %>%
  ggplot(aes(y = TotalSteps)) +
  geom_boxplot()

quantile(daily_merged$TotalSteps, na.rm = TRUE)

ncol(t(subset(daily_merged, !is.na(TotalSteps))))
#Finding: 472 of 943 steps (50%) had a length between 3,795 and 10,734

daily_merged %>%
  ggplot(aes(x = TotalSteps)) +
  geom_histogram(binwidth = 1000) +
  stat_bin(binwidth = 1000, geom="text", aes(label = ..count..),
           vjust = -1)
#Finding: 896 out of 943 steps (95,02%) had a length between 0 and 15,500

## TotalDistance
daily_merged %>%
  ggplot(aes(y = TotalDistance)) +
  geom_boxplot()

quantile(daily_merged$TotalDistance, na.rm = TRUE)

ncol(t(subset(daily_merged, !is.na(TotalDistance))))
#Finding: 472 of 943 distances travelled (50%) had a length between 2.62 and 7.72

daily_merged %>%
  ggplot(aes(x = TotalDistance)) +
  geom_histogram(binwidth = 2) +
  stat_bin(binwidth = 2, geom="text", aes(label = ..count..),
           vjust = -1)
#Finding: 910 out of 943 distances travelled (96,50%) had a length between 0 and 10


## Calories
daily_merged %>%
  ggplot(aes(y = Calories)) +
  geom_boxplot()

quantile(daily_merged$Calories, na.rm = TRUE)

ncol(t(subset(daily_merged, !is.na(Calories))))
#Finding: 472 of 943 calories burned (50%) ranged from 1,829.5 to 2,796.5

daily_merged %>%
  ggplot(aes(x = Calories)) +
  geom_histogram(binwidth = 250) +
  stat_bin(binwidth = 250, geom="text", aes(label = ..count..),
           vjust = -1)
#Finding: 780 out of 943 calories burned (82,72%) ranged from 1,500 to 3,000
