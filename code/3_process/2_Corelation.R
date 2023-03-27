# Daily_calories as already in daily_activity
cor(daily_activity$Calories, daily_calories$Calories)

# Daily_steps as already in daily_activity
cor(daily_activity$TotalSteps, daily_steps$StepTotal)

# Daily_intensities as already in daily_activity
cor(daily_activity$SedentaryMinutes, daily_intensities$SedentaryMinutes)
cor(daily_activity$LightlyActiveMinutes, daily_intensities$LightlyActiveMinutes)
cor(daily_activity$FairlyActiveMinutes, daily_intensities$FairlyActiveMinutes)
cor(daily_activity$VeryActiveMinutes, daily_intensities$VeryActiveMinutes)

cor(daily_activity$SedentaryActiveDistance, daily_intensities$SedentaryActiveDistance)
cor(daily_activity$LightActiveDistance, daily_intensities$LightActiveDistance)
cor(daily_activity$ModeratelyActiveDistance, daily_intensities$ModeratelyActiveDistance)
cor(daily_activity$VeryActiveDistance, daily_intensities$VeryActiveDistance)

# remove those tables that are not needed anymore
rm(daily_intensities)
rm(daily_calories)
rm(daily_steps)
