# Daily_calories as already in daily_activity
cor(daily_activity_v2$Calories, daily_calories_v2$Calories)

# Daily_steps as already in daily_activity
cor(daily_activity_v2$TotalSteps, daily_steps_v2$StepTotal)

# Daily_intensities as already in daily_activity
cor(daily_activity_v2$SedentaryMinutes, daily_intensities_v2$SedentaryMinutes)
cor(daily_activity_v2$LightlyActiveMinutes, daily_intensities_v2$LightlyActiveMinutes)
cor(daily_activity_v2$FairlyActiveMinutes, daily_intensities_v2$FairlyActiveMinutes)
cor(daily_activity_v2$VeryActiveMinutes, daily_intensities_v2$VeryActiveMinutes)

cor(daily_activity_v2$SedentaryActiveDistance, daily_intensities_v2$SedentaryActiveDistance)
cor(daily_activity_v2$LightActiveDistance, daily_intensities_v2$LightActiveDistance)
cor(daily_activity_v2$ModeratelyActiveDistance, daily_intensities_v2$ModeratelyActiveDistance)
cor(daily_activity_v2$VeryActiveDistance, daily_intensities_v2$VeryActiveDistance)

# remove those tables that are not needed anymore
rm(daily_intensities_v2)
rm(daily_calories_v2)
rm(daily_steps_v2)
