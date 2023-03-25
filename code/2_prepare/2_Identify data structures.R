# Function to look into data
view_into <- function(df){
  view_into_skim <- skim_without_charts(df)
  view_into_head <- head(df)
  view_into_colnames <- colnames(df)
  view_into_glimpse <- glimpse(df)
  view_into_list <- list("skim_without_charts" = view_into_skim,
                         "head" = view_into_head,
                         "colnames" = view_into_colnames,
                         "glimpse" = view_into_glimpse)
  return(view_into_list)
}

# View into all available data
view_into(daily_activity)
view_into(daily_calories)
view_into(daily_intensities)
view_into(daily_steps)
view_into(heartrate_seconds)
view_into(hourly_calories)
view_into(hourly_intensities)
view_into(hourly_steps)
view_into(minute_calories_narrow)
view_into(minute_calories_wide)
view_into(minute_intensities_narrow)
view_into(minute_intensities_wide)
view_into(minute_mets_narrow)
view_into(minute_sleep)
view_into(minute_steps_narrow)
view_into(minute_steps_wide)
view_into(sleep_day)
view_into(weight_log_info)

# Look for duplicates
sum(duplicated(daily_merged)) # 3
sum(duplicated(heartrate_seconds)) # 0
sum(duplicated(hourly_merged)) # 0
sum(duplicated(minute_merged)) # 543
sum(duplicated(weight_log_info)) # 0
