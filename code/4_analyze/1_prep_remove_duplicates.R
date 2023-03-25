# Discover duplicate rows
nrow(daily_merged[duplicated(daily_merged), ]) # 3
nrow(hourly_merged[duplicated(hourly_merged), ]) # 0
nrow(minute_merged[duplicated(minute_merged), ]) # 543
nrow(heartrate_seconds[duplicated(heartrate_seconds), ]) # 0
nrow(weight_log_info[duplicated(weight_log_info), ]) # 0

# Remove duplicates for daily_merged and minute_merged
daily_merged <- daily_merged %>%
  distinct()

minute_merged <- minute_merged %>%
  distinct()
