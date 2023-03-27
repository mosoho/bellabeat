# Find duplicates in minute_sleep
sum(duplicated(minute_sleep))
# Find duplicates in sleep_day
sum(duplicated(sleep_day))

# Clean both dataframes from duplicates
minute_sleep <- minute_sleep %>%
  distinct()

sleep_day <- sleep_day %>%
  distinct()
