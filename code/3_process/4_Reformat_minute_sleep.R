# minute_sleep is in different formatting than the rest -------------------
# -> date has :30 seconds. Overview
min(minute_sleep$date) # 2016-04-11 20:48:00
max(minute_sleep$date) # 2016-05-12 09:56:00
minute_sleep %>%arrange(desc(date))
# -> See how many are affected
# Substract seconds
minute_sleep$date_2 <- str_sub(minute_sleep$date, start = -2)
# Count rows = 61,807 of 188,521 ~ 33%
nrow(minute_sleep %>%
       filter(date_2 == 30))
# Best Guess: Remove 30 seconds to have consistent values for all rows
# Find all with "30" and replace with 00, then rename & clean up
str_sub(minute_sleep$date, start = -2, end = -2) <- "0"
minute_sleep <- minute_sleep %>%
  mutate(ActivityMinute = date)
minute_sleep <- select(minute_sleep, -'date_2', -'date')

# Sort by ID, then ActivityMinute
minute_steps <- arrange(minute_steps, Id, ActivityMinute)
