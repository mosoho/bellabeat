# Brief view --------------------------------------------------------------
summary(heartrate_seconds)
glimpse(heartrate_seconds)

heartrate_seconds %>%
  group_by(Id) %>%
  summarise(Count = n())
# 14 users in this table
# Line graphs  ------------------------------------------------------------
heartrate_seconds %>%
  ggplot(aes(x = Value)) +
  geom_freqpoly() +
  scale_y_continuous(name = "Count", labels = comma)
