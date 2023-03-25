## How many IDs do we have?
test <- weight_log_info %>%
  group_by(Id) %>%
  summarise(Entries = n())

# Finding: 8 users have used the weight log. Only 3 entered more than 2 records (5,
#          24 and 30)
# Clean up
rm(test)

## How many people do use manual/automatic reporting?
weight_log_info %>%
  filter(IsManualReport == TRUE) %>%
  group_by(Id, IsManualReport) %>%
  summarise(Entries = n())

# 3 use automatic reporting
# 5 use manual reporting

# How tall are participants?
# We have BMI and WeightKg. Needed = HeightM.
# WeightKg / (HeightM)² = BMI
# ->   HeightM =  (WeightKg / BMI )squareroot

weight_log_info$HeightM <- sqrt(weight_log_info$WeightKg / weight_log_info$BMI)

# Now group by user
weight_log_info %>%
  group_by(Id) %>%
  summarise(HeightMMedian = median(HeightM)) %>%
  ggplot(aes(x = HeightMMedian)) +
  geom_histogram() +
  scale_x_continuous(name="Height in Meters", breaks=seq(1.5,1.9,by=.01)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
