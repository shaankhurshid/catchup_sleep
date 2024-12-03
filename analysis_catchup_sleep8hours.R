
library(dplyr)

data8 <- read.csv("~/catchup_sleep/complete_time_sed_with_all_activities_secondary_vars_091624.csv")

### Using 8 hours of sleep ####

# Step 1: Calculate total sleep time over 7 days (in minutes)
data8$total_sleep <- rowSums(data8[, paste0("sleep", 1:7)], na.rm = TRUE)

# Step 2: Check if any day has less than 480 minutes of sleep
# Create a logical matrix where TRUE indicates sleep time < 480 minutes
sleep_less_than_8_hours <- data8[, paste0("sleep", 1:7)] < 480

# Step 3: Assign sleep groups
# Initialize sleep_group with NA
data8$sleep_group <- NA

# Assign Group 1: Not Enough Sleep
data8$sleep_group[data8$total_sleep < 3360] <- "Not Enough Sleep"

# Assign Group 2: Regular Sleep (no days with less than 7 hours)
data8$sleep_group[
  data8$total_sleep >= 3360 &
    apply(sleep_less_than_8_hours, 1, function(x) all(!x))
] <- "Regular Sleep"

# Assign Group 3: Catch-up Sleep (some days with less than 7 hours)
data8$sleep_group[
  data8$total_sleep >= 3360 &
    apply(sleep_less_than_8_hours, 1, any)
] <- "Catch-up Sleep"

# View the updated data with the new 'sleep_group' column
head(data8[, c("sample_id", paste0("sleep", 1:7), "total_sleep", "sleep_group")])


# Calculate total sleep duration in hours
data8$total_sleep_hours <- data8$total_sleep / 60

# Calculate average sleep duration per day in hours
data8$avg_sleep_per_day_hours <- data8$total_sleep_hours / 7


# Export the 'data' dataframe to a CSV file
write.csv(data8, "~/catchup_sleep/data/data_8hours.csv", row.names = FALSE)

