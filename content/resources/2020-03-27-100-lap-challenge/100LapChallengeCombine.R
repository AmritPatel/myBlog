# Older library allowing for "unpivoting" the data.
library(reshape2)

# Combine all days into a single dataset.
runData <- bind_rows(day1, day2, day3, day4)