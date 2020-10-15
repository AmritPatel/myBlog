# Assign RHR.
# Based on previous 7 days read off of watch.
RHR <- 60

# Calculate RPI
# (from: https://fellrnr.com/wiki/Running_Economy).
runData <- 
  runData %>%
  mutate(RPI = 1 / (((Avg.HR - RHR) * (Time / 60)) / Distance) * 100000)

runData$Date <- as.Date(runData$Date)

# Change the data from "wide" to "long'
# to make plotting easier.
pltRunData <- 
  melt(runData, id.vars = c("Date", "Laps")) %>%
  filter(variable != "Distance")

# Create a dataset of variable means
# to add to plot.
meanData <- pltRunData %>%
  # Mean data
  group_by(Date, variable) %>%
  summarise(avg = mean(value)) %>%
  # Add a column for mean value
  # plot position (y-coordinate)
  left_join(pltRunData %>% group_by(variable) %>%
              summarise(ypos = 0.95*max(value))) %>%
  filter(variable != "Distance")

lessPlt <- pltRunData %>% filter(variable == "Avg.HR" |
                                   variable == "Avg.Pace" |
                                   variable == "Cadence" |
                                   variable == "Temp" |
                                   variable == "RPI"   
)

lessMean <- meanData %>% filter(variable == "Avg.HR" |
                                  variable == "Avg.Pace" |
                                  variable == "Cadence" |
                                  variable == "Temp" |
                                  variable == "RPI"
) 