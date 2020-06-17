# Basic importing, re-formatting and distance correction.
getData <- function(fname, RHR) {
  
  library(tidyverse)
  
  # Read in .fit data and create df columns of interest.
  tbl_df(read.csv(fname, stringsAsFactors = F)) %>%
    mutate(Time = row_number(timestamp),
           Distance = distance / 1.609, # Convert from km to miles.
           Avg.Pace = 1000 * 60 / (speed / 1.609), # Convert from kmh to min/mi. Bug somewhere causing 1000 factor. 
           Avg.HR = heart_rate,
           RPI = 1 / (((Avg.HR - RHR) * (1 / 60)) / (Distance - lag(Distance))) * 100000,
           Cadence = cadence * 2,
           #Kcals = total_calories,
           Temp = temperature * 9 / 5 + 32,
           Date = as.Date(timestamp)
    ) %>%
    select(Time:Date)
}

# Create single dataframe from runs of interest.
dfRPI <- function(workDir) {
  
  library(data.table)
  
  setwd(workDir)
  
  # lapply + glob passes all .csv files to previously defined data import and formatting function `getData`.
  data <- lapply(Sys.glob(paste0(workDir, "*.csv")), getData, RHR = 56) 
  
  # Combine created list data into a single dataframe.
  df <- rbindlist(data)
  
}

# Add user-provided humidity data to the running activity datqframe, then calculate and add a heat index column
# (using `weathermetrics`) and previously validated heat limited pace model described in 2020-06-13 post.
addHI <- function(df, hum) {
  
  library(weathermetrics)
  library(tidyverse)

  hum$Date <- as.Date(hum$Date)
  
  # Load previously defined heat limited pace model.
  heatPaceKM <- readRDS("~/GitHub/myBlog/content/resources/2020-06-13-heat-training/heatPaceKM.rds")
  
  # Define heat limited pace function from model.
  heatLimPace <- function(T, H) {
    predict(heatPaceKM, data.frame(temp=c(T), humidity=c(H)), type = "UK")$mean
  }
  
  # Add humidity data to dataframe, 
  left_join(df, hum) %>%
    # Add heat index column,
    mutate(heatIndex = heat.index(Temp, rh=humidity),
           # Add heat limited pace column,
           predKM = heatLimPace(Temp, humidity),
           # And add column indicating whether or not heat limited pace was violated.
           Violation = ifelse(Avg.Pace < predKM, TRUE, FALSE)
    )
}

# Format, then plot RPI along with a few other variables of interest.
plotRPI <- function(df, titleText) {

  library(tidyverse)
  
  # Calculate standard deviation and mean data for RPI data to filter out data collected before 1.3 mi. that is biased high.
  df <- 
    df %>%
    group_by(Date) %>%
    filter(Distance > 1.3) %>%
    summarise(sd = sd(RPI, na.rm = T)) %>%
    left_join(df)
  
  df <- 
    df %>%
    group_by(Date) %>%
    filter(Distance > 1.3) %>%
    summarise(mn = mean(RPI, na.rm = T)) %>%
    left_join(df)
  
  # Change the data from "wide" to "long' to make plotting easier.
  pltRunData <- 
    reshape2::melt(df %>%
                            # Filter out early inaccurate data.
                     mutate(Avg.Pace = ifelse(Distance <= 0.1, NA, Avg.Pace),
                            Avg.HR = ifelse(Distance <= 0.1, NA, Avg.HR),
                            RPI = ifelse(Distance <= 1.3, NA, RPI),
                            # Filter out points that exceed # of standard deviations specified.
                            RPI = ifelse(abs(RPI - mn) > 1.2 * sd, NA, RPI),
                            Cadence = ifelse(Distance <= 0.1, NA, Cadence),
                     ),
                   id.vars = c("Date", "Distance")
    )
  
  # Create a dataset of variable means to add to plots.
  meanData <- 
    # Mean data
    bind_rows(
      pltRunData %>%
        group_by(Date, variable) %>%
        filter(variable != "RPI") %>%
        summarise(avg = mean(value, na.rm = T, trim = 0.002)),
      # RPI data doesn't settle until after about 1.3 miles into run so needs special treatment.
      pltRunData %>%
        group_by(Date, variable) %>%
        filter(variable == "RPI", Distance > 1.3) %>%
        summarise(avg = mean(value, na.rm = T, trim = 0))
    ) %>%
    # Add a column for mean value plot position (x,y-coordinates).
    left_join(pltRunData %>%
                group_by(Date, variable) %>%
                summarise(ypos = mean(value, na.rm = T, trim = 0.01),
                          xpos = 0.95 * mean(Distance, na.rm = T, trim = 0.01)
                )
    ) %>%
    filter(variable == "Avg.HR" |
             variable == "Avg.Pace" |
             variable == "Cadence" |
             variable == "heatIndex" |
             variable == "RPI"
    )
  
  # Only keep variables of interest.
  lessPlt <-
    pltRunData %>%
    filter(variable == "Avg.HR" |
             variable == "Avg.Pace" |
             variable == "Cadence" |
             variable == "heatIndex" |
             variable == "RPI"   
  )
  
  lessMean <-
    meanData %>%
    filter(variable == "Avg.HR" |
             variable == "Avg.Pace" |
             variable == "Cadence" |
             variable == "heatIndex" |
             variable == "RPI"
  ) 
  
  ggplot(lessPlt, aes(x=Distance, y=value)) +
    geom_hline(data=lessMean, aes(yintercept = avg), colour="red", lty=3) +
    geom_text(data=lessMean,
              aes(label=round(avg,1), x=xpos, y=ypos),
              colour="red",
              vjust=-1,
              size=3) +
    geom_line() + 
    facet_grid(variable ~ Date, scales = "free") +
    labs(title = titleText,
         #subtitle = "",
         x = "Distance (mi)",
         y = "Value"
    )
  
}

# RPI vs. distance plot indicating where in the run the heat limited pace was violated. Only valid if user supplies humidity data.
plotViolation <- function(df, titleText) {
  
  library(gghighlight)
  library(tidyverse)
  
  ggplot(df, aes(Distance, RPI, color = Violation)) +
    geom_point() +
    facet_wrap(~Date, scales = "free") +
    theme(legend.position="bottom") +
    gghighlight(Violation == TRUE,
                calculate_per_facet = T,
                use_direct_label = F
    ) +
    labs(title = titleText,
         subtitle = "Red points indicate when pace faster than heat limited pace",
         x = "Distance (mi)",
         y = "Running Performance Index"
    )
}

# Summary plot intended to compare average RPIs from multiple runs to visually check if heat limited violations impact RPI.
plotViolationSumm <- function(df, titleText) {
  
  library(gghighlight)
  library(tidyverse)
  
  # Load previously defined heat limited pace model.
  heatPaceKM <- readRDS("~/GitHub/myBlog/content/resources/2020-06-13-heat-training/heatPaceKM.rds")
  
  # Define heat limited pace function from model.
  heatLimPace <- function(T, H) {
    predict(heatPaceKM, data.frame(temp=c(T), humidity=c(H)), type = "UK")$mean
  }
  
  pltDF <-
    df %>%
    group_by(Date) %>%
    summarise(Avg.Pace = mean(Avg.Pace, na.rm = T, trim = 0.01),
              Temp = mean(Temp, na.rm = T),
              humidity = mean(humidity, na.rm = T),
              RPI = mean(RPI, na.rm = T)
              ) %>%
    mutate(predKM = heatLimPace(Temp, humidity),
           Violation = ifelse(Avg.Pace < predKM, TRUE, FALSE)
    )
  
  ggplot(pltDF, aes(Date, RPI, color = Violation)) +
    geom_point() +
    theme(legend.position="bottom") +
    gghighlight(Violation == TRUE,
                calculate_per_facet = T,
                use_direct_label = F
    ) +
    labs(title = titleText,
         subtitle = "Red points indicate when average pace faster than heat limited pace",
         x = "Date",
         y = "Running Performance Index"
    )
}