# Basic importing, re-formatting and distance correction.
getData <- function(fname) {
  library(tidyverse)
  day <- tbl_df(read.csv(fname, stringsAsFactors = F))
  # Remove last entry, which gives summary.
  day <- day %>% slice(-n():-n())
  day$Laps <- as.numeric(day$Laps) 
  
  # Cool function allows splitting pace in mm:ss format
  # by automatically detecting the ":" separator. This
  # is to convert pace to a single plottable number. 
  day <- day %>% separate(Avg.Pace, c("Avg.Pace.Min", "Avg.Pace.Sec"))
  day$Avg.Pace.Min <- as.numeric(day$Avg.Pace.Min)
  day$Avg.Pace.Sec <- as.numeric(day$Avg.Pace.Sec)
  
  # Re-format lap time into a single number for plotting. 
  day <- day %>% separate(Time, c("Time.Min", "Time.Sec"))
  day$Time.Min <- as.numeric(day$Time.Min)
  day$Time.Sec <- as.numeric(day$Time.Sec)
  
  day <- 
    day %>% 
    # Distance correction.
    mutate(Distance = 0.0701900899) %>%
    # Only keep data of interest.
    select(Laps,
           Time.Min,
           Time.Sec,
           Distance,
           Avg.HR,
           Max.HR,
           Cadence = Avg.Run.Cadence,
           Kcals = Calories,
           Temp = Avg.Temperature,
    )
}
  
  # Import nice library for a variety of data manipulation and visualization.
  library(tidyverse)

  # Call data import function defined previously.
  day1 <- getData("~/Downloads/Vadhana Running - 2020-03-24.csv") %>% mutate(Date = "2020-03-24")
  
  # Split up data into 3 sections: fine-fix-fine.
  
  # 'day1.1' is fine.
  day1.1 <- day1 %>% slice(1:9)
  
  # Split 10th lap into 2.
  day1.2 <- day1 %>% slice(10)
  
  day1.2 <- 
    # Create a double entry.
    bind_rows(day1.2, day1.2) %>%
    # Divide the lap time into 2 equal times and re-assign.
    # Same for calories burned.
    mutate(Time.Sec = Time.Min * 60 / 2 + Time.Sec / 2, Time.Min = 0, Kcals = Kcals / 2)
  
  # Lap 10 -> Lap 10 + Lap 11 
  day1.2$Laps[2] <- 11
  
  # 'day1.3' is fine.
  day1.3 <- 
    day1 %>%
    slice(11:n()) %>%
    # Shift lap numbers by due to fixing Lap 10.
    mutate(Laps = Laps + 1)
  
  # Recombine the sections, then fix the pace
  # and convert to single plottable number.
  day1 <- 
    bind_rows(day1.1, day1.2, day1.3) %>%
    mutate(Avg.Pace = Time.Sec / 60 / Distance) %>%
    select(-Time.Min) %>% rename(Time = Time.Sec)

  day2 <- getData("~/Downloads/Vadhana Running - 2020-03-25.csv") %>% mutate(Date = "2020-03-25")
  
  day2.1 <- day2 %>% slice(1:31)
  
  # This time it was Lap 32!
  day2.2 <- day2 %>% slice(32)
  
  day2.2 <- 
    bind_rows(day2.2, day2.2) %>%
    mutate(Time.Sec = Time.Min * 60 / 2 + Time.Sec / 2, Time.Min = 0, Kcals = Kcals / 2)
  
  day2.2$Laps[2] <- 33
  
  day2.3 <- 
    day2 %>%
    slice(33:n()) %>%
    mutate(Laps = Laps + 1)
  
  day2 <- 
    bind_rows(day2.1, day2.2, day2.3) %>%
    mutate(Avg.Pace = Time.Sec / 60 / Distance) %>%
    select(-Time.Min) %>% rename(Time = Time.Sec)
  
  day3 <- getData("~/Downloads/Vadhana Running - 2020-03-27.csv") %>% mutate(Date = "2020-03-27")
  
  day3 <- 
    day3 %>%
    mutate(Avg.Pace = Time.Sec / 60 / Distance) %>%
    select(-Time.Min) %>% rename(Time = Time.Sec)

  # Older library allowing for "unpivoting" the data.
  library(reshape2)
  
  # Combine all days into a single dataset.
  runData <- bind_rows(day1, day2, day3)
  
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
    summarize(avg = mean(value)) %>%
    # Add a column for mean value
    # plot position (y-coordinate)
    left_join(pltRunData %>% group_by(variable) %>%
                summarize(ypos = 0.95*max(value))) %>%
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
  
  ggplot(lessPlt, aes(x=Laps, y=value)) +
    geom_hline(data=lessMean, aes(yintercept = avg), colour="red", lty=3) +
    geom_text(data=lessMean,
              aes(label=round(avg,1), x=62, y=ypos),
              colour="red",
              hjust=1,
              size=3) +
    xlim(c(1,65)) +
    geom_line() + 
    facet_grid(variable ~ Date, scales = "free_y")