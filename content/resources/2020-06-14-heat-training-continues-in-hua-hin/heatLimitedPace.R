# Get list of .fit files based on running activity IDs.
getFit <- function(workDir, actName) {
  library(DBI)
  library(RSQLite)
  library(tidyverse)
  library(stringr)

  # Establish database connection.
  con <- dbConnect(SQLite(), "/Users/amrit/HealthData/DBs/garmin_activities.db")

  # Make dataframe of running actvities.
  runningActs <- dbReadTable(con, 'running_activities_view')

  # Path of the SQLite activities database.
  path <- "~/HealthData/FitFiles/Activities/"

  # Get .fit filenames based on running activity names.
  runs <-
    runningActs %>%
    # Allow partial activity name matching.
    filter(str_detect(name, actName)) %>%
    select(activity_id) %>%
    mutate(activity_id = paste0(path, activity_id, ".fit"))

  # Setup a working directory to copy files to for processing.
  setwd(workDir)

  # Copy .fit files to working directory.
  lapply(paste("cp", runs$activity_id, workDir), system)

}

# Convert .fit files to .csv.
getCSV <- function(workDir) {

  library(reticulate)

  # Run python file created by @mcandocia on GitHub @ https://github.com/mcandocia/examples/tree/master/convert_fit_to_csv 
  # to convert .fit data to .csv to allow for processing in R.
  source_python("/Users/amrit/HealthData/FitFiles/convert_fit_to_csv.py", envir = NULL)

  setwd(workDir)

  # Remove unneeded .csv files.
  system("rm *laps.csv")
  system("rm *starts.csv")

}

# Clear the working directory to allow for subsequent jobs.
cleanup <- function() {
  
  system("rm *.fit *.csv *.log True")
  
}
