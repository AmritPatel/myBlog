source("~/GitHub/myBlog/content/resources/2020-06-14-heat-training-continues-in-hua-hin/plotRPI.R")
source("~/GitHub/myBlog/content/resources/2020-06-14-heat-training-continues-in-hua-hin/heatLimitedPace.R")

# Wrapper function to generate data for RPI and heat limited pace plots.
getRPIandHLPfromFIT <- function(wd, aname) {
  
  getFit(workDir = wd, actName = aname)
  getCSV(workDir = wd)
  dfRPI(workDir = wd)
  
}

