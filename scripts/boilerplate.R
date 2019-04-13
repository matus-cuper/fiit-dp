if (!require("cluster")) install.packages("cluster")
if (!require("clusterCrit")) install.packages("clusterCrit")
if (!require("data.table")) install.packages("data.table")
if (!require("devtools")) install.packages("devtools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("dtwclust")) install.packages("dtwclust")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("rngtools")) install.packages("rngtools")
if (!require("stringr")) install.packages("stringr")
if (!require("TSrepr")) install.packages("TSrepr")
if (!require("zoo")) install.packages("zoo")


devtools::install_github("twitter/AnomalyDetection", quiet = TRUE)
devtools::install_github("twitter/BreakoutDetection", quiet = TRUE)

library(AnomalyDetection)
library(BreakoutDetection)


MPD <- 48
WEEK <- 7


source("~/r/fiit-dp/scripts/loaders.R")
source("~/r/fiit-dp/scripts/aggregators.R")
source("~/r/fiit-dp/scripts/visualizators.R")
source("~/r/fiit-dp/scripts/analyzators.R")
source("~/r/fiit-dp/scripts/clusteringByLaurinec.R")
source("~/r/fiit-dp/scripts/anomalyDetectors.R")
source("~/r/fiit-dp/scripts/utilities.R")
source("~/r/fiit-dp/scripts/filters.R")
source("~/r/fiit-dp/scripts/agg-functions.R")
source("~/r/fiit-dp/scripts/env.R")
source("~/r/fiit-dp/scripts/server-functions.R")
