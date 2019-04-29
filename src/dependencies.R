if (!require("cluster")) install.packages("cluster")
if (!require("clusterCrit")) install.packages("clusterCrit")
if (!require("data.table")) install.packages("data.table")
if (!require("devtools")) install.packages("devtools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("dtwclust")) install.packages("dtwclust")
if (!require("dygraphs")) install.packages("dygraphs")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("grid")) install.packages("grid")
if (!require("gtable")) install.packages("gtable")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("rngtools")) install.packages("rngtools")
if (!require("Rtsne")) install.packages("Rtsne")
if (!require("scales")) install.packages("scales")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyquant")) install.packages("tidyquant")
if (!require("TSrepr")) install.packages("TSrepr")
if (!require("zoo")) install.packages("zoo")


devtools::install_github("twitter/AnomalyDetection", quiet = TRUE)
devtools::install_github("twitter/BreakoutDetection", quiet = TRUE)
devtools::install_github("vqv/ggbiplot", quiet = TRUE)

library(AnomalyDetection)
library(BreakoutDetection)
library(ggbiplot)


MPD <- 48
WEEK <- 7


source("~/r/fiit-dp/scripts/agg-functions.R")
source("~/r/fiit-dp/scripts/filter-functions.R")
source("~/r/fiit-dp/scripts/load-functions.R")
source("~/r/fiit-dp/scripts/utilities.R")
source("~/r/fiit-dp/scripts/visualize-functions.R")

prepareDataset <- function(dataDir = "~/data/load/") {
  # Prepare dataset
  IRELAND <- loadIrelandIntoDF(dataDir)
  COLUMNS <- colnames(IRELAND)
  DATACOLUMNS <- colnames(IRELAND)[c(8:length(colnames(IRELAND)))]
  METADATACOLUMNS <- colnames(IRELAND)[c(1:7)]
  IRELAND[DATACOLUMNS] <- lapply(lapply(IRELAND[DATACOLUMNS], unlist), as.double)
  write.csv2(IRELAND, "~/data/ireland.csv", row.names = FALSE)

  # Load variables
  IRELAND <- read.csv2("~/data/ireland.csv")
  IRELAND <- IRELAND[with(IRELAND, order(timestamp)), ]
  IRELAND$id <- NULL
  IRELAND <- IRELAND[241:nrow(IRELAND), ]
  IRELAND <- IRELAND[1:(nrow(IRELAND)-240), ]
  row.names(IRELAND) <- NULL
  COLUMNS <- colnames(IRELAND)
  DATACOLUMNS <- colnames(IRELAND)[c(7:length(colnames(IRELAND)))]
  METADATACOLUMNS <- colnames(IRELAND)[c(1:6)]
  write.csv2(IRELAND, "~/data/ireland.csv", row.names = FALSE)

  if(!exists("IRELAND"))
    IRELAND <- read.csv2("~/data/ireland.csv", stringsAsFactors = FALSE)
  WEEKS <- round(nrow(IRELAND) / (MPD * WEEK))
  # IRELAND <- loadIrelandIntoDF("~/data/load/")

  CLUSTERING_WORKDAYS <- movingWindowWorkdays(windowSize = 2, clusterCount = 25, distanceMetric = "gak")
  CLUSTERING_HOLIDAYS <- movingWindowHolidays(windowSize = 2, clusterCount = 25, distanceMetric = "gak")

  SUSPICIOUS_WORKDAYS <- scoreSuspiciousTS(CLUSTERING_WORKDAYS)
  SUSPICIOUS_HOLIDAYS <- scoreSuspiciousTS(CLUSTERING_HOLIDAYS)
}
