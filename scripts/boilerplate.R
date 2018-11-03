# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")
# install.packages("zoo")

library(AnomalyDetection)
library(dplyr)
library(lubridate)
library(zoo)


WEEK <- 7


loadDatasetFromDDC <- function(pathToFile) {
  train <- read.csv2(pathToFile, sep = ',')
  train$Values <- as.numeric(train$Values)
  train$Timestamp <- as.POSIXlt(train$Timestamp)

  # For some reason, missing values are identified as 1, not NA
  train$Values[train$Values == 1] <- NA

  # Approximate missing values
  approximation <- data.frame(train$Values, index(1:nrow(train)))
  approximation <- data.frame(na.approx(approximation, maxgap = 100))
  train$Values <- approximation$train.Values

  colnames(train) <- c("timestamp", "load")
  return(train)
}

loadDatasetFromDDCSummarized <- function(pathToFile) {
  train <- loadDatasetFromDDC(pathToFile)
  train$load <- diff(train$load)[1:nrow(train)]

  return(train)
}

loadDatasetFromSchool <- function(pathToFile) {
  train <- read.csv2(pathToFile)
  train$id <- NULL
  train$date <- NULL
  train$time <- NULL
  train$dayOfWeek <- NULL
  train$dayId <- NULL
  train$holiday <- NULL

  train$load <- as.numeric(train$load)
  train$dateTime <- as.POSIXlt(train$dateTime)

  colnames(train) <- c("timestamp", "load")
  return(train)
}

loadDataset <- function(pathToFile, groupBy = 1) {
  if (grepl("electricity", pathToFile)) {
    train <- loadDatasetFromDDC(pathToFile)
  } else if (grepl("summarized", pathToFile)) {
    train <- loadDatasetFromDDCSummarized(pathToFile)
  } else if (grepl("slovakia", pathToFile)) {
    train <- loadDatasetFromSchool(pathToFile)
  } else if (grepl("ireland", pathToFile)) {
    train <- loadDatasetFromSchool(pathToFile)
  }

  if (!exists("train")) {
    return(NULL)
  }

  # First create date column
  train <- train[!is.na(train$load), ]
  train$date <- strftime(train$timestamp, "%Y-%m-%d")

  # Make temporary dataset more readable
  agg <- aggregate(load ~ date, data = train, FUN = length)
  agg$date <- strptime(agg$date, "%Y-%m-%d")
  agg$freq <- agg$load
  agg$load <- NULL

  # Find day period and remove uncomplete days
  freq <- max(agg$freq)

  # Filter unused rows and columns
  train <- train[train$date %in% agg[freq == agg$freq,]$date, ]
  train$date <- NULL

  if (groupBy > 1) {
    train$id <- ceiling(c(1:nrow(train)) / groupBy)
    agg <- aggregate(cbind(load) ~ id, data = train, sum)
    timestamps <- train$timestamp[seq(1, nrow(train), groupBy)]
    train <- data.frame(timestamps, agg$load)
  }

  colnames(train) <- c("timestamp", "load")
  return(train)
}
