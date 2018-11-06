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
  train$Values <- as.double(as.character(train$Values))
  train$Timestamp <- as.POSIXlt(train$Timestamp)

  # For some reason, missing values are identified as 1, not NA
  train$Values[train$Values == 1] <- NA

  # Approximate missing values
  approximation <- data.frame(train$Values, index(1:nrow(train)))
  approximation <- data.frame(na.approx(approximation, maxgap = 100))
  train$Values <- approximation$train.Values

  train$date <- as.factor(strftime(train$Timestamp, "%Y-%m-%d"))
  train$time <- as.factor(strftime(train$Timestamp, "%H:%M:%S"))
  train$day <- as.integer(as.POSIXlt(train$date)$wday + 1)
  train$holiday <- as.integer(0)

  colnames(train) <- c("timestamp", "load", "date", "time", "day", "holiday")
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
  train$dayId <- NULL

  train$load <- as.double(as.character(train$load))
  train$dateTime <- as.POSIXlt(train$dateTime)

  colnames(train) <- c("timestamp", "date", "time", "day", "holiday", "load")
  train <- train[, c(1, 6, 2, 3, 4, 5)]
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

  # Remove NA values
  train <- train[!is.na(train$load), ]

  # Make temporary dataset more readable
  agg <- aggregate(load ~ date, data = train, FUN = length)
  agg$date <- strptime(agg$date, "%Y-%m-%d")
  agg$freq <- agg$load
  agg$load <- NULL

  # Find day period and remove uncomplete days
  freq <- max(agg$freq)

  # Filter unused rows
  train <- train[train$date %in% agg[freq == agg$freq,]$date, ]

  if (groupBy > 1)
    train <- groupByAggregate(train, groupBy)

  return(train)
}

groupByAggregate <- function(dataset, groupSize) {
  # Sum load by generated group
  dataset$id <- ceiling(c(1:nrow(dataset)) / groupSize)
  agg <- aggregate(cbind(load) ~ id, data = dataset, sum)

  # Join dataset and rename columns
  dataset <- dataset[seq(1 + ceiling((groupSize / 2) - 1), nrow(dataset), groupSize), c("timestamp", "date", "time", "day", "holiday") ]
  dataset$load <- agg$load
  dataset <- dataset[, c(1, 6, 2, 3, 4, 5)]

  return(dataset)
}

groupByAddColumn <- function(dataset, groupSize) {
  dataset$group <- c(1:nrow(dataset)) %% groupSize + 1
  return(dataset)
}

visualizeDataset <- function(dataset, days = WEEK, sleepTime = 0.5) {
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
  datasetSize <- freq * days

  plot(ts(dataset$load[1:datasetSize]))
  for (i in 1:(nrow(dataset) / datasetSize)) {
    lines(ts(dataset$load[((i * datasetSize) + 1):((1 + i) * datasetSize)]))
    Sys.sleep(sleepTime)
  }

  dataset <- groupBy(dataset, datasetSize)
  mea <- aggregate(load ~ group, data = dataset, FUN = mean)
  med <- aggregate(load ~ group, data = dataset, FUN = median)
  lines(mea, col = "red", lwd = 2)
  lines(med, col = "green", lwd = 2)
}

findAnomaliesSum <- function(dataset, days = 4 * WEEK, aggregations = c(0, 1, 2, 3, 4)) {
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
  datasetSize <- freq * days

  dataset <- dataset[1:datasetSize, ]
  anomalies <- data.frame(matrix(0, nrow = nrow(dataset), ncol = length(aggregations)))

  for (a in aggregations) {
    ratio <- 2**a
    # TODO create new func
    train <- groupByAggregate(dataset, ratio)
    train <- train[1:(datasetSize / ratio), ]

    res <- AnomalyDetectionVec(
      train$load,
      max_anoms=0.1,
      direction='both',
      plot=TRUE,
      period = freq / ratio,
      longterm_period = freq * WEEK / ratio
    )

    print(paste("period", freq / ratio))
    print(paste("long period", freq * WEEK / ratio))

    plot(ts(train$load, frequency = freq / ratio))
    # plot(res$plot)
    Sys.sleep(1)
    plot(res$plot)
    Sys.sleep(2)

    for (j in 1:ratio) {
      anomalies[res$anoms$index * ratio + j, i + 1] <- 1
    }
  }

  anomalies$score <- apply(anomalies, 1, FUN = function(x) sum(x))
  return(anomalies)
}

visualizeDataset <- function(dataset, days = WEEK, sleepTime = 0.5) {
  dataset <- c
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
  datasetSize <- freq * days

  plot(ts(dataset$load[1:datasetSize]))

  res <- AnomalyDetectionVec(
    dataset$load[1:datasetSize],
    max_anoms=0.1,
    direction='both',
    plot=TRUE,
    period = freq,
    longterm_period = freq * WEEK
  )


  for (i in 1:(nrow(dataset) / datasetSize)) {
    lines(ts(dataset$load[((i * datasetSize) + 1):((1 + i) * datasetSize)]))
    Sys.sleep(sleepTime)
  }

  dataset <- groupBy(dataset, datasetSize)
  mea <- aggregate(load ~ group, data = dataset, FUN = mean)
  med <- aggregate(load ~ group, data = dataset, FUN = median)
  lines(mea, col = "red", lwd = 2)
  lines(med, col = "green", lwd = 2)
}
