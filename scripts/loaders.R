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


loadWholeDataset <- function(targetDirectory, weeks = 10, fileCount = Inf) {
  result <- data.frame()
  counter <- 0

  for (f in list.files(targetDirectory)) {
    # Load new file for processing
    pathToFile <- paste(targetDirectory, f, sep = "/")
    dataset <- loadDataset(pathToFile)
    print(paste("Start processing", pathToFile))
    counter <- counter + 1

    # Filter rows and columns
    freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
    dataset <- dataset[1:(freq * WEEK * weeks), ]
    dataset <- dataset[, c("timestamp", "load")]
    colnames(dataset) <- c("timestamp", f)
    dataset$timestamp <- as.character(dataset$timestamp)

    # Merge dataframes
    if (length(result) > 0)
      result <- merge(result, dataset)
    else
      result <- dataset

    if (counter >= fileCount)
      break
  }

  return(result)
}
