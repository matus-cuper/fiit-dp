loadDatasetFromDDC <- function(pathToFile) {
  train <- read.csv2(pathToFile, sep = ',')

  train$Values <- as.numeric(train$Values)
  train$Timestamp <- as.POSIXlt(train$Timestamp)
  train$Values[train$Values == 1] <- NA

  tmp <- data.frame(train$Values, index(1:nrow(train)))
  tmp <- na.approx(tmp, maxgap = 100)
  tmp <- data.frame(tmp)
  tmp$train.Values[is.na(tmp$train.Values)] <- -1

  train$Values <- tmp$train.Values
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

  if (!exists("train"))
    return(NULL)

  if (groupBy > 1) {
    train$id <- floor(c((1:nrow(train))-1) / groupBy)
    tmp <- aggregate(cbind(load) ~ id, data = train, sum)
    timestamps <- train$timestamp[seq(0 + floor(groupBy/2), nrow(train), groupBy)]
    train <- data.frame(timestamps, tmp$load)
  }

  colnames(train) <- c("timestamp", "load")
  return(train)
}
