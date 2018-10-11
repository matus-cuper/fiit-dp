loadDatasetFromDDC <- function(pathToFile) {
  train <- read.csv2(pathToFile, sep = ',')
  train <- read.csv2("~/r/fiit-dp/data/ddc/data/2.csv", sep = ',')
  
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

loadDataset <- function(pathToFile) {
  if (grepl("ddc", pathToFile)) {
    return(loadDatasetFromDDC(pathToFile))
  } else if (grepl("slovakia", pathToFile)) {
    return(loadDatasetFromSchool(pathToFile))
  } else if (grepl("ireland", pathToFile)) {
    return(loadDatasetFromSchool(pathToFile))
  }
}
