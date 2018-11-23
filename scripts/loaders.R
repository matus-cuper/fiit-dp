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

# Load in this kind of time series constatnly grow, two data points in neighbourhood are subtracted
loadDatasetFromDDCSummarized <- function(pathToFile) {
  train <- loadDatasetFromDDC(pathToFile)
  train$load <- diff(train$load)[1:nrow(train)]

  return(train)
}

loadDatasetFromSchool <- function(pathToFile) {
  train <- read.csv2(pathToFile)
  # TODO: add nrows

  df <- data.frame(list(
    timestamp = as.POSIXlt(train$dateTime),
    load = as.double(as.character(train$load)),
    date = train$date,
    time = train$time,
    day = train$dayOfWeek,
    holiday = train$holiday
  ))

  return(df)
}

# Create data.frame from given file
loadDatasetFromFile <- function(pathToFile, windowOffset = 0, windowTotalLength = 10, windowSize = 7) {
  if (!file.exists(pathToFile))
    return(NULL)
  
  if (grepl("electricity", pathToFile)) {
    df <- loadDatasetFromDDC(pathToFile)
  } else if (grepl("summarized", pathToFile)) {
    df <- loadDatasetFromDDCSummarized(pathToFile)
  } else if (grepl("slovakia", pathToFile)) {
    df <- loadDatasetFromSchool(pathToFile)
  } else if (grepl("ireland", pathToFile)) {
    df <- loadDatasetFromSchool(pathToFile)
  }

  if (!exists("df"))
    return(NULL)

  # Remove NA values
  df <- df[!is.na(df$load), ]

  # Create temporary dataset
  agg <- aggregate(load ~ date, data = df, FUN = length)
  agg$date <- strptime(agg$date, "%Y-%m-%d")
  agg$count <- agg$load

  # Find day period and remove uncomplete days
  freq <- getFrequency(df)
  df <- df[df$date %in% agg[freq == agg$count,]$date, ]

  # Filter rows and columns
  df <- df[(windowOffset + 1):min(windowOffset + freq * windowSize * windowTotalLength, nrow(df)), ]

  return(df)
}

# Create data.frame from whole directory, loads are stored as column
loadWholeDataset <- function(targetDirectory, windowOffset = 0, windowTotalLength = 10, windowSize = 7, fileCount = Inf) {
  result <- data.frame()
  pb <- txtProgressBar(min = 0, max = fileCount, style = 3)
  counter <- 0

  for (f in list.files(targetDirectory)) {
    # Load new file for processing
    pathToFile <- paste(targetDirectory, f, sep = "/")
    df <- loadDatasetFromFile(pathToFile, windowOffset = windowOffset, windowTotalLength = windowTotalLength, windowSize = windowSize)

    setTxtProgressBar(pb, counter)
    counter <- counter + 1

    # Filter rows and columns
    df <- data.frame(list(
      timestamp = as.character(df$timestamp),
      load = df$load
    ))
    colnames(df) <- c("timestamp", f)

    # Merge dataframes
    if (length(result) > 0)
      result <- merge(result, df)
    else
      result <- df

    if (counter >= fileCount)
      break
  }

  close(pb)
  return(result)
}
