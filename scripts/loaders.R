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
loadDatasetFromFile <- function(pathToFile) {
  pathToFile <- "~/r/fiit-dp/data/ireland/3788.csv"
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

  return(df)
}

# Create data.frame from whole directory, loads are stored as column
loadWholeDataset <- function(targetDirectory, weeks = 10, fileCount = Inf) {
  result <- data.frame()
  counter <- 0

  for (f in list.files(targetDirectory)) {
    # Load new file for processing
    pathToFile <- paste(targetDirectory, f, sep = "/")
    df <- loadDatasetFromFile(pathToFile)
    print(paste("Start processing", pathToFile))
    counter <- counter + 1

    # Filter rows and columns
    freq <- getFrequency(df)
    df <- df[1:(freq * WEEK * weeks), c("timestamp", "load")]
    colnames(df) <- c("timestamp", f)
    df$timestamp <- as.character(df$timestamp)

    # Merge dataframes
    if (length(result) > 0)
      result <- merge(result, df)
    else
      result <- df

    if (counter >= fileCount)
      break
  }

  return(result)
}
