# Load number of rows from Ireland dataset
loadIrelandDataset <- function(pathToFile, windowOffset = 0, windowsCount = 10) {
  df <- read.csv2(
    pathToFile,
    col.names = c("id", "timestamp", "date", "time", "day", "holiday", "dayId", "load"),
    nrows = 48 * 7 * windowsCount + windowOffset,
    stringsAsFactors = FALSE
  )

  df$id <- NULL
  df$dayId <- NULL
  df <- df[(windowOffset + 1):min(48 * 7 * windowsCount + windowOffset, nrow(df)), ]
  df$load <- as.double(df$load)

  return(list(
    dataset = df,
    frequency = 48
  ))
}

# Load number of rows from Ireland dataset in aggregated form
loadIrelandDatasetAggregated <- function(pathToFile, windowOffset = 0, windowsCount = 10, fun = mean) {
  df <- loadIrelandDataset(pathToFile, windowOffset = windowOffset, windowsCount = windowsCount)

  return(aggregate(load ~ day + time, df$dataset, FUN = fun))
}

# Load number of rows from Ireland dataset directory in aggregated form
loadIrelandDirectory <- function(targetDirectory, windowOffset = 0, windowsCount = 10, fun = NULL, fileCount = Inf) {
  counter <- 0
  result <- data.frame()
  allFiles <- list.files(targetDirectory)
  bar <- txtProgressBar(min = 0, max = min(fileCount, length(allFiles)), style = 3)

  for (filename in allFiles) {
    # Load new file for processing
    if (is.null(fun)) {
      df <- loadIrelandDataset(paste(targetDirectory, filename, sep = "/"), windowOffset = windowOffset, windowsCount = windowsCount)
      df <- df$dataset
    }
    else
      df <- loadIrelandDatasetAggregated(paste(targetDirectory, filename, sep = "/"), windowOffset = windowOffset, windowsCount = windowsCount, fun = fun)

    names(df)[names(df) == "load"] <- filename

    # Merge dataframes
    if (length(result) > 0)
      result <- merge(result, df)
    else
      result <- df

    # Utilities such as progress bar and file counter
    counter <- counter + 1
    setTxtProgressBar(bar, counter)
    if (counter >= fileCount)
      break
  }

  close(bar)
  return(result)
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
    irelandData <- loadIrelandDataset(pathToFile, windowOffset = windowOffset, windowsCount = windowTotalLength)
  } else
    return(NULL)

  # Filter rows
  irelandData$dataset <- irelandData$dataset[(windowOffset + 1):min(windowOffset + irelandData$frequency * windowSize * windowTotalLength, nrow(irelandData$dataset)), ]

  return(irelandData)
}
