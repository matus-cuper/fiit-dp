# Load number of rows from Ireland dataset, result is stored in same format as is read from CSV
loadIrelandDataset <- function(pathToFile, windowOffset = 0, windowsCount = 10) {
  df <- read.csv2(
    pathToFile,
    col.names = c("id", "timestamp", "date", "time", "day", "holiday", "dayId", "load"),
    nrows = MPD * WEEK * windowsCount + windowOffset,
    stringsAsFactors = FALSE
  )

  df$id <- NULL
  df$dayId <- NULL
  df <- df[(windowOffset + 1):min(MPD * WEEK * windowsCount + windowOffset, nrow(df)), ]
  df$load <- as.double(df$load)

  return(list(
    dataset = df,
    frequency = MPD
  ))
}

# Load number of rows from Ireland dataset in aggregated form
loadIrelandDatasetAggregated <- function(pathToFile, windowOffset = 0, windowsCount = 10, fun = mean, granularity = "weekly") {
  df <- loadIrelandDataset(pathToFile, windowOffset = windowOffset, windowsCount = windowsCount)

  if (granularity == "weekly") {
    return(aggregate(load ~ day + time, df$dataset, FUN = fun))
  } else if (granularity == "dailyWork") {
    return(aggregate(load ~ time, filterHolidays(filterWeekdays(df$dataset)), FUN = fun))
  } else {
    return(NULL)
  }
}

# Load number of rows from Ireland dataset directory in aggregated form
loadIrelandDirectory <- function(targetDirectory, windowOffset = 0, windowsCount = 10, fun = NULL, fileCount = Inf, granularity = "weekly") {
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
      df <- loadIrelandDatasetAggregated(paste(targetDirectory, filename, sep = "/"), windowOffset = windowOffset, windowsCount = windowsCount, fun = fun, granularity = granularity)

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

# Load number of rows from Ireland dataset directory in aggregated form
loadIrelandDirectoryMovingWindow <- function(targetDirectory,
                                             windowOffset = 0, windowsSize = 10, clusterCount = 10, windowsCount = 10,
                                             fun = mean, norm_fun = norm_z, fileCount = Inf, granularity = "weekly") {
  df <- data.frame()
  for (i in 0:windowsCount) {
    agg <- loadIrelandDirectory(targetDirectory = targetDirectory,
                                windowOffset = windowOffset + i,
                                windowsCount = windowsSize,
                                fun = fun,
                                fileCount = fileCount,
                                granularity = granularity
    )

    clu <- tsclust(
      t(data.frame(lapply(agg[, -1:-2], norm_fun))),
      k = clusterCount,
      distance = "dtw_basic",
      args = tsclust_args(dist = list(window.size = clusterCount))
    )
    cvi <- cvi(clu)
    df <- rbind(df, cvi)
  }
  names(df) <- names(cvi)

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
    irelandData <- loadIrelandDataset(pathToFile, windowOffset = windowOffset, windowsCount = windowTotalLength)
  } else
    return(NULL)

  # Filter rows
  irelandData$dataset <- irelandData$dataset[(windowOffset + 1):min(windowOffset + irelandData$frequency * windowSize * windowTotalLength, nrow(irelandData$dataset)), ]

  return(irelandData)
}
