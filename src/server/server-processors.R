# Prepare dataset
IRELAND <- loadIrelandIntoDF("~/data/load/")
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

IRELAND <- read.csv2("~/data/ireland.csv")
WEEKS <- round(nrow(IRELAND) / (MPD * WEEK))


# IRELAND <- loadIrelandIntoDF("~/data/load/")
loadIrelandIntoDF <- function(directory) {
  result <- data.frame()
  for (f in list.files(directory)) {
    df <- read.csv2(
      paste(directory, f, sep = ""),
      col.names = c("id", "timestamp", "date", "time", "day", "holiday", "dayId", f),
      stringsAsFactors = FALSE
    )

    if (nrow(result) == 0)
      result <- df
    else
      result <- merge.data.frame(x = result, y = df)
    print(paste("Processed file", f))
  }

  return(result)
}


movingWindowWeeks <- function(windowSize = 4, clusterCount = 20, distanceMetric = "dtw_basic") {
  result <- data.frame()
  bar <- txtProgressBar(min = 0, max = WEEKS - windowSize, style = 3)
  counter <- 0

  for (week in seq(1, WEEKS - windowSize)) {
    s <- (week - 1) * (MPD * WEEK) + 1
    e <- WEEK * MPD * (week + windowSize - 1)
    tmp <- IRELAND[s:e, COLUMNS]

    result <- rbind(
      result,
      cvi(tsclust(
        t(data.frame(lapply(aggregate(. ~ time + day, tmp[c("time", "day", DATACOLUMNS)], FUN = mean), norm_z))),
        k = clusterCount,
        distance = distanceMetric,
        args = tsclust_args(dist = list(window.size = clusterCount))
      ))
    )

    # Utilities such as progress bar and file counter
    counter <- counter + 1
    setTxtProgressBar(bar, counter)
  }
  names(result) <- c("Sil", "SF", "CH", "DB", "DBstar", "D", "COP")

  close(bar)
  return(result)
}

movingWindowWorkdays <- function(windowSize = 4, clusterCount = 20, distanceMetric = "dtw_basic") {
  result <- data.frame()
  bar <- txtProgressBar(min = 0, max = WEEKS - windowSize, style = 3)
  counter <- 0

  for (week in seq(1, WEEKS - windowSize)) {
    s <- (week - 1) * (MPD * WEEK) + 1
    e <- WEEK * MPD * (week + windowSize - 1)
    tmp <- filterHolidays(filterWeekdays(IRELAND[s:e, COLUMNS]))

    result <- rbind(
      result,
      cvi(tsclust(
        t(data.frame(lapply(aggregate(. ~ time, tmp[c("time", DATACOLUMNS)], FUN = mean), norm_z))),
        k = clusterCount,
        distance = distanceMetric,
        args = tsclust_args(dist = list(window.size = clusterCount))
      ))
    )

    # Utilities such as progress bar and file counter
    counter <- counter + 1
    setTxtProgressBar(bar, counter)
  }
  names(result) <- c("Sil", "SF", "CH", "DB", "DBstar", "D", "COP")

  close(bar)
  return(result)
}


movingWindowWorkdaysParallel <- function(windowSize = 4, clusterCount = 20, distanceMetric = "dtw_basic") {
  result <- foreach(week = seq(1, WEEKS - windowSize)) %dopar% {
    s <- (week - 1) * (MPD * WEEK) + 1
    e <- WEEK * MPD * (week + windowSize - 1)
    tmp <- filterHolidays(filterWeekdays(IRELAND[s:e, COLUMNS]))

    cvi(tsclust(
      t(data.frame(lapply(aggregate(. ~ time, tmp[c("time", DATACOLUMNS)], FUN = mean), norm_z))),
      k = clusterCount,
      distance = distanceMetric,
      args = tsclust_args(dist = list(window.size = clusterCount))
    ))
  }
}


movingWindowWorkdaysByDay <- function(windowSize = 4, clusterCount = 20, distanceMetric = "dtw_basic") {
  result <- data.frame()

  relevantDays <- filterHolidays(filterWeekdays(IRELAND))
  bar <- txtProgressBar(min = 0, max = (nrow(relevantDays) - windowSize * 5 * MPD) / MPD, style = 3)
  counter <- 0

  for (day in seq(1, (nrow(relevantDays) - windowSize * 5 * MPD) / MPD)) {
    s <- (day - 1) * MPD + 1
    e <- MPD * (day + (windowSize * 5) - 1)
    tmp <- relevantDays[s:e, COLUMNS]

    result <- rbind(
      result,
      cvi(tsclust(
        t(data.frame(lapply(aggregate(. ~ time, tmp[c("time", DATACOLUMNS)], FUN = mean), norm_z))),
        k = clusterCount,
        distance = distanceMetric,
        args = tsclust_args(dist = list(window.size = clusterCount))
      ))
    )

    # Utilities such as progress bar and file counter
    counter <- counter + 1
    setTxtProgressBar(bar, counter)
  }
  names(result) <- c("Sil", "SF", "CH", "DB", "DBstar", "D", "COP")

  close(bar)
  return(result)
}
