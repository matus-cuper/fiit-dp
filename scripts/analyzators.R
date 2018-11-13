analyzeFile <- function(pathToFile) {
  if (file.exists(pathToFile))
    return(analyzeDataset(loadDataset(pathToFile)))
}

# for (f in c("1000.csv", "1001.csv", "1002.csv", "1003.csv", "1004.csv", "1005.csv", "1006.csv", "1009.csv", "1013.csv", "1014.csv", "1015.csv")){
analyzeDataset <- function(dataset, windowStart = 1, windowEnd = freq * WEEK * 2, windowSize = freq * WEEK * 1, maxAnomalies = 0.1) {
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)

  while (windowEnd < nrow(dataset) && windowStart > 0) {
    result = AnomalyDetectionVec(
      dataset$load[windowStart:windowEnd],
      max_anoms = maxAnomalies,
      direction = "both",
      plot = TRUE,
      period = freq,
      longterm_period = freq * WEEK
    )

    plot(result$plot)
    Sys.sleep(0)

    input <- readline(prompt="Press [enter] to continue ")

    if (input == "n") {
      windowStart <- windowStart + windowSize
      windowEnd <- windowEnd + windowSize
      next
    }

    if (input == "p") {
      windowStart <- windowStart - windowSize
      windowEnd <- windowEnd - windowSize
      next
    }

    if (input == "+") {
      windowSize <- floor(windowSize / 2)
      freq <- floor(windowSize / 2)
      windowEnd <- windowStart + floor((windowEnd - windowStart) / 2)
      next
    }

    if (input == "-") {
      windowSize <- windowSize * 2
      freq <- windowSize * 2
      windowEnd <- windowStart + (windowEnd - windowStart) * 2
      next
    }

    if (input == "N" || input == "q" ) {
      break
    }

    windowStart <- windowStart + windowSize
    windowEnd <- windowEnd + windowSize
  }
}
