# Find anomalies for given dataset
findAnomalies <- function(dataset, windowSize = 7) {
  freq <- getFrequency(dataset)

  res <- AnomalyDetectionVec(
    dataset$load,
    max_anoms = 0.1,
    direction = 'both',
    period = freq,
    longterm_period = freq * windowSize
  )

  return(data.frame(list(
    index = res$anoms$index,
    value = res$anoms$anoms
  )))
}

# Find anomalies for different aggregations of data and append these scores to input dataset
findAnomaliesSumScore <- function(dataset, aggregations = c(0, 1, 2, 3, 4)) {
  freq <- getFrequency(dataset)
  anomalies <- data.frame(matrix(0, nrow = nrow(dataset), ncol = length(aggregations)))

  for (a in aggregations) {
    ratio <- 2**a
    anoms <- findAnomalies(groupByAggregate(dataset, ratio))
    anomalies[unlist(lapply(anoms$index, function(x) (x * 4) + c(1:ratio) )), a + 1] <- 1
  }

  anomalies$score <- apply(anomalies, 1, FUN = function(x) sum(x))
  anomalies$id <- c(1:nrow(anomalies))
  dataset$id <- c(1:nrow(dataset))

  result <- merge(dataset, anomalies, by = "id")
  result$id <- NULL

  return(result)
}

# Run single anomaly detection with different alpha and anomalities count
runAnomalyDetection <- function(dataset, numberOfAnomalies = c(0.1), alphaOfAnomalies = c(0.05),
                                windowsLength = 8, longtermPeriod = 5, measurementsPerDay = MPD) {

  oneWeekLength <- longtermPeriod * measurementsPerDay
  totalWeeks <- floor(nrow(dataset) / (oneWeekLength * windowsLength))

  result <- c()
  for (i in numberOfAnomalies) {
    for (j in alphaOfAnomalies) {
      for (week in 1:totalWeeks) {
        s <- (week - 1) * windowsLength * oneWeekLength + 1
        e <- week * windowsLength * oneWeekLength
        tryCatch({
          anomalies <- AnomalyDetectionVec(dataset$load[s:e], max_anoms = i, alpha = j, period = measurementsPerDay, longterm_period = oneWeekLength, plot = FALSE, direction = "both")
        }, error = function(e){cat("ERROR: Number of anomalies is too small\n")})
        result <- c(result, anomalies$anoms$index + s)
      }
    }
  }

  return(result)
}

# Run anomaly detection with different alpha, anomalities count and aggregation window
customAnomalyDetection <- function(dataset, aggregations = c(0, 1, 2, 3, 4), numberOfAnomalies = c(0.1), alphaOfAnomalies = c(0.05),
                                   windowsLength = 8, longtermPeriod = 5, measurementsPerDay = MPD) {

  anomalies <- data.frame(matrix(0, nrow = nrow(dataset), ncol = length(aggregations)))
  names(anomalies) <- sapply(aggregations, as.character)

  for (a in aggregations) {
    ratio <- 2**a
    anoms <- runAnomalyDetection(
      dataset = groupByAggregate(dataset, ratio),
      windowsLength = windowsLength,
      longtermPeriod = longtermPeriod,
      numberOfAnomalies = numberOfAnomalies,
      alphaOfAnomalies = alphaOfAnomalies,
      measurementsPerDay = measurementsPerDay / ratio
    )
    tanoms <- table(anoms)

    columns <- as.numeric(names(tanoms)) * ratio
    values <- c(1:ratio) - 1
    cartesian <- expand.grid(columns, values)

    canoms <- c(rep(tanoms, each = ratio))
    names(canoms) <- sort(cartesian$Var1 + cartesian$Var2)

    anomalies[names(canoms), as.character(a)] <- canoms
  }

  return(anomalies)
}

# Dummy wrapper for Twitter Anomaly Detection
anomalyDetectionWrapper <- function(suspiciousColumns, settings, computeHolidays = FALSE) {
  df <- list()
  if (computeHolidays) {
    for(s in suspiciousColumns) {
      tmp <- merge(filterHolidays(IRELAND[c(METADATACOLUMNS, s)]), filterWeekends(IRELAND[c(METADATACOLUMNS, s)]), all = TRUE)
      names(tmp) <- c(METADATACOLUMNS, "load")
      df[[s]] <- customAnomalyDetection(tmp, aggregations = settings$aggregations, numberOfAnomalies = settings$numberOfAnomalies, alphaOfAnomalies = settings$alphaOfAnomalies)
      print(paste(match(s, suspiciousColumns), "/", length(suspiciousColumns)))
    }
  }
  else {
    for(s in suspiciousColumns) {
      tmp <- filterWorkdays(IRELAND[c(METADATACOLUMNS, s)])
      names(tmp) <- c(METADATACOLUMNS, "load")
      df[[s]] <- customAnomalyDetection(tmp, aggregations = settings$aggregations, numberOfAnomalies = settings$numberOfAnomalies, alphaOfAnomalies = settings$alphaOfAnomalies)
      print(paste(match(s, suspiciousColumns), "/", length(suspiciousColumns)))
    }
  }

  return(df)
}
