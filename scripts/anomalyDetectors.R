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

# TODO: Add visualization of these process if it is really needed
