findAnomaliesSumScore <- function(dataset, days = 4 * WEEK, aggregations = c(0, 1, 2, 3, 4), sleepTime = 0.0) {
  freq <- getFrequency(dataset)
  datasetSize <- freq * days

  dataset <- dataset[1:datasetSize, ]
  anomalies <- data.frame(matrix(0, nrow = nrow(dataset), ncol = length(aggregations)))

  for (a in aggregations) {
    ratio <- 2**a
    # TODO create new func
    train <- groupByAggregate(dataset, ratio)
    train <- train[1:(datasetSize / ratio), ]

    res <- AnomalyDetectionVec(
      train$load,
      max_anoms=0.1,
      direction='both',
      plot=TRUE,
      period = freq / ratio,
      longterm_period = freq * WEEK / ratio
    )

    print(paste("period", freq / ratio))
    print(paste("long period", freq * WEEK / ratio))

    plot(ts(train$load, frequency = freq / ratio))
    Sys.sleep(sleepTime)
    plot(res$plot)
    Sys.sleep(2*sleepTime)

    for (j in 1:ratio) {
      anomalies[res$anoms$index * ratio + j, a + 1] <- 1
    }
  }

  dataset$id <- c(1:nrow(dataset))
  anomalies$score <- apply(anomalies, 1, FUN = function(x) sum(x))
  anomalies$id <- c(1:nrow(anomalies))

  result <- merge(dataset, anomalies, by = "id")
  result$id <- NULL

  return(result)
}
