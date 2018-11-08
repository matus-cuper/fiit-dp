visualizeDataset <- function(dataset, days = WEEK, sleepTime = 0.5) {
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
  datasetSize <- freq * days

  plot(ts(dataset$load[1:datasetSize]))
  for (i in 1:(nrow(dataset) / datasetSize)) {
    lines(ts(dataset$load[((i * datasetSize) + 1):((1 + i) * datasetSize)]))
    Sys.sleep(sleepTime)
  }

  dataset <- groupBy(dataset, datasetSize)
  mea <- aggregate(load ~ group, data = dataset, FUN = mean)
  med <- aggregate(load ~ group, data = dataset, FUN = median)
  lines(mea, col = "red", lwd = 2)
  lines(med, col = "green", lwd = 2)
}


visualizeDataset <- function(dataset, days = WEEK, sleepTime = 0.5) {
  dataset <- c
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
  datasetSize <- freq * days

  plot(ts(dataset$load[1:datasetSize]))

  res <- AnomalyDetectionVec(
    dataset$load[1:datasetSize],
    max_anoms=0.1,
    direction='both',
    plot=TRUE,
    period = freq,
    longterm_period = freq * WEEK
  )


  for (i in 1:(nrow(dataset) / datasetSize)) {
    lines(ts(dataset$load[((i * datasetSize) + 1):((1 + i) * datasetSize)]))
    Sys.sleep(sleepTime)
  }

  dataset <- groupBy(dataset, datasetSize)
  mea <- aggregate(load ~ group, data = dataset, FUN = mean)
  med <- aggregate(load ~ group, data = dataset, FUN = median)
  lines(mea, col = "red", lwd = 2)
  lines(med, col = "green", lwd = 2)
}
