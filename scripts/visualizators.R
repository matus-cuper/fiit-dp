visualizeDataset <- function(dataset, days = WEEK, sleepTime = 0.5) {
  freq <- getFrequency(dataset)
  datasetSize <- freq * days

  plot(ts(dataset$load[1:datasetSize]))

  for (i in 1:(nrow(dataset) / datasetSize)) {
    lines(ts(dataset$load[((i * datasetSize) + 1):((1 + i) * datasetSize)]))
    Sys.sleep(sleepTime)
  }

  dataset <- groupByAddColumn(dataset, datasetSize)
  mea <- aggregate(load ~ group, data = dataset, FUN = mean)
  med <- aggregate(load ~ group, data = dataset, FUN = median)
  lines(mea, col = "red", lwd = 2)
  lines(med, col = "green", lwd = 2)
}

visualizeDatasetAnomalies <- function(dataset) {
  dataset$points <- dataset$load
  dataset$points[dataset$score <= 0] <- NA
  dataset$timestamp <- as.character(result$timestamp)

  p <- plot_ly(dataset, x = ~timestamp, y = ~load, name = 'load', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~points, name = 'anomalies', mode = 'markers', color = ~score)

  print(p)
}
