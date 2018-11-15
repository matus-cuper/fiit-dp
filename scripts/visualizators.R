# Visualize whole dataset in one plot with mean and median
visualizeDatasetStats <- function(dataset, windowTotalLength = 10, windowSize = 7, sleepTime = 0.0) {
  windowSize <- getFrequency(df) * windowSize * windowTotalLength

  plot(ts(dataset$load[1:windowSize]))
  for (i in 1:(nrow(dataset) / windowSize)) {
    lines(ts(dataset$load[((i * windowSize) + 1):((1 + i) * windowSize)]))
    Sys.sleep(sleepTime)
  }

  dataset <- groupByAddColumn(dataset, windowSize)
  mea <- aggregate(load ~ group, data = dataset, FUN = mean)
  med <- aggregate(load ~ group, data = dataset, FUN = median)

  lines(mea, col = "red", lwd = 2)
  lines(med, col = "green", lwd = 2)
}

# Visualize whole dataset in one plot with anomaly scores
visualizeDatasetAnomalies <- function(dataset) {
  df <- data.frame(list(
    timestamp = as.character(result$timestamp),
    load = dataset$load,
    score = dataset$score
  ))
  df$load[df$score <= 0] <- NA

  p <- plot_ly(df, x = ~timestamp, y = ~load, name = 'load', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~points, name = 'anomalies', mode = 'markers', color = ~score)

  print(p)
}
