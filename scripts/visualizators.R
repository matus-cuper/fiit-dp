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
  dataset <- df.3778.stats
  df <- data.frame(list(
    timestamp = as.character(dataset$timestamp),
    load = dataset$load,
    points = dataset$load,
    score = dataset$score
  ), stringsAsFactors = FALSE)
  df$points[df$score <= 0] <- NA

  p <- plot_ly(df, x = ~timestamp, y = ~load, name = 'load', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~points, name = 'anomalies', mode = 'markers', color = ~score)

  print(p)
}

# Visualize CVI
visualizeCVIToFile <- function(df, graphName, pathToFile = "/tmp") {
  actualTime <- as.character(format(as.POSIXct(Sys.time()), "%Y%m%d%H%M"))
  legendTitle <- list(yref = "paper", xref = "paper", y = 1.05, x = 1.09, text = 'Veľkosť okna', showarrow = FALSE)

  plotData <- data.frame(list(
    Sil = df$Sil,
    CH = df$CH,
    DB = df$DB,
    DBstar = df$DBstar,
    D = df$D,
    COP = df$COP,
    wsize = str_match(df$var, "cvi.([0-9])")[,2],
    nclus = as.numeric(str_match(df$var, "cvi.[0-9].(.*)")[,2])
  ))

  graphTitles <- list(
    Sil = 'Silhouettov index (max.)',
    CH = 'Calinski-Harabaszov index (max.)',
    DB = 'Davies-Bouldinov index (min.)',
    DBstar = 'Modifikovaný Davies-Bouldinov index (min.)',
    D = 'Dunnov index (max.)',
    COP = 'COP index (min.)'
  )

  for(index in names(graphTitles)) {
    tmp <- plotData[c(index, "wsize", "nclus")]
    names(tmp) <- c("val", "wsize", "nclus")

    plot <- plot_ly(data = tmp, x = ~nclus, y = ~val, color = ~wsize, marker = list(size = 10), mode = "markers+lines", type = "scatter") %>%
      layout(xaxis = list(title = 'Počet zhlukov'), yaxis = list(title = 'Hodnota indexu'), title = graphTitles[index], showlegend = TRUE, annotations = legendTitle)
    plotly_IMAGE(plot, format = "png", out_file = paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''))
  }
}
