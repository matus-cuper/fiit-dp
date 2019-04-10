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

# Visualize CVI with same metric
visualizeCVISameMetric <- function(df, graphName, pathToFile = "/tmp", save = FALSE) {
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
    DBstar = 'Modifikovaný Davies-Bouldinov index (min.)'
    # ,
    # D = 'Dunnov index (max.)',
    # COP = 'COP index (min.)'
  )

  for(index in names(graphTitles)) {
    tmp <- plotData[c(index, "wsize", "nclus")]
    names(tmp) <- c("val", "wsize", "nclus")
    plot <- plot_ly(data = tmp, x = ~nclus, y = ~val, color = ~wsize, marker = list(size = 10), mode = "markers+lines", type = "scatter") %>%
      layout(xaxis = list(title = 'Počet zhlukov', dtick = 5), yaxis = list(title = 'Hodnota indexu'), title = graphTitles[[index]], showlegend = TRUE, annotations = legendTitle)
    print(plot)
    Sys.sleep(3)
    if (save)
      plotly_IMAGE(plot, format = "png", out_file = paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''))
  }
}

# Visualize CVI with same metric
visualizeCVIDiffMetric <- function(df, graphName, pathToFile = "/tmp", save = FALSE) {
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
    nclus = as.numeric(str_match(df$var, "cvi.[0-9].([0-9]*).*")[,2]),
    metric = str_match(df$var, "cvi.[0-9].[0-9]0.(.*)")[,2]
  ))

  graphTitles <- list(
    Sil = 'Silhouettov index (max.)',
    CH = 'Calinski-Harabaszov index (max.)',
    DB = 'Davies-Bouldinov index (min.)',
    DBstar = 'Modifikovaný Davies-Bouldinov index (min.)'
    # ,
    # D = 'Dunnov index (max.)',
    # COP = 'COP index (min.)'
  )

  for(index in names(graphTitles)) {
    tmp <- plotData[c(index, "wsize", "metric")]
    names(tmp) <- c("val", "wsize", "metric")
    plot <- plot_ly(data = tmp, x = ~metric, y = ~val, color = ~wsize, type = "bar") %>%
      layout(xaxis = list(title = 'Použitá metrika'), yaxis = list(title = 'Hodnota indexu'), title = graphTitles[[index]], showlegend = TRUE, annotations = legendTitle)
    print(plot)
    Sys.sleep(3)
    if (save)
      plotly_IMAGE(plot, format = "png", out_file = paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''))
  }
}

# Visualize CVI with diff window
visualizeCVIDiffWindow <- function(df, graphName, pathToFile = "/tmp", save = FALSE) {
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
    nclus = as.numeric(str_match(df$var, "cvi.[0-9].([0-9]*).*")[,2]),
    metric = str_match(df$var, "cvi.[0-9].[0-9]0.(.*)")[,2]
  ))

  barTitles <- data.frame(list(
    weeks = "Týždne, krok po týždni",
    workdays = "Pracovné dni, krok po týždni",
    workdaysByDay = "Pracovné dni, krok po dni"
  ))

  plotData$metric <- as.character(plotData$metric)

  plotData$metric <- gsub("weeks", barTitles[["weeks"]], plotData$metric)
  plotData$metric <- gsub("workdaysByDay", barTitles[["workdaysByDay"]], plotData$metric)
  plotData$metric <- gsub("workdays", barTitles[["workdays"]], plotData$metric)

  graphTitles <- list(
    Sil = 'Silhouettov index (max.)',
    CH = 'Calinski-Harabaszov index (max.)',
    DB = 'Davies-Bouldinov index (min.)',
    DBstar = 'Modifikovaný Davies-Bouldinov index (min.)'
    # ,
    # D = 'Dunnov index (max.)',
    # COP = 'COP index (min.)'
  )

  for(index in names(graphTitles)) {
    tmp <- plotData[c(index, "metric")]
    names(tmp) <- c("val", "metric")
    tmp$index <- index
    plot <- plot_ly(data = tmp, x = ~metric, y = ~val, color = ~metric, type = "bar") %>%
      layout(xaxis = list(title = 'Použitá metrika'), yaxis = list(title = 'Hodnota indexu'), title = graphTitles[[index]], showlegend = TRUE, annotations = legendTitle)
    print(plot)
    Sys.sleep(3)
    if (save)
      plotly_IMAGE(plot, format = "png", out_file = paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''))
  }
}
