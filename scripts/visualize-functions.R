# Visualize subresults of twitter
visualizeTwitterTS <- function(consumerID, resultTS, from = 0, to = 960) {
  df <- IRELAND[, c('timestamp', consumerID)]
  df$timestamp <- as.POSIXlt(df$timestamp)

  df$anomaly <- 'no'
  df$anomaly[df$timestamp %in% as.POSIXlt(resultTS)] <- 'ts'
  names(df) <- c("timestamp", "load", "anomaly")

  p <- ggplot(df[from:to, ], aes(x = as.POSIXct(timestamp))) +
    geom_line(aes(y = load)) +
    geom_point(aes(y = load, color = anomaly)) +
    scale_color_manual(values=c("#000000", "#FF0000"))
  print(p)
}

# Visualize results of twitter anomaly detection package
visualizeTwitterResults <- function(consumerID, from = 0, to = 960) {
  df <- IRELAND[, c('timestamp', consumerID)]
  df$timestamp <- as.POSIXlt(df$timestamp)

  df$anomaly <- 'no'
  df$anomaly[df$timestamp %in% TWITTER_TS[[consumerID]]$anom$anoms$timestamp] <- 'ts'
  df$anomaly[TWITTER_VEC[[consumerID]]$anom$anoms$index %in% which(df$anomaly == 'ts')] <- 'both'
  df$anomaly[TWITTER_VEC[[consumerID]]$anom$anoms$index %in% which(df$anomaly == 'no')] <- 'vec'
  names(df) <- c("timestamp", "load", "anomaly")

  p <- ggplot(df[from:to, ], aes(x = as.POSIXct(timestamp))) +
    geom_line(aes(y = load)) +
    geom_point(aes(y = load, color = anomaly)) +
    scale_color_manual(values=c("#FF0000", "#000000", "#FF0FF0", "#FFFF00"))
  print(p)
  Sys.sleep(2)
}

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

    plot <- ggplot(tmp, aes(y = val, color = wsize)) + geom_line(aes(x = nclus), size = 1) + geom_point(aes(x = nclus), size = 2.5) +
      scale_x_continuous(minor_breaks = seq(15, 25, 2.5), breaks = seq(15, 25, 5)) +
      xlab("Počet zhlukov") + ylab("Hodnota indexu") + labs(color = "Veľkosť okna") + ggtitle(graphTitles[[index]]) +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black"),
        legend.key = element_rect(colour = "transparent", fill = "white")
      )

    print(plot)
    Sys.sleep(2)
    if (save)
      ggsave(paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''), plot, width = 9, height = 4)
  }
}

# Visualize CVI with same metric
visualizeCVIDiffMetric <- function(df, graphName, pathToFile = "/tmp", save = FALSE) {
  actualTime <- as.character(format(as.POSIXct(Sys.time()), "%Y%m%d%H%M"))

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
    tmp[tmp$val == Inf, "val"] <- NaN

    plot <- ggplot(tmp, aes(y = val, fill = wsize)) + geom_bar(aes(x = metric), stat = "identity", position=position_dodge()) +
      xlab("Počet zhlukov") + ylab("Hodnota indexu") + labs(fill = "Veľkosť okna") + ggtitle(graphTitles[[index]]) +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black"),
        legend.key = element_rect(colour = "transparent", fill = "white")
      )

    print(plot)
    Sys.sleep(2)
    if (save)
      ggsave(paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''), plot, width = 9, height = 3.5)
  }
}

# Visualize CVI with diff window
visualizeCVIDiffWindow <- function(df, graphName, pathToFile = "/tmp", save = FALSE) {
  actualTime <- as.character(format(as.POSIXct(Sys.time()), "%Y%m%d%H%M"))

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

    plot <- ggplot(tmp, aes(y = val, fill = metric)) + geom_bar(aes(x = metric), stat = "identity", position=position_dodge()) +
      xlab("Počet zhlukov") + ylab("Hodnota indexu") + labs(fill = "Veľkosť okna") + ggtitle(graphTitles[[index]]) +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black"),
        legend.key = element_rect(colour = "transparent", fill = "white")
      )

    print(plot)
    Sys.sleep(2)
    if (save)
      ggsave(paste(pathToFile, '/', actualTime,  '-', index, '-', graphName, '.png', sep = ''), plot, width = 9, height = 3.5)
  }
}
