cvi.1.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 1, clusterCount = 10, windowsCount = 10, fileCount = 100)
cvi.1.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 1, clusterCount = 15, windowsCount = 10, fileCount = 100)
cvi.1.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 1, clusterCount = 20, windowsCount = 10, fileCount = 100)
cvi.1.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 1, clusterCount = 25, windowsCount = 10, fileCount = 100)

cvi.2.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 10, windowsCount = 10, fileCount = 100)
cvi.2.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 15, windowsCount = 10, fileCount = 100)
cvi.2.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 20, windowsCount = 10, fileCount = 100)
cvi.2.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 25, windowsCount = 10, fileCount = 100)

cvi.3.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 10, windowsCount = 10, fileCount = 100)
cvi.3.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 15, windowsCount = 10, fileCount = 100)
cvi.3.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 20, windowsCount = 10, fileCount = 100)
cvi.3.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 25, windowsCount = 10, fileCount = 100)

cvi.4.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 10, windowsCount = 10, fileCount = 100)
cvi.4.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 15, windowsCount = 10, fileCount = 100)
cvi.4.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 20, windowsCount = 10, fileCount = 100)
cvi.4.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 25, windowsCount = 10, fileCount = 100)

cvi.5.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 10, windowsCount = 10, fileCount = 100)
cvi.5.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 15, windowsCount = 10, fileCount = 100)
cvi.5.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 20, windowsCount = 10, fileCount = 100)
cvi.5.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 25, windowsCount = 10, fileCount = 100)

cvi.6.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 6, clusterCount = 10, windowsCount = 10, fileCount = 100)
cvi.6.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 6, clusterCount = 15, windowsCount = 10, fileCount = 100)
cvi.6.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 6, clusterCount = 20, windowsCount = 10, fileCount = 100)
cvi.6.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 6, clusterCount = 25, windowsCount = 10, fileCount = 100)


load("~/r/fiit-dp/scripts/cvi-aggregated.RData")
tmp <- bind_rows(list(cvi.1.10 = cvi.1.10, cvi.1.15 = cvi.1.15, cvi.1.20 = cvi.1.20, cvi.1.25 = cvi.1.25, cvi.2.10 = cvi.2.10, cvi.2.15 = cvi.2.15, cvi.2.20 = cvi.2.20, cvi.2.25 = cvi.2.25, cvi.3.10 = cvi.3.10, cvi.3.15 = cvi.3.15, cvi.3.20 = cvi.3.20, cvi.3.25 = cvi.3.25, cvi.4.10 = cvi.4.10, cvi.4.15 = cvi.4.15, cvi.4.20 = cvi.4.20, cvi.4.25 = cvi.4.25, cvi.5.10 = cvi.5.10, cvi.5.15 = cvi.5.15, cvi.5.20 = cvi.5.20, cvi.5.25 = cvi.5.25, cvi.6.10 = cvi.6.10, cvi.6.15 = cvi.6.15, cvi.6.20 = cvi.6.20, cvi.6.25 = cvi.6.25), .id = 'var')
cvi.100 <- aggregate(. ~ var, tmp, mean)



t <- data.frame(list(
  val = cvi.100$CH,
  size = str_match(cvi.100$var, "cvi.([0-9])")[,2], clus = as.numeric(str_match(cvi.100$var, "cvi.[0-9].(.*)")[,2]) ))
legendtitle <- list(yref='paper', xref="paper", y=1.05, x=1.1, text="Veľkosť okna", showarrow=F)
p <- plot_ly(data = t, x = ~clus, y = ~val, color = ~size, marker = list(size = 10), mode = "markers+lines", type = "scatter") %>%
  layout(xaxis = list(title = "Počet zhlukov"), yaxis = list(title = "Hodnota indexu"), title = 'Calinski-Harabaszov index (max.)', showlegend = TRUE, annotations = legendtitle)
export(p, file = "cvi.100.ch.png")
