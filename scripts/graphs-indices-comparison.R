cvi.2.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 10, windowsCount = 10, fileCount = 500)
cvi.2.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 15, windowsCount = 10, fileCount = 500)
cvi.2.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 20, windowsCount = 10, fileCount = 500)
cvi.2.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 2, clusterCount = 25, windowsCount = 10, fileCount = 500)

cvi.3.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 10, windowsCount = 10, fileCount = 500)
cvi.3.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 15, windowsCount = 10, fileCount = 500)
cvi.3.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 20, windowsCount = 10, fileCount = 500)
cvi.3.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 3, clusterCount = 25, windowsCount = 10, fileCount = 500)

cvi.4.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 10, windowsCount = 10, fileCount = 500)
cvi.4.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 15, windowsCount = 10, fileCount = 500)
cvi.4.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 20, windowsCount = 10, fileCount = 500)
cvi.4.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 4, clusterCount = 25, windowsCount = 10, fileCount = 500)

cvi.5.10 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 10, windowsCount = 10, fileCount = 500)
cvi.5.15 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 15, windowsCount = 10, fileCount = 500)
cvi.5.20 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 20, windowsCount = 10, fileCount = 500)
cvi.5.25 <- loadIrelandDirectoryMovingWindow("~/r/fiit-dp/data/ireland", windowsSize = 5, clusterCount = 25, windowsCount = 10, fileCount = 500)


tmp <- bind_rows(list(cvi.2.10 = cvi.2.10, cvi.2.15 = cvi.2.15, cvi.2.20 = cvi.2.20, cvi.2.25 = cvi.2.25, cvi.3.10 = cvi.3.10, cvi.3.15 = cvi.3.15, cvi.3.20 = cvi.3.20, cvi.3.25 = cvi.3.25, cvi.4.10 = cvi.4.10, cvi.4.15 = cvi.4.15, cvi.4.20 = cvi.4.20, cvi.4.25 = cvi.4.25, cvi.5.10 = cvi.5.10, cvi.5.15 = cvi.5.15, cvi.5.20 = cvi.5.20, cvi.5.25 = cvi.5.25), .id = 'var')
# cvi.100 <- aggregate(. ~ var, tmp, mean)
# cvi.200 <- aggregate(. ~ var, tmp, mean)
cvi.500 <- aggregate(. ~ var, tmp, mean)


t <- data.frame(list(
  val = cvi.100$COP,
  size = str_match(cvi.100$var, "cvi.([0-9])")[,2],
  clus = as.numeric(str_match(cvi.100$var, "cvi.[0-9].(.*)")[,2])
))
t <- data.frame(list(
  val = cvi.200$COP,
  size = str_match(cvi.200$var, "cvi.([0-9])")[,2],
  clus = as.numeric(str_match(cvi.200$var, "cvi.[0-9].(.*)")[,2])
))
t <- data.frame(list(
  val = cvi.500$COP,
  size = str_match(cvi.500$var, "cvi.([0-9])")[,2],
  clus = as.numeric(str_match(cvi.500$var, "cvi.[0-9].(.*)")[,2])
))
legendtitle <- list(yref='paper', xref="paper", y=1.05, x=1.1, text="Veľkosť okna", showarrow=F)
p <- plot_ly(data = t, x = ~clus, y = ~val, color = ~size, marker = list(size = 10), mode = "markers+lines", type = "scatter") %>%
  layout(xaxis = list(title = "Počet zhlukov"), yaxis = list(title = "Hodnota indexu"), title = 'COP index (min.)', showlegend = TRUE, annotations = legendtitle)
print(p)

