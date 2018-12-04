# Draw plot of sample data and do time series decomposition

preparedData <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3788.csv", windowsCount = 4)
ts <- as.data.frame(stl(ts(preparedData$dataset$load, frequency = 48), t.window = 336, s.window = "periodic", robust = TRUE)$time.series)

matplot(ts, type = c("l"), col = 1:length(ts), ylab = "Hodnota", xlab = "Poradie meraní")

matplot(preparedData$dataset$load, type = c("l"), col = 1:length(ts$seasonal), ylab = "Hodnota", xlab = "Poradie meraní")
matplot(ts$seasonal, type = c("l"), col = 1:length(ts$seasonal), ylab = "Hodnota", xlab = "Poradie meraní")


preparedData <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3788.csv", windowsCount = 12)
ts <- as.data.frame(stl(ts(preparedData$dataset$load, frequency = 48), t.window = 336, s.window = "periodic", robust = TRUE)$time.series)

matplot(ts$trend, type = c("l"), col = 1:length(ts$trend), ylab = "Hodnota", xlab = "Poradie meraní")
matplot(ts$remainder, type = c("l"), col = 1:length(ts$remainder), ylab = "Hodnota", xlab = "Poradie meraní")
