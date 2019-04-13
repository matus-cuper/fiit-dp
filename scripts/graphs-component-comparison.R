# Draw plot of sample data and do time series decomposition

# preparedData <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3788.csv", windowsCount = 4)
preparedData <- IRELAND[1:4032, c(METADATACOLUMNS, "X3788.csv")]
names(preparedData) <- c(METADATACOLUMNS, "load")
ts <- as.data.frame(stl(ts(preparedData$load, frequency = 48), t.window = 336, s.window = "periodic", robust = TRUE)$time.series)

plotTimeSeries <- ts[1:1344, ]
matplot(plotTimeSeries, type = c("l"), col = 1:length(plotTimeSeries), ylab = "Hodnota", xlab = "Poradie meraní")

# matplot(plotTimeSeries$seasonal, type = c("l"), col = 1:length(plotTimeSeries$seasonal), ylab = "Hodnota", xlab = "Poradie meraní")
plotData <- data.frame(
  "load" = ts$seasonal[1:1344],
  "consumer" = 1:length(ts$seasonal[1:1344])
)
ggplot(plotData, aes(x = consumer, y = load)) + geom_line() +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") +
  scale_x_continuous(minor_breaks = seq(0 , nrow(plotData), 48), breaks = seq(0, nrow(plotData), 336)) +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 1),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )

plotData <- data.frame(
  "load" = preparedData$load[1:1344],
  "consumer" = 1:length(preparedData$load[1:1344])
)
ggplot(plotData, aes(x = consumer, y = load)) + geom_line() +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") +
  scale_x_continuous(minor_breaks = seq(0 , nrow(plotData), 48), breaks = seq(0, nrow(plotData), 336)) +
  scale_y_continuous(minor_breaks = seq(0 , 6, 1), breaks = seq(0, 6, 2)) +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 1),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )

# matplot(ts$trend, type = c("l"), col = 1:length(ts$trend), ylab = "Hodnota", xlab = "Poradie meraní")
plotData <- data.frame(
  "load" = ts$trend,
  "consumer" = 1:length(ts$trend)
)
ggplot(plotData, aes(x = consumer, y = load)) + geom_line() +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") +
  scale_x_continuous(minor_breaks = seq(0 , nrow(plotData), 336), breaks = seq(0, nrow(plotData), 336)) +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 1),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )

# matplot(ts$remainder, type = c("l"), col = 1:length(ts$remainder), ylab = "Hodnota", xlab = "Poradie meraní")
plotData <- data.frame(
  "load" = ts$remainder,
  "consumer" = 1:length(ts$remainder)
)
ggplot(plotData, aes(x = consumer, y = load)) + geom_line() +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") +
  scale_x_continuous(minor_breaks = seq(0 , nrow(plotData), 336), breaks = seq(0, nrow(plotData), 336)) +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 1),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )
