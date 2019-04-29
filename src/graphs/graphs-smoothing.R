df.3727 <- data.frame(timestamp = IRELAND$timestamp, time = IRELAND$time, holiday = IRELAND$holiday, load = IRELAND$X3727.csv)
df.6998 <- data.frame(timestamp = IRELAND$timestamp, time = IRELAND$time, holiday = IRELAND$holiday, load = IRELAND$X6998.csv)
df.7422 <- data.frame(timestamp = IRELAND$timestamp, time = IRELAND$time, holiday = IRELAND$holiday, load = IRELAND$X7422.csv)

plotData <- data.frame(
  "id" = rep(1:48, length(filterWorkdays(filterWeekdays(df.3727))$load) / 48),
  "X3727" = filterWorkdays(filterWeekdays(df.3727))$load,
  "X6998" = filterWorkdays(filterWeekdays(df.6998))$load,
  "X7422" = filterWorkdays(filterWeekdays(df.7422))$load
)
plotDataAgg <- aggregate(. ~ id, data = plotData, mean)

p1 <- ggplot(plotDataAgg, aes(id - 1)) +
  scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
  geom_line(aes(y = X3727, color = hue_pal()(3)[2]), size = 1) +
  geom_line(aes(y = X6998, color = hue_pal()(3)[3]), size = 1) +
  geom_line(aes(y = X7422, color = hue_pal()(3)[1]), size = 1) +
  xlab("Identifikátor spotrebiteľa") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )


loess.3727 <- loess(plotDataAgg$X3727 ~ c(1:48), span = 0.25)
loess.6998 <- loess(plotDataAgg$X6998 ~ c(1:48), span = 0.25)
loess.7422 <- loess(plotDataAgg$X7422 ~ c(1:48), span = 0.25)
plotDataLoess <- data.frame(X3727 = predict(loess.3727), X6998 = predict(loess.6998), X7422 = predict(loess.7422))

p2 <- ggplot(plotDataLoess, aes(x = 1:48)) +
  geom_line(aes(y = X3727, color = hue_pal()(3)[2]), size = 1) +
  geom_line(aes(y = X6998, color = hue_pal()(3)[3]), size = 1) +
  geom_line(aes(y = X7422, color = hue_pal()(3)[1]), size = 1) +
  xlab("Identifikátor spotrebiteľa") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )

grid.arrange(p1, p2, ncol = 2)


df.1525 <- data.frame(timestamp = IRELAND$timestamp, load = IRELAND$X1525.csv)

df.range <- 97:336
twitter <- TWITTER_TS$X1525.csv$anom$anoms
twitter <- twitter[twitter$timestamp %in% df.1525$timestamp, ]
plotData <- merge(x = df.1525, y = twitter, by = "timestamp", all.x = TRUE)
plotData[!is.na(plotData$anoms), "anoms"] <- 1
plotData[is.na(plotData$anoms), "anoms"] <- 0

plotData <- plotData[df.range, ]
plotData$id <- 1:nrow(plotData)
loess10 <- loess(plotData$anoms ~ plotData$id, span = 0.10)
loess20 <- loess(plotData$anoms ~ plotData$id, span = 0.20)
loess30 <- loess(plotData$anoms ~ plotData$id, span = 0.30)

p3 <- ggplot(plotData, aes(x = id - 1)) +
  geom_line(aes(y = anoms, color = 1), size = 1) +
  xlab("Poradie meraní") + ylab("Príznak anomálnosti") + ggtitle("Pôvodné príznaky anomálií") +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.5), breaks = seq(0, 1, 0.5), limits = c(-0.3, 1.1)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.position = "none",
    legend.key = element_rect(colour = "transparent", fill = "white")
  )

p4 <- ggplot(plotData, aes(x = id - 1)) +
  geom_line(aes(y = predict(loess30), color = hue_pal()(3)[1]), size = 1) +
  geom_line(aes(y = predict(loess10), color = hue_pal()(3)[2]), size = 1) +
  geom_line(aes(y = predict(loess20), color = hue_pal()(3)[3]), size = 1) +
  xlab("Poradie meraní") + labs(color = "Rozpätie")  + ggtitle("Vyhladené príznaky anomálií") +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.5), breaks = seq(0, 1, 0.5), limits = c(-0.3, 1.1)) +
  scale_color_manual(labels = c("0.05", "0.10", "0.20"), values = c(hue_pal()(3)[1], hue_pal()(3)[2], hue_pal()(3)[3])) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    axis.title.y = element_blank()
  )

grid.arrange(p3, p4, ncol = 2)
