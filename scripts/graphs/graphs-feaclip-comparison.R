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


plotDataFea <- data.frame(lapply(plotDataAgg[, c("X3727", "X6998", "X7422")], repr_feaclip))

p2 <- ggplot(plotDataFea, aes(x = 1:8)) +
  scale_x_continuous(breaks = seq(1, 8, 1), labels = rownames(plotDataFea)) +
  scale_y_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12), limits = c(0, 36)) +
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
