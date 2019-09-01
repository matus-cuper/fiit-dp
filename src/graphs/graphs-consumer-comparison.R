df.3727 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3727.csv", windowsCount = Inf)
df.6998 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/6998.csv", windowsCount = Inf)
df.7422 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/7422.csv", windowsCount = Inf)

boxPlotData <- data.frame(
  "load" = c(df.3727$dataset$load, df.6998$dataset$load, df.7422$dataset$load),
  "consumer" = c(rep("3727", nrow(df.3727$dataset)), rep("6998", nrow(df.6998$dataset)), rep("7422", nrow(df.7422$dataset)))
)
p1 <- ggboxplot(boxPlotData, x = "consumer", y = "load", color = "consumer") +
  xlab("Consumer ID") + ylab("Electricity consumption in kW") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "black")
  )
plotData <- data.frame(
  "id" = rep(1:48, length(df.3727$dataset$load) / 48),
  "X3727" = df.3727$dataset$load,
  "X6998" = df.6998$dataset$load,
  "X7422" = df.7422$dataset$load
)
plotData <- aggregate(. ~ id, data = plotData, mean)
p2 <- ggplot(plotData, aes(id - 1)) +
  scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
  geom_line(aes(y = X3727, color = hue_pal()(3)[2]), size = 1) +
  geom_line(aes(y = X6998, color = hue_pal()(3)[3]), size = 1) +
  geom_line(aes(y = X7422, color = hue_pal()(3)[1]), size = 1) +
  xlab("Hour of day") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )
grid.arrange(p1, p2, ncol = 2)



boxPlotData <- data.frame(
  "load" = c(
    filterWorkdays(filterWeekdays(df.3727$dataset))$load,
    filterWorkdays(filterWeekdays(df.6998$dataset))$load,
    filterWorkdays(filterWeekdays(df.7422$dataset))$load
  ),
  "consumer" = c(
    rep("3727", nrow(filterWorkdays(filterWeekdays(df.3727$dataset)))),
    rep("6998", nrow(filterWorkdays(filterWeekdays(df.6998$dataset)))),
    rep("7422", nrow(filterWorkdays(filterWeekdays(df.7422$dataset))))
  )
)
p1 <- ggboxplot(boxPlotData, x = "consumer", y = "load", color = "consumer") +
  xlab("Identifikátor spotrebiteľa") + ylab("Spotreba elektrickej energie v kW") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "black")
  )
plotData <- data.frame(
  "id" = rep(1:48, length(filterWorkdays(filterWeekdays(df.3727$dataset))$load) / 48),
  "X3727" = filterWorkdays(filterWeekdays(df.3727$dataset))$load,
  "X6998" = filterWorkdays(filterWeekdays(df.6998$dataset))$load,
  "X7422" = filterWorkdays(filterWeekdays(df.7422$dataset))$load
)
plotData <- aggregate(. ~ id, data = plotData, mean)
p2 <- ggplot(plotData, aes(id - 1)) +
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
grid.arrange(p1, p2, ncol = 2)



boxPlotData <- data.frame(
  "load" = c(
    merge(filterHolidays(df.3727$dataset), filterWeekends(df.3727$dataset), all = TRUE)$load,
    merge(filterHolidays(df.6998$dataset), filterWeekends(df.6998$dataset), all = TRUE)$load,
    merge(filterHolidays(df.7422$dataset), filterWeekends(df.7422$dataset), all = TRUE)$load
  ),
  "consumer" = c(
    rep("3727", nrow(merge(filterHolidays(df.3727$dataset), filterWeekends(df.3727$dataset), all = TRUE))),
    rep("6998", nrow(merge(filterHolidays(df.6998$dataset), filterWeekends(df.6998$dataset), all = TRUE))),
    rep("7422", nrow(merge(filterHolidays(df.7422$dataset), filterWeekends(df.7422$dataset), all = TRUE)))
  )
)
p1 <- ggboxplot(boxPlotData, x = "consumer", y = "load", color = "consumer") +
  xlab("Identifikátor spotrebiteľa") + ylab("Spotreba elektrickej energie v kW") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "black")
  )
plotData <- data.frame(
  "id" = rep(1:48, length(merge(filterHolidays(df.3727$dataset), filterWeekends(df.3727$dataset), all = TRUE)$load) / 48),
  "X3727" = merge(filterHolidays(df.3727$dataset), filterWeekends(df.3727$dataset), all = TRUE)$load,
  "X6998" = merge(filterHolidays(df.6998$dataset), filterWeekends(df.6998$dataset), all = TRUE)$load,
  "X7422" = merge(filterHolidays(df.7422$dataset), filterWeekends(df.7422$dataset), all = TRUE)$load
)
plotData <- aggregate(. ~ id, data = plotData, mean)
p2 <- ggplot(plotData, aes(id - 1)) +
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
grid.arrange(p1, p2, ncol = 2)
