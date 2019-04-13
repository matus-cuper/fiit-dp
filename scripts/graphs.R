df.3778 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3788.csv", windowsCount = Inf)

orig <- df.3778$dataset$load[337:384]
instances <- c(1:(length(orig)/4))
instances <- instances * 4 - 2
samples <- orig[seq(2, length(orig), by = 4)]
groups <- data.frame(list(
  val = df.3778$dataset$load[337:384],
  group = rep(1:(length(orig) / 4), each = 4)
))
means <- round(aggregate(. ~ group, groups, mean)$val, 3)
medians <- round(aggregate(. ~ group, groups, median)$val, 3)
pips <- repr_pip(orig, times = length(orig) / 4 - 1)

d <- data.frame(instances, samples, means, medians, pips)
p <- plot_ly(d, x = ~instances, y = ~samples, name = 'Vzorkovanie dát', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~means, name = 'Priemer', mode = 'lines') %>%
  add_trace(y = ~medians, name = 'Medián', mode = 'lines') %>%
  add_trace(y = ~pips, name = 'PIP', mode = 'lines') %>%
  layout(xaxis = list(title = "Poradie meraní"), yaxis = list(title = "Hodnota"))
print(p)

inst <- c(1:length(orig))
d <- data.frame(inst, orig)
p <- plot_ly(d, x = ~inst, y = ~orig, name = 'Pôvodné dáta', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "Poradie meraní"), yaxis = list(title = "Hodnota"), showlegend = TRUE)
print(p)


df.3727 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3727.csv", windowsCount = Inf)
df.6998 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/6998.csv", windowsCount = Inf)
df.7422 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/7422.csv", windowsCount = Inf)
boxPlotData <- data.frame(
  "load" = c(df.3727$dataset$load, df.6998$dataset$load, df.7422$dataset$load),
  "consumer" = c(rep("3727", nrow(df.3727$dataset)), rep("6998", nrow(df.6998$dataset)), rep("7422", nrow(df.7422$dataset)))
)
p <- ggboxplot(boxPlotData, x = "consumer", y = "load", color = "consumer") +
      xlab("Identifikátor spotrebiteľa") + ylab("Spotreba elektrickej energie v kW") +
      theme(legend.position = "none")
print(p)
