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
