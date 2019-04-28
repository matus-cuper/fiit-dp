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
p <- ggplot(d, aes(instances - 1)) +
  geom_line(aes(y = medians, color = hue_pal()(5)[2]), size = 1) +
  geom_line(aes(y = means, color = hue_pal()(5)[3]), size = 1) +
  geom_line(aes(y = samples, color = hue_pal()(5)[4]), size = 1) +
  geom_line(aes(y = pips, color = hue_pal()(5)[5]), size = 1) +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Typ redukcie") +
  scale_color_manual(labels = c("Vzorkovanie", "Priemer", "Medián", "PIP"), values = c(hue_pal()(5)[2], hue_pal()(5)[3], hue_pal()(5)[4], hue_pal()(5)[5])) +
  scale_y_continuous(minor_breaks = seq(0, 5, 1), breaks = seq(0, 5, 1)) +
  scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )
print(p)

inst <- c(1:length(orig))
d <- data.frame(inst, orig)
p <- ggplot(d, aes(inst - 1)) +
  geom_line(aes(y = orig, color = hue_pal()(1)), size = 1) +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") +
  scale_y_continuous(minor_breaks = seq(0, 5, 1), breaks = seq(0, 5, 1)) +
  scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )
print(p)
