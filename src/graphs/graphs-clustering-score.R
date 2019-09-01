n <- 4
# centroids <- data.frame(
#   mux = runif(n, 5, 25) - runif(n, 0, 10),
#   muy = runif(n, 5, 25) - runif(n, 0, 10),
#   sdx = runif(n, 1, 3),
#   sdy = runif(n, 1, 3),
#   n = c(round(runif(n - 1, 4, 7)^2), 4)
# )
#
# df <- data.frame(
#   x = unlist(apply(centroids, 1, function(x) rnorm(x["n"], x["mux"], x["sdx"])), use.names = FALSE),
#   y = unlist(apply(centroids, 1, function(x) rnorm(x["n"], x["muy"], x["sdy"])), use.names = FALSE),
#   groups = as.factor(rep(1:nrow(centroids), centroids$n))
# )
#
# save(df, file = "./graphs-clustering-score.rda")
load(file = "r/fiit-dp/src/graphs/graphs-clustering-score.rda")
load(file = "./graphs-clustering-score.rda")

#
df[df$y > 10, 'y'] <- df[df$y > 10, 'y'] - 10

df.centroids <- aggregate(. ~ groups, df, mean)
df.centroids$label <- c("Major cluster", "Major cluster", "Minor cluster")

ggplot(df, aes(x = x - 6, y = y - 10, colour = groups)) +
  xlab("PCA1") + ylab("PCA2") + labs(color = "ID zhluku") +
  geom_point(size = 2.5) +
  scale_color_manual(labels = c("1", "2", "3"), values = c(hue_pal()(3)[1], hue_pal()(3)[2], hue_pal()(3)[3])) +
  geom_segment(x = df[106, "x"] - 6, xend = df[107, "x"] - 6, y = df[106, "y"] - 10, yend = df[107, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_segment(x = df[106, "x"] - 6, xend = df[108, "x"] - 6, y = df[106, "y"] - 10, yend = df[108, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_segment(x = df[106, "x"] - 6, xend = df[109, "x"] - 6, y = df[106, "y"] - 10, yend = df[109, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_segment(x = df[43, "x"] - 6, xend = df[36, "x"] - 6, y = df[43, "y"] - 10, yend = df[36, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_segment(x = df[43, "x"] - 6, xend = df[52, "x"] - 6, y = df[43, "y"] - 10, yend = df[52, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_segment(x = df[43, "x"] - 6, xend = df[54, "x"] - 6, y = df[43, "y"] - 10, yend = df[54, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_segment(x = df[43, "x"] - 6, xend = df[46, "x"] - 6, y = df[43, "y"] - 10, yend = df[46, "y"] - 10, colour = "#777777", arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 20), size = 0.2) +
  geom_point(size = 2) +
  geom_label(aes(x = df.centroids$x[1] - 8, y = df.centroids$y[1] - 11, label = df.centroids$label[1], size = 3)) +
  geom_label(aes(x = df.centroids$x[2] - 8, y = df.centroids$y[2] - 11, label = df.centroids$label[2], size = 3)) +
  geom_label(aes(x = df.centroids$x[3] - 8, y = df.centroids$y[3] - 11, label = df.centroids$label[3], size = 3)) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.position = "none"
  )
