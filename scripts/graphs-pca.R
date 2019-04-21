df <- data.frame(
  x = rnorm(50, 5, 4),
  y = rnorm(50, 2, 2)
)
p1 <- ggplot(df, aes(x = x, y = y)) + geom_point() +
  xlab("dimenzia X") + ylab("dimenzia Y") +
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )

df.pca <- prcomp(df)
p2 <- ggplot(as.data.frame(df.pca$x), aes(x = df.pca$x[, 1], y = c(0))) + geom_point() +
  xlab("PCA1") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )

ggarrange(p1, p2)
