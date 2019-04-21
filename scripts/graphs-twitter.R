tw.raw <- as.data.frame(unlist(lapply(TWITTER_TS[DATACOLUMNS], function(x) x$score), use.names = FALSE))
names(tw.raw) <- c("score")
tw.raw$rownames <- DATACOLUMNS

tw.lambda <- 1.5
tw.c1 <- quantile(tw.raw$score, probs = 0.75) + tw.lambda * IQR(tw.raw$score)
tw.c2 <- quantile(tw.raw$score, probs = 0.25) - tw.lambda * IQR(tw.raw$score)

tw.result <- c(
  tw.raw[tw.raw$score > tw.c1, ]$rownames,
  tw.raw[tw.raw$score < tw.c2, ]$rownames
)

tw.anoms <- unique(sort(tw.result))


tw.feaclip <- lapply(IRELAND[DATACOLUMNS], function(x) repr_windowing(x, win_size = 48, func = repr_feaclip))
tw.colnames <- names(repr_feaclip(IRELAND$X1000.csv))

for (tw.col in DATACOLUMNS) {
  dim(tw.feaclip[[tw.col]]) <- c(length(tw.feaclip[[tw.col]]) / 8, 8)
  tw.feaclip[[tw.col]] <- as.data.frame(tw.feaclip[[tw.col]])
  names(tw.feaclip[[tw.col]]) <- tw.colnames
}

tw.fea <- list()
for (tw.col in DATACOLUMNS)
  tw.fea[[tw.col]] <- colMeans(tw.feaclip[[tw.col]])

{
  tw.dfea <- unlist(tw.fea, use.names = FALSE)
  dim(tw.dfea) <- c(length(DATACOLUMNS), 8)
  tw.dfea <- as.data.frame(tw.dfea)
  rownames(tw.dfea) <- names(tw.fea)
  names(tw.dfea) <- tw.colnames
}

{
  tw.groups <- as.data.frame(rep(2, length(DATACOLUMNS)))
  rownames(tw.groups) <- DATACOLUMNS
  tw.groups[tw.anoms, ] <- 1
  names(tw.groups) <- c("isAnomaly")
  tw.groups <- as.factor(tw.groups$isAnomaly)
}

{
  tw.tsne <- Rtsne(tw.dfea, verbose = TRUE, max_iter = 1000)
  tw.plot <- as.data.frame(tw.tsne$Y)
  names(tw.plot) <- c("X", "Y")
  tw.plot$groups <- tw.groups
}

tw.ts.tsne <- ggplot(tw.plot, aes(x = X, y = Y, colour = groups)) + geom_point() +
  xlab("TSNE1") + ylab("TSNE2") + labs(color = "Anomálnosť") +
  scale_color_manual(labels = c("áno", "nie"), values = c(hue_pal()(2)[1], hue_pal()(2)[2])) +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )


tw.pca <- prcomp(tw.dfea)
tw.ts.pca <- ggbiplot(tw.pca, groups = tw.groups, var.axes = FALSE) +
  xlab("PCA1") + ylab("PCA2") + labs(color = "Anomálnosť") +
  scale_color_manual(labels = c("áno", "nie"), values = c(hue_pal()(2)[1], hue_pal()(2)[2])) +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )





tw.raw <- as.data.frame(unlist(lapply(TWITTER_VEC[DATACOLUMNS], function(x) x$score), use.names = FALSE))
names(tw.raw) <- c("score")
tw.raw$rownames <- DATACOLUMNS

tw.lambda <- 1.5
tw.c1 <- quantile(tw.raw$score, probs = 0.75) + tw.lambda * IQR(tw.raw$score)
tw.c2 <- quantile(tw.raw$score, probs = 0.25) - tw.lambda * IQR(tw.raw$score)

tw.result <- c(
  tw.raw[tw.raw$score > tw.c1, ]$rownames,
  tw.raw[tw.raw$score < tw.c2, ]$rownames
)

tw.anoms <- unique(sort(tw.result))


tw.feaclip <- lapply(IRELAND[DATACOLUMNS], function(x) repr_windowing(x, win_size = 48, func = repr_feaclip))
tw.colnames <- names(repr_feaclip(IRELAND$X1000.csv))

for (tw.col in DATACOLUMNS) {
  dim(tw.feaclip[[tw.col]]) <- c(length(tw.feaclip[[tw.col]]) / 8, 8)
  tw.feaclip[[tw.col]] <- as.data.frame(tw.feaclip[[tw.col]])
  names(tw.feaclip[[tw.col]]) <- tw.colnames
}

tw.fea <- list()
for (tw.col in DATACOLUMNS)
  tw.fea[[tw.col]] <- colMeans(tw.feaclip[[tw.col]])

{
  tw.dfea <- unlist(tw.fea, use.names = FALSE)
  dim(tw.dfea) <- c(length(DATACOLUMNS), 8)
  tw.dfea <- as.data.frame(tw.dfea)
  rownames(tw.dfea) <- names(tw.fea)
  names(tw.dfea) <- tw.colnames
}

{
  tw.groups <- as.data.frame(rep(2, length(DATACOLUMNS)))
  rownames(tw.groups) <- DATACOLUMNS
  tw.groups[tw.anoms, ] <- 1
  names(tw.groups) <- c("isAnomaly")
  tw.groups <- as.factor(tw.groups$isAnomaly)
}

{
  tw.tsne <- Rtsne(tw.dfea, verbose = TRUE, max_iter = 1000)
  tw.plot <- as.data.frame(tw.tsne$Y)
  names(tw.plot) <- c("X", "Y")
  tw.plot$groups <- tw.groups
}

tw.vec.tsne <- ggplot(tw.plot, aes(x = X, y = Y, colour = groups)) + geom_point() +
  xlab("TSNE1") + ylab("TSNE2") + labs(color = "Anomálnosť") +
  scale_color_manual(labels = c("áno", "nie"), values = c(hue_pal()(2)[1], hue_pal()(2)[2])) +
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )


tw.pca <- prcomp(tw.dfea)
tw.vec.pca <- ggbiplot(tw.pca, groups = tw.groups, var.axes = FALSE) +
  xlab("PCA1") + ylab("PCA2") + labs(color = "Anomálnosť") +
  scale_color_manual(labels = c("áno", "nie"), values = c(hue_pal()(2)[1], hue_pal()(2)[2])) +
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )


tw.legend <- gtable_filter(ggplotGrob(tw.ts.tsne), "guide-box")

grid.arrange(arrangeGrob(tw.ts.tsne + theme(legend.position = "none"),
                         tw.ts.pca + theme(legend.position = "none"),
                         tw.vec.tsne + theme(legend.position = "none"),
                         tw.vec.pca + theme(legend.position = "none"),
                         nrow = 2),
             tw.legend,
             widths = unit.c(unit(1, "npc") - tw.legend$width, tw.legend$width),
             nrow = 1)
