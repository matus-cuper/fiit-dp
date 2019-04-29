my.names <- names(repr_feaclip(IRELAND$X3778.csv))

my.raw.fea <- lapply(IRELAND[DATACOLUMNS], repr_feaclip)

my.raw.df <- unlist(my.raw.fea, use.names = FALSE)
dim(my.raw.df) <- c(length(DATACOLUMNS), 8)
my.raw.df <- data.frame(my.raw.df)
names(my.raw.df) <- my.names
rownames(my.raw.df) <- DATACOLUMNS

my.raw.sum.iqr <- getIQRrule(my.raw.df$sum_1)
my.raw.cro.iqr <- getIQRrule(my.raw.df$cross.)

my.raw.groups <- ((my.raw.df$sum_1 %in% my.raw.sum.iqr) | (my.raw.df$cross. %in% my.raw.cro.iqr))

my.raw.tsne <- Rtsne(my.raw.df, verbose = TRUE, max_iter = 500)
my.raw.tsne.plotData <- as.data.frame(my.raw.tsne$Y)
names(my.raw.tsne.plotData) <- c("X", "Y")
my.raw.tsne.plotData$groups <- my.raw.groups


my.raw.tsne.plot <- ggplot(my.raw.tsne.plotData, aes(x = X, y = Y, colour = groups)) + geom_point() +
  xlab("TSNE1") + ylab("TSNE2") + labs(color = "Anomálnosť") +
  scale_color_manual(labels = c("áno", "nie"), values = c(hue_pal()(2)[1], hue_pal()(2)[2])) +
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )
print(my.raw.tsne.plot)


my.raw.fea <- lapply(IRELAND[DATACOLUMNS], repr_feaclip)

my.raw.df <- unlist(my.raw.fea, use.names = FALSE)
dim(my.raw.df) <- c(length(DATACOLUMNS), 8)
my.raw.df <- data.frame(my.raw.df)
names(my.raw.df) <- my.names
rownames(my.raw.df) <- DATACOLUMNS

my.raw.sum.iqr <- getIQRrule(my.raw.df$sum_1)
my.raw.cro.iqr <- getIQRrule(my.raw.df$cross.)

my.raw.groups <- ((my.raw.df$sum_1 %in% my.raw.sum.iqr) | (my.raw.df$cross. %in% my.raw.cro.iqr))

my.raw.tsne <- Rtsne(my.raw.df, verbose = TRUE, max_iter = 500)
my.raw.tsne.plotData <- as.data.frame(my.raw.tsne$Y)
names(my.raw.tsne.plotData) <- c("X", "Y")
my.raw.tsne.plotData$groups <- my.raw.groups


my.raw.tsne.plot <- ggplot(my.raw.tsne.plotData, aes(x = X, y = Y, colour = groups)) + geom_point() +
  xlab("TSNE1") + ylab("TSNE2") + labs(color = "Anomálnosť") +
  scale_color_manual(labels = c("áno", "nie"), values = c(hue_pal()(2)[1], hue_pal()(2)[2])) +
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )
print(my.raw.tsne.plot)