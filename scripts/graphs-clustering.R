a.medoidsID <- attributes(CLUSTERING_WORKDAYS[[51]]$centroids)$series_id
a.clustersID <- list()
for (a.col in 1:length(a.medoidsID))
  a.clustersID[[a.col]] <- which(CLUSTERING_WORKDAYS[[51]]$cluster == a.col)

s <- 50 * WEEK * MPD + 1
e <- 52 * WEEK * MPD
a.df <- filterWorkdays(filterWeekdays(IRELAND[s:e, ]))
a.agg <- data.frame(lapply(aggregate(. ~ time, a.df[c("time", DATACOLUMNS)], FUN = mean)[DATACOLUMNS], norm_z))


a.plots <- list()
for (a.col in 1:25) {
  a.tmp <- a.agg[DATACOLUMNS[a.clustersID[[a.col]]]]
  a.tmp$id <- 0:47
  a.melt <- melt(a.tmp, id.vars = "id", measure.vars = DATACOLUMNS[a.clustersID[[a.col]]])
  a.medoid <- a.agg[DATACOLUMNS[a.medoidsID[[a.col]]]]
  names(a.medoid) <- c("value")
  a.medoid$id <- c(0:47)
  a.medoid$variable <- "medoid"
  a.melt$group <- "a"
  a.medoid$group <- "b"
  a.another <- rbind(a.melt, a.medoid)

  if (a.col %in% c(1, 6, 11, 16)) {
    a.plots[[a.col]] <- ggplot(a.another, aes(x = id, y = value, group = variable, colour = group)) +
      geom_line() + scale_color_manual(values = c("#000000", "#FF0000")) +
      scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
      scale_y_continuous(limits = c(-3, 7)) +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black")
      )
  }
  else if (a.col %in% c(22, 23, 24, 25)) {
    a.plots[[a.col]] <- ggplot(a.another, aes(x = id, y = value, group = variable, colour = group)) +
      geom_line() + scale_color_manual(values = c("#000000", "#FF0000")) +
      scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
      scale_y_continuous(limits = c(-3, 7)) +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black")
      )
  }
  else if (a.col == 21) {
    a.plots[[a.col]] <- ggplot(a.another, aes(x = id, y = value, group = variable, colour = group)) +
      geom_line() + scale_color_manual(values = c("#000000", "#FF0000")) +
      scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
      scale_y_continuous(limits = c(-3, 7)) +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black")
      )
  }
  else {
    a.plots[[a.col]] <- ggplot(a.another, aes(x = id, y = value, group = variable, colour = group)) +
      geom_line() + scale_color_manual(values = c("#000000", "#FF0000")) +
      scale_x_continuous(minor_breaks = seq(0, 48, 6), breaks = seq(0, 48, 12)) +
      scale_y_continuous(limits = c(-3, 7)) +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "grey"),
        panel.background = element_rect(fill = "white", color = "black")
      )
  }
}

grid.arrange(a.plots[[1]], a.plots[[2]], a.plots[[3]], a.plots[[4]], a.plots[[5]],
             a.plots[[6]], a.plots[[7]], a.plots[[8]], a.plots[[9]], a.plots[[10]],
             a.plots[[11]], a.plots[[12]], a.plots[[13]], a.plots[[14]], a.plots[[15]],
             a.plots[[16]], a.plots[[17]], a.plots[[18]], a.plots[[19]], a.plots[[20]],
             a.plots[[21]], a.plots[[22]], a.plots[[23]], a.plots[[24]], a.plots[[25]])
