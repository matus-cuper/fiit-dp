# Load datasets
df.9 <- loadIrelandDirectory("~/r/fiit-dp/data/ireland", windowsCount = 9)
df.agg.15 <- loadIrelandDirectory("~/r/fiit-dp/data/ireland", windowsCount = 15, fun = mean)
df.agg.9 <- loadIrelandDirectory("~/r/fiit-dp/data/ireland", windowsCount = 9, fun = mean)
df.agg.5 <- loadIrelandDirectory("~/r/fiit-dp/data/ireland", windowsCount = 5, fun = mean)

# Create normalized dataset with filtered days
df.agg.15.days <- normalizeDataset(aggregate(. ~ time, df.agg.15, FUN = mean))
df.agg.9.days <- normalizeDataset(aggregate(. ~ time, df.agg.9, FUN = mean))
df.agg.5.days <- normalizeDataset(aggregate(. ~ time, df.agg.5, FUN = mean))
df.agg.15.weekends <- normalizeDataset(aggregate(. ~ time, filterWeekdays(df.agg.15, skipDays = c(1:5)), FUN = mean))
df.agg.9.weekends <- normalizeDataset(aggregate(. ~ time, filterWeekdays(df.agg.9, skipDays = c(1:5)), FUN = mean))
df.agg.5.weekends <- normalizeDataset(aggregate(. ~ time, filterWeekdays(df.agg.5, skipDays = c(1:5)), FUN = mean))
df.agg.15.weekdays <- normalizeDataset(aggregate(. ~ time, filterWeekdays(df.agg.15), FUN = mean))
df.agg.9.weekdays <- normalizeDataset(aggregate(. ~ time, filterWeekdays(df.agg.9), FUN = mean))
df.agg.5.weekdays <- normalizeDataset(aggregate(. ~ time, filterWeekdays(df.agg.5), FUN = mean))

# Load single time series
df.3778 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/3788.csv", windowsCount = Inf)
tmp <- filterWeekdays(df.3778$dataset)
tmp <- tmp[3361:8640, c("timestamp", "load")]
tmp$timestamp <- as.POSIXct(tmp$timestamp)
res <- AnomalyDetectionVec(tmp$load, period = 48, longterm_period = 48*5, plot = TRUE, max_anoms = 0.1, direction = "both")
res <- AnomalyDetectionTs(tmp, plot = TRUE, max_anoms = 0.1, direction = "both")


# Figure for 70 samples and 10 clusters
clustersCount <- 10
res <- clusteringLaurinecByMean(df.agg.5.days[, grepl("2[0]...csv", names(df.agg.5.days))], 
                                clusterRangeMin = 1, clusterRangeMax = 25, clustersCount = clustersCount)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.5.days.LAU.pdf')

pc <- tsclust(t(df.agg.5.days[, grepl("2[0]...csv", names(df.agg.5.days))]), k = clustersCount, distance = "dtw_basic",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.5.days.DTW.pdf')

# Figure for 70 samples and 10 clusters but longer aggregate
clustersCount <- 10
res <- clusteringLaurinecByMean(df.agg.15.days[, grepl("2[0]...csv", names(df.agg.15.days))], 
                                clusterRangeMin = 1, clusterRangeMax = 25, clustersCount = clustersCount)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.15.days.LAU.pdf')

pc <- tsclust(t(df.agg.15.days[, grepl("2[0]...csv", names(df.agg.15.days))]), k = clustersCount, distance = "dtw_basic",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.15.days.DTW.pdf')

# Figure for 70 samples and 10 clusters with different distance metrics
pc <- tsclust(t(df.agg.9.days[, grepl("2[0]...csv", names(df.agg.9.days))]), k = clustersCount, distance = "gak",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.9.days.GAK.pdf')

pc <- tsclust(t(df.agg.9.days[, grepl("2[0]...csv", names(df.agg.9.days))]), type = "hierarchical", k = clustersCount, distance = "dtw",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.9.days.DTWhier.pdf')

pc <- tsclust(t(df.agg.9.days[, grepl("2[0]...csv", names(df.agg.9.days))]), k = clustersCount, distance = "sbd",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.9.days.SBD.pdf')

pc <- tsclust(t(df.agg.9.days[, grepl("2[0]...csv", names(df.agg.9.days))]), k = clustersCount, distance = "lbk",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.9.days.LBK.pdf')

pc <- tsclust(t(df.agg.9.days[, grepl("2[0]...csv", names(df.agg.9.days))]), k = clustersCount, distance = "dtw2",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.9.days.DTW2.pdf')
# https://rdrr.io/cran/dtwclust/man/tsclust.html


# --------------------------------------------
mean(as.data.frame(table(pc@cluster))$Freq)
median(as.data.frame(table(pc@cluster))$Freq)
# --------------------------------------------


# Figure for 70 samples and 10 clusters with different day types and different time series lengths
clustersCount <- 10
res <- clusteringLaurinecByMean(df.agg.15.weekdays[, grepl("2[0]...csv", names(df.agg.15.weekdays))], 
                                clusterRangeMin = 1, clusterRangeMax = 25, clustersCount = clustersCount)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.15.weekdays.LAU.pdf')

pc <- tsclust(t(df.agg.15.weekdays[, grepl("2[0]...csv", names(df.agg.15.weekdays))]), k = clustersCount, distance = "dtw_basic",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.15.weekdays.DTW.pdf')


clustersCount <- 10
res <- clusteringLaurinecByMean(df.agg.15.weekends[, grepl("2[0]...csv", names(df.agg.15.weekends))], 
                                clusterRangeMin = 1, clusterRangeMax = 25, clustersCount = clustersCount)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.15.weekends.LAU.pdf')

pc <- tsclust(t(df.agg.15.weekends[, grepl("2[0]...csv", names(df.agg.15.weekends))]), k = clustersCount, distance = "dtw_basic",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.15.weekends.DTW.pdf')


clustersCount <- 10
res <- clusteringLaurinecByMean(df.agg.5.weekdays[, grepl("2[0]...csv", names(df.agg.5.weekdays))], 
                                clusterRangeMin = 1, clusterRangeMax = 25, clustersCount = clustersCount)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.5.weekdays.LAU.pdf')

pc <- tsclust(t(df.agg.5.weekdays[, grepl("2[0]...csv", names(df.agg.5.weekdays))]), k = clustersCount, distance = "dtw_basic",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.5.weekdays.DTW.pdf')


clustersCount <- 10
res <- clusteringLaurinecByMean(df.agg.5.weekends[, grepl("2[0]...csv", names(df.agg.5.weekends))], 
                                clusterRangeMin = 1, clusterRangeMax = 25, clustersCount = clustersCount)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.5.weekends.LAU.pdf')

pc <- tsclust(t(df.agg.5.weekends[, grepl("2[0]...csv", names(df.agg.5.weekends))]), k = clustersCount, distance = "dtw_basic",
              args = tsclust_args(dist = list(window.size = clustersCount)))
plot(pc)
dev.print(pdf, '~/r/fiit-dp//presentation/df.agg.5.weekends.DTW.pdf')


# Some breakout detection stuff
df.1000 <- loadIrelandDataset("~/r/fiit-dp/data/ireland/1000.csv", windowsCount = Inf)
visualizeDatasetStats(df.1000$dataset)
res = breakout(df.1000$dataset$load, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot
res <- breakout(df.9$`3378.csv`[625:1248], min.size = 6, method = 'multi', beta = 0.001, degree = 10, plot = TRUE)
res$loc

plot(ts(df.9$`3788.csv`[625:1248]))
abline(v = res$loc, col = "red")
