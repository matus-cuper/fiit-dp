prepareDataset()

# Create normalized dataste
own.ireland <- data.frame(lapply(IRELAND[DATACOLUMNS], norm_z))
own.ireland <- cbind(IRELAND[METADATACOLUMNS], own.ireland)

# Compute score based on clustering
SUSPICIOUS_HOLIDAYS <- scoreSuspiciousTS(CLUSTERING_HOLIDAYS)
SUSPICIOUS_WORKDAYS <- scoreSuspiciousTS(CLUSTERING_WORKDAYS)

# Select consumers that are identified as anomalous multiple times
own.numberOfConsumers <- 50 / length(DATACOLUMNS)
own.result <- c()
for (own.col in 1:length(CLUSTERING_HOLIDAYS)) {
  own.t0 <- Sys.time()
  own.week <- own.col
  own.s <- (own.week - 1) * WEEK * MPD + 1
  own.e <- (own.week + 1) * WEEK * MPD
  own.range <- own.s:own.e

  own.holidays.df <- merge(filterHolidays(own.ireland[own.range, ]), filterWeekends(own.ireland[own.range, ]), all = TRUE)
  own.workdays.df <- filterWorkdays(filterWeekdays(own.ireland[own.range, ]))

  own.holidays.iqr <- lapply(own.holidays.df[DATACOLUMNS], repr_windowing, win_size = 48, func = repr_feaclip)
  own.workdays.iqr <- lapply(own.workdays.df[DATACOLUMNS], repr_windowing, win_size = 48, func = repr_feaclip)
  own.holidays.iqr <- lapply(own.holidays.iqr, function(x) data.frame(sum_1 = mean(x[2 * 1:(length(x) / 8)]),
                                                                    cross. = mean(x[4 * 1:(length(x) / 8)])))
  own.workdays.iqr <- lapply(own.workdays.iqr, function(x) data.frame(sum_1 = mean(x[2 * 1:(length(x) / 8)]),
                                                                    cross. = mean(x[4 * 1:(length(x) / 8)])))

  own.workdays.suspicious.feaclip <- unlistAndTransform(own.workdays.iqr, c(length(DATACOLUMNS), 2), c("sum_1", "cross."))
  own.holidays.suspicious.feaclip <- unlistAndTransform(own.holidays.iqr, c(length(DATACOLUMNS), 2), c("sum_1", "cross."))
  own.workdays.suspicious.feaclip <- own.workdays.suspicious.feaclip[
    own.workdays.suspicious.feaclip$sum_1 %in% getIQRrule(own.workdays.suspicious.feaclip$sum_1) |
      own.workdays.suspicious.feaclip$cross. %in% getIQRrule(own.workdays.suspicious.feaclip$cross.), ]
  own.holidays.suspicious.feaclip <- own.holidays.suspicious.feaclip[
    own.holidays.suspicious.feaclip$sum_1 %in% getIQRrule(own.holidays.suspicious.feaclip$sum_1) |
      own.holidays.suspicious.feaclip$cross. %in% getIQRrule(own.holidays.suspicious.feaclip$cross.), ]

  own.workdays.suspicious <- names(getIQRrule(unlist(lapply(SUSPICIOUS_WORKDAYS, function(x) x[own.week]))))
  own.holidays.suspicious <- names(getIQRrule(unlist(lapply(SUSPICIOUS_HOLIDAYS, function(x) x[own.week]))))

  own.result <- c(own.result, rownames(own.workdays.suspicious.feaclip), rownames(own.holidays.suspicious.feaclip),
                 own.workdays.suspicious, own.workdays.suspicious, own.holidays.suspicious)
  print(own.week)
  print(Sys.time() - own.t0)
}

# Select one suspicious consumer
SUSPICIOUS_CONSUMERS <- names(table(own.result)[which(table(own.result) > quantile(table(own.result), probs = 1 - own.numberOfConsumers))])
# 2172 2196 5449 6536 6537
own.selected <- "X2172.csv"
# own.selected <- "X6536.csv"

# Smooth raw time series data of selected consumer
own.loess <- predict(loess(IRELAND[[own.selected]] ~ c(1:nrow(IRELAND)), span = 0.0006))
# dygraph(xts(own.loess, order.by = as.POSIXct(IRELAND$timestamp)))
# dygraph(xts(IRELAND[[own.selected]], order.by = as.POSIXct(IRELAND$timestamp)))

# Run S-H-ESD method
own.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = own.loess), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

# Compute scoring based on clustering score for every analyzed week
own.df <- IRELAND[, c("timestamp", own.selected)]
names(own.df) <- c("timestamp", "load")
own.df$score <- 0
for (own.col in 1:(WEEKS - 1)) {
  own.s <- (own.col - 1) * WEEK * MPD + 1
  own.e <- own.col * WEEK * MPD

  own.range.workdays <- as.numeric(rownames(filterWorkdays(filterWeekdays(own.ireland[own.s:own.e, ]))))
  own.range.holidays <- c(
    as.numeric(rownames(filterHolidays(own.ireland[own.s:own.e, ]))),
    as.numeric(rownames(filterWeekends(own.ireland[own.s:own.e, ])))
  )

  if (own.col %in% c(1, WEEKS - 1)) {
    if (own.col == 1) {
      own.df$score[own.range.workdays] <- SUSPICIOUS_WORKDAYS[[own.selected]][1]
      own.df$score[own.range.holidays] <- SUSPICIOUS_HOLIDAYS[[own.selected]][1]
    }
    else {
      own.df$score[own.range.workdays] <- SUSPICIOUS_WORKDAYS[[own.selected]][own.col - 1]
      own.df$score[own.range.holidays] <- SUSPICIOUS_HOLIDAYS[[own.selected]][own.col - 1]
    }
  }
  else {
    own.df$score[own.range.workdays] <- mean(SUSPICIOUS_WORKDAYS[[own.selected]][c(own.col - 1, own.col)])
    own.df$score[own.range.holidays] <- mean(SUSPICIOUS_HOLIDAYS[[own.selected]][c(own.col - 1, own.col)])
  }
}

# Merge computed score of consumer with S-H-ESD results
own.merged <- merge(x = data.frame(timestamp  = own.df$timestamp), y = own.twitter$anoms, by = "timestamp", all.x = TRUE)
own.merged$anoms[!is.na(own.merged$anoms)] <- 1
own.merged$anoms[is.na(own.merged$anoms)] <- 0

own.df$smoothing <- predict(loess(own.merged$anoms ~ c(1:nrow(IRELAND)), span = 0.001))
own.df$final <- own.df$score + own.df$smoothing

# Append some columns needed for visualization
own.df <- own.df[own.df$timestamp > "2009-07-31 23:59:59", ]

own.df$weekday <- as.POSIXlt(own.df$timestamp)$wday
own.df$weekdayFactor <- factor(lubridate::wday(own.df$timestamp, week_start = 1), levels = rev(1:7), labels = rev(c("Po", "Ut", "St", "Št", "Pi", "So", "Ne")), ordered = TRUE)
own.df$monthFactor <- factor(month(own.df$timestamp), levels = as.character(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "Máj", "Jún", "Júl", "Aug", "Sep", "Okt", "Nov", "Dec"), ordered = TRUE)
own.df$yearMonth <- factor(as.yearmon(own.df$timestamp))
own.df$week <- as.numeric(format(as.POSIXct(own.df$timestamp), "%W"))
own.plotData <- ddply(own.df, .(yearMonth), transform, monthWeek = 1 + week - min(week))

# Draw heat map of selected consumer
own.p1 <- ggplot(own.plotData, aes(monthWeek, weekdayFactor, fill = own.plotData$score)) +
  geom_tile(colour = "white") +
  facet_grid(year(own.plotData$timestamp) ~ monthFactor) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("Týždeň mesiaca") + ylab("Dni v týždni") + labs(fill = "Skóre") +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )


# Draw line plot for selected range of given consumer
own.offset <- 61 * MPD
own.range <- 1:672
own.p2 <- ggplot() + geom_line(aes(x = own.range + own.offset, y = load, color = final), size = 1, data = own.df[own.range + own.offset, ]) +
  xlab("Poradie meraní (začiatok decembra)") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black")
  )

# Arrange both plots into one picture
grid.arrange(own.p1, own.p2, nrow = 2)
