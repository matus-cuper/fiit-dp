co.selected <- "X4539.csv"
co.offset <- 455 * MPD
co.offset <- 70 * MPD
co.range <- 1:672

# Compute S-H-ESD
ad.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = IRELAND[[co.selected]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

ad.df <- IRELAND[, c("timestamp", co.selected)]
names(ad.df) <- c("timestamp", "load")
ad.df$score <- 0

# Merge results with original data
ad.merged <- merge(x = data.frame(timestamp  = ad.df$timestamp), y = ad.twitter$anoms, by = "timestamp", all.x = TRUE)
ad.merged$anoms[!is.na(ad.merged$anoms)] <- 1
ad.merged$anoms[is.na(ad.merged$anoms)] <- 0

# Add some variables needed for visualization
ad.df$final <- ad.merged$anoms
ad.df <- ad.df[ad.df$timestamp > "2009-07-31 23:59:59", ]

ad.df$weekday <- as.POSIXlt(ad.df$timestamp)$wday
ad.df$weekdayFactor <- factor(lubridate::wday(ad.df$timestamp, week_start = 1), levels = rev(1:7), labels = rev(c("Po", "Ut", "St", "Št", "Pi", "So", "Ne")), ordered = TRUE)
ad.df$monthFactor <- factor(month(ad.df$timestamp), levels = as.character(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "Máj", "Jún", "Júl", "Aug", "Sep", "Okt", "Nov", "Dec"), ordered = TRUE)
ad.df$yearMonth <- factor(as.yearmon(ad.df$timestamp))
ad.df$week <- as.numeric(format(as.POSIXct(ad.df$timestamp), "%W"))
ad.plotData <- ddply(ad.df, .(yearMonth), transform, monthWeek = 1 + week - min(week))

# Draw line plot
ad.p1 <- ggplot() + geom_line(aes(x = co.range + co.offset, y = load, color = final), size = 1, data = ad.df[co.range + co.offset, ]) +
  ggtitle("Metóda S-H-ESD") +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(hjust = 0.5)
  )


# Smooth raw time series data of selected consumer
own.loess <- predict(loess(IRELAND[[co.selected]] ~ c(1:nrow(IRELAND)), span = 0.0006))

# Run S-H-ESD method
own.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = own.loess), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

# Compute scoring based on clustering score for every analyzed week
own.df <- IRELAND[, c("timestamp", co.selected)]
names(own.df) <- c("timestamp", "load")
own.df$score <- 0
for (own.col in 1:(WEEKS - 1)) {
  own.s <- (own.col - 1) * WEEK * MPD + 1
  own.e <- own.col * WEEK * MPD

  co.range.workdays <- as.numeric(rownames(filterWorkdays(filterWeekdays(own.ireland[own.s:own.e, ]))))
  co.range.holidays <- c(
    as.numeric(rownames(filterHolidays(own.ireland[own.s:own.e, ]))),
    as.numeric(rownames(filterWeekends(own.ireland[own.s:own.e, ])))
  )

  if (own.col %in% c(1, WEEKS - 1)) {
    if (own.col == 1) {
      own.df$score[co.range.workdays] <- SUSPICIOUS_WORKDAYS[[co.selected]][1]
      own.df$score[co.range.holidays] <- SUSPICIOUS_HOLIDAYS[[co.selected]][1]
    }
    else {
      own.df$score[co.range.workdays] <- SUSPICIOUS_WORKDAYS[[co.selected]][own.col - 1]
      own.df$score[co.range.holidays] <- SUSPICIOUS_HOLIDAYS[[co.selected]][own.col - 1]
    }
  }
  else {
    own.df$score[co.range.workdays] <- mean(SUSPICIOUS_WORKDAYS[[co.selected]][c(own.col - 1, own.col)])
    own.df$score[co.range.holidays] <- mean(SUSPICIOUS_HOLIDAYS[[co.selected]][c(own.col - 1, own.col)])
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

own.p2 <- ggplot() + geom_line(aes(x = co.range + co.offset, y = load, color = final), size = 1, data = own.df[co.range + co.offset, ]) +
  ggtitle("Navrhnutá metóda") +
  xlab("Poradie meraní") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

grid.arrange(ad.p1, own.p2, nrow = 2)
