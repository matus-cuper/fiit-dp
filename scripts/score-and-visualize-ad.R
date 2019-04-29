prepareDataset()

ad.selected <- "X2172.csv"

# Compute S-H-ESD
ad.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = IRELAND[[ad.selected]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

ad.df <- IRELAND[, c("timestamp", ad.selected)]
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
ad.offset <- 122 * MPD
ad.range <- 1:672
ad.p1 <- ggplot() + geom_line(aes(x = ad.range + ad.offset, y = load, color = final), size = 1, data = ad.df[ad.range + ad.offset, ]) +
  xlab("Poradie meraní (začiatok decembra)") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black")
  )


ad.selected <- "X6536.csv"

# Compute S-H-ESD
ad.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = IRELAND[[ad.selected]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

ad.df <- IRELAND[, c("timestamp", ad.selected)]
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
ad.offset <- 61 * MPD
ad.range <- 1:672
ad.p2 <- ggplot() + geom_line(aes(x = ad.range + ad.offset, y = load, color = final), size = 1, data = ad.df[ad.range + ad.offset, ]) +
  xlab("Poradie meraní (začiatok októbra)") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black")
  )

# Arrange both plots into one picture
grid.arrange(ad.p1, ad.p2, nrow = 2)
