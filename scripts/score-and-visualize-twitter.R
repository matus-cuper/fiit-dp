my.selected <- "X2172.csv"

my.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = IRELAND[[my.selected]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

my.df <- IRELAND[, c("timestamp", my.selected)]
names(my.df) <- c("timestamp", "load")
my.df$score <- 0


my.merged <- merge(x = data.frame(timestamp  = my.df$timestamp), y = my.twitter$anoms, by = "timestamp", all.x = TRUE)
my.merged$anoms[!is.na(my.merged$anoms)] <- 1
my.merged$anoms[is.na(my.merged$anoms)] <- 0

my.df$final <- my.merged$anoms


my.df <- my.df[my.df$timestamp > "2009-07-31 23:59:59", ]

my.df$weekday <- as.POSIXlt(my.df$timestamp)$wday
my.df$weekdayFactor <- factor(lubridate::wday(my.df$timestamp, week_start = 1), levels = rev(1:7), labels = rev(c("Po", "Ut", "St", "Št", "Pi", "So", "Ne")), ordered = TRUE)
my.df$monthFactor <- factor(month(my.df$timestamp), levels = as.character(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "Máj", "Jún", "Júl", "Aug", "Sep", "Okt", "Nov", "Dec"), ordered = TRUE)
my.df$yearMonth <- factor(as.yearmon(my.df$timestamp))
my.df$week <- as.numeric(format(as.POSIXct(my.df$timestamp), "%W"))
my.plotData <- ddply(my.df, .(yearMonth), transform, monthWeek = 1 + week - min(week))


my.offset <- 122 * MPD
my.range <- 1:672
my.p1 <- ggplot() + geom_line(aes(x = my.range + my.offset, y = load, color = final), size = 1, data = my.df[my.range + my.offset, ]) +
  xlab("Poradie meraní (začiatok decembra)") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black")
  )


my.selected <- "X6536.csv"

my.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = IRELAND[[my.selected]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

my.df <- IRELAND[, c("timestamp", my.selected)]
names(my.df) <- c("timestamp", "load")
my.df$score <- 0


my.merged <- merge(x = data.frame(timestamp  = my.df$timestamp), y = my.twitter$anoms, by = "timestamp", all.x = TRUE)
my.merged$anoms[!is.na(my.merged$anoms)] <- 1
my.merged$anoms[is.na(my.merged$anoms)] <- 0

my.df$final <- my.merged$anoms


my.df <- my.df[my.df$timestamp > "2009-07-31 23:59:59", ]

my.df$weekday <- as.POSIXlt(my.df$timestamp)$wday
my.df$weekdayFactor <- factor(lubridate::wday(my.df$timestamp, week_start = 1), levels = rev(1:7), labels = rev(c("Po", "Ut", "St", "Št", "Pi", "So", "Ne")), ordered = TRUE)
my.df$monthFactor <- factor(month(my.df$timestamp), levels = as.character(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "Máj", "Jún", "Júl", "Aug", "Sep", "Okt", "Nov", "Dec"), ordered = TRUE)
my.df$yearMonth <- factor(as.yearmon(my.df$timestamp))
my.df$week <- as.numeric(format(as.POSIXct(my.df$timestamp), "%W"))
my.plotData <- ddply(my.df, .(yearMonth), transform, monthWeek = 1 + week - min(week))


my.offset <- 61 * MPD
my.range <- 1:672
my.p2 <- ggplot() + geom_line(aes(x = my.range + my.offset, y = load, color = final), size = 1, data = my.df[my.range + my.offset, ]) +
  xlab("Poradie meraní (začiatok októbra)") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black")
  )

grid.arrange(my.p1, my.p2, nrow = 2)
