my.ireland <- data.frame(lapply(IRELAND[DATACOLUMNS], norm_z))
my.ireland <- cbind(IRELAND[METADATACOLUMNS], my.ireland)

SUSPICIOUS_HOLIDAYS <- scoreSuspiciousTS(CLUSTERING_HOLIDAYS)
SUSPICIOUS_WORKDAYS <- scoreSuspiciousTS(CLUSTERING_WORKDAYS)

my.selected <- "X1525.csv"
my.df <- my.ireland[, c("timestamp", my.selected)]
names(my.df) <- c("timestamp", "load")
my.df$score <- 0

for (my.col in 1:(WEEKS - 1)) {
  my.s <- (my.col - 1) * WEEK * MPD + 1
  my.e <- my.col * WEEK * MPD
  
  my.range.workdays <- as.numeric(rownames(filterWorkdays(filterWeekdays(my.ireland[my.s:my.e, c("timestamp", "day", "holiday")]))))
  my.range.holidays <- c(
    as.numeric(rownames(filterHolidays(my.ireland[my.s:my.e, c("timestamp", "day", "holiday")]))), 
    as.numeric(rownames(filterWeekends(my.ireland[my.s:my.e, c("timestamp", "day", "holiday")])))
  )
  
  
  if (my.col %in% c(1, WEEKS - 1)) {
    if (my.col == 1) {
      my.df$score[my.range.workdays] <- SUSPICIOUS_WORKDAYS[[my.selected]][1]
      my.df$score[my.range.holidays] <- SUSPICIOUS_HOLIDAYS[[my.selected]][1]
    }
    else {
      my.df$score[my.range.workdays] <- SUSPICIOUS_WORKDAYS[[my.selected]][my.col - 1]
      my.df$score[my.range.holidays] <- SUSPICIOUS_HOLIDAYS[[my.selected]][my.col - 1]
    }
  }
  else {
    my.df$score[my.range.workdays] <- mean(SUSPICIOUS_WORKDAYS[[my.selected]][c(my.col - 1, my.col)])
    my.df$score[my.range.holidays] <- mean(SUSPICIOUS_HOLIDAYS[[my.selected]][c(my.col - 1, my.col)])
  }
}

my.merged <- merge(x = data.frame(timestamp  = my.df$timestamp), y = TWITTER_TS[[my.selected]]$anom$anoms, by = "timestamp", all.x = TRUE)
my.merged$anoms[!is.na(my.merged$anoms)] <- 1
my.merged$anoms[is.na(my.merged$anoms)] <- 0

my.es <- c(es(my.merged$anoms)$fitted)
my.es.rev <- rev(c(es(rev(my.merged$anoms))$fitted))

my.df$smoothing <- (my.es + my.es.rev) / 2
my.df$final <- my.df$score + my.df$smoothing
my.df$date <- as.Date(my.df$timestamp, format = "%Y-%m-%d")
my.df$hm <- format(as.POSIXct(my.df$timestamp), "%H:%M")
my.lab <- with(my.df, paste(format(as.POSIXct(my.df$timestamp), "%H"), "00", sep = ":"))

ggplot(data = my.df[1:2976, ], aes(x = date, y = hm, fill = final)) + geom_tile() +
  xlab("Date") + ylab("Hour") + labs(fill = "Score") +
  scale_fill_gradient(low = "white", high="red") +
  scale_y_discrete(breaks = c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00")) +
  theme(
    text = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = "black")
  )
