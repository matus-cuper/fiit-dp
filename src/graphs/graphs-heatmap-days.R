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


my.df <- my.df[my.df$timestamp > "2009-07-31 23:59:59", ]
my.df <- my.df[my.df$timestamp > "2009-12-31 23:59:59", ]

my.df$weekday <- as.POSIXlt(my.df$timestamp)$wday
my.df$weekdayFactor <- factor(lubridate::wday(my.df$timestamp, week_start = 1), levels = rev(1:7), labels = rev(c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")), ordered = TRUE)
my.df$monthFactor <- factor(month(my.df$timestamp), levels = as.character(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)
my.df$yearMonth <- factor(as.yearmon(my.df$timestamp))
my.df$week <- as.numeric(format(as.POSIXct(my.df$timestamp), "%W"))
my.plotData <- ddply(my.df, .(yearMonth), transform, monthWeek = 1 + week - min(week))

ggplot(my.plotData, aes(monthWeek, weekdayFactor, fill = my.plotData$score)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(my.plotData$timestamp) ~ monthFactor) + 
  scale_fill_gradient(low = "white", high = "red") + 
  xlab("Week of the month") + ylab("Day of the week") + labs(fill = "Score") +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )
