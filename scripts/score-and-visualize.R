# Majme normalizovany dataset
my.ireland <- data.frame(lapply(IRELAND[DATACOLUMNS], norm_z))
my.ireland <- cbind(IRELAND[METADATACOLUMNS], my.ireland)

# Vypocitame si skore pomocou zhlukovnaia
SUSPICIOUS_HOLIDAYS <- scoreSuspiciousTS(CLUSTERING_HOLIDAYS)
SUSPICIOUS_WORKDAYS <- scoreSuspiciousTS(CLUSTERING_WORKDAYS)

# Vyberieme odberatelov, ktory boli viac krat identifikovany ako anomalny
my.numberOfConsumers <- 50 / length(DATACOLUMNS)
my.result <- c()
for (my.col in 1:length(CLUSTERING_HOLIDAYS)) {
  my.t0 <- Sys.time()
  my.week <- my.col
  my.s <- (my.week - 1) * WEEK * MPD + 1
  my.e <- (my.week + 1) * WEEK * MPD
  my.range <- my.s:my.e

  my.holidays.df <- merge(filterHolidays(my.ireland[my.range, ]), filterWeekends(my.ireland[my.range, ]), all = TRUE)
  my.workdays.df <- filterWorkdays(filterWeekdays(my.ireland[my.range, ]))

  my.holidays.iqr <- lapply(my.holidays.df[DATACOLUMNS], repr_windowing, win_size = 48, func = repr_feaclip)
  my.workdays.iqr <- lapply(my.workdays.df[DATACOLUMNS], repr_windowing, win_size = 48, func = repr_feaclip)
  my.holidays.iqr <- lapply(my.holidays.iqr, function(x) data.frame(sum_1 = mean(x[2 * 1:(length(x) / 8)]),
                                                                    cross. = mean(x[4 * 1:(length(x) / 8)])))
  my.workdays.iqr <- lapply(my.workdays.iqr, function(x) data.frame(sum_1 = mean(x[2 * 1:(length(x) / 8)]),
                                                                    cross. = mean(x[4 * 1:(length(x) / 8)])))

  my.workdays.suspicious.feaclip <- unlistAndTransform(my.workdays.iqr, c(length(DATACOLUMNS), 2), c("sum_1", "cross."))
  my.holidays.suspicious.feaclip <- unlistAndTransform(my.holidays.iqr, c(length(DATACOLUMNS), 2), c("sum_1", "cross."))
  my.workdays.suspicious.feaclip <- my.workdays.suspicious.feaclip[
    my.workdays.suspicious.feaclip$sum_1 %in% getIQRrule(my.workdays.suspicious.feaclip$sum_1) |
      my.workdays.suspicious.feaclip$cross. %in% getIQRrule(my.workdays.suspicious.feaclip$cross.), ]
  my.holidays.suspicious.feaclip <- my.holidays.suspicious.feaclip[
    my.holidays.suspicious.feaclip$sum_1 %in% getIQRrule(my.holidays.suspicious.feaclip$sum_1) |
      my.holidays.suspicious.feaclip$cross. %in% getIQRrule(my.holidays.suspicious.feaclip$cross.), ]

  my.workdays.suspicious <- names(getIQRrule(unlist(lapply(SUSPICIOUS_WORKDAYS, function(x) x[my.week]))))
  my.holidays.suspicious <- names(getIQRrule(unlist(lapply(SUSPICIOUS_HOLIDAYS, function(x) x[my.week]))))

  my.result <- c(my.result, rownames(my.workdays.suspicious.feaclip), rownames(my.holidays.suspicious.feaclip),
                 my.workdays.suspicious, my.workdays.suspicious, my.holidays.suspicious)
  print(my.week)
  print(Sys.time() - my.t0)
}
SUSPICIOUS_CONSUMERS <- names(table(my.result)[which(table(my.result) > quantile(table(my.result), probs = 1 - my.numberOfConsumers))])
# 2172 2196 5449 6536 6537
my.selected <- "X2172.csv"
# my.selected <- "X6536.csv"

# Vyhladime vstupne casove rady
my.loess <- predict(loess(IRELAND[[my.selected]] ~ c(1:nrow(IRELAND)), span = 0.0006))
# dygraph(xts(my.loess, order.by = as.POSIXct(IRELAND$timestamp)))
# dygraph(xts(IRELAND[[my.selected]], order.by = as.POSIXct(IRELAND$timestamp)))

# Aplikujeme S-H-ESD
my.twitter <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND$timestamp), load = my.loess), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = TRUE)

# Vypocitame skore pre jednotlive tyzdne pre daneho odberatela na zaklade zhlukovacieho skore
my.df <- IRELAND[, c("timestamp", my.selected)]
names(my.df) <- c("timestamp", "load")
my.df$score <- 0
for (my.col in 1:(WEEKS - 1)) {
  my.s <- (my.col - 1) * WEEK * MPD + 1
  my.e <- my.col * WEEK * MPD

  my.range.workdays <- as.numeric(rownames(filterWorkdays(filterWeekdays(my.ireland[my.s:my.e, ]))))
  my.range.holidays <- c(
    as.numeric(rownames(filterHolidays(my.ireland[my.s:my.e, ]))),
    as.numeric(rownames(filterWeekends(my.ireland[my.s:my.e, ])))
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

# Vypocitame skore pre daneho odberatela na zaklade twitter anomaly detection
my.merged <- merge(x = data.frame(timestamp  = my.df$timestamp), y = my.twitter$anoms, by = "timestamp", all.x = TRUE)
my.merged$anoms[!is.na(my.merged$anoms)] <- 1
my.merged$anoms[is.na(my.merged$anoms)] <- 0

my.df$smoothing <- predict(loess(my.merged$anoms ~ c(1:nrow(IRELAND)), span = 0.001))
my.df$final <- my.df$score + my.df$smoothing


my.df <- my.df[my.df$timestamp > "2009-07-31 23:59:59", ]

my.df$weekday <- as.POSIXlt(my.df$timestamp)$wday
my.df$weekdayFactor <- factor(lubridate::wday(my.df$timestamp, week_start = 1), levels = rev(1:7), labels = rev(c("Po", "Ut", "St", "Št", "Pi", "So", "Ne")), ordered = TRUE)
my.df$monthFactor <- factor(month(my.df$timestamp), levels = as.character(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "Máj", "Jún", "Júl", "Aug", "Sep", "Okt", "Nov", "Dec"), ordered = TRUE)
my.df$yearMonth <- factor(as.yearmon(my.df$timestamp))
my.df$week <- as.numeric(format(as.POSIXct(my.df$timestamp), "%W"))
my.plotData <- ddply(my.df, .(yearMonth), transform, monthWeek = 1 + week - min(week))

my.p1 <- ggplot(my.plotData, aes(monthWeek, weekdayFactor, fill = my.plotData$score)) +
  geom_tile(colour = "white") +
  facet_grid(year(my.plotData$timestamp) ~ monthFactor) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("Týždeň mesiaca") + ylab("Dni v týždni") + labs(fill = "Skóre") +
  theme(
    panel.grid.major = element_line(colour = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey"),
    panel.background = element_rect(fill = "white", color = "black")
  )


my.offset <- 61 * MPD
my.range <- 1:672
my.p2 <- ggplot() + geom_line(aes(x = my.range + my.offset, y = load, color = final), size = 1, data = my.df[my.range + my.offset, ]) +
  xlab("Poradie meraní (začiatok decembra)") + ylab("Spotreba elektrickej energie v kW") + labs(color = "Skóre") +
  scale_colour_gradient(low = "black", high = "red") +
  theme(
    panel.background = element_rect(fill = "white", color = "black")
  )

grid.arrange(my.p1, my.p2, nrow = 2)
