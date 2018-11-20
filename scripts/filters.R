filterWeekdays <- function(dataset, skipDays = c(6, 7)) {
  return(dataset[!(as.POSIXlt(dataset$timestamp)$wday + 1) %in% skipDays, ])
}
