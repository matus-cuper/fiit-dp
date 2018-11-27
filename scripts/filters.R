filterWeekdays <- function(dataset, skipDays = c(6, 7)) {
  if ("day" %in% names(dataset))
    return(dataset[!dataset$day %in% skipDays, ])
  return(dataset[!(as.POSIXlt(dataset$timestamp)$wday + 1) %in% skipDays, ])
}
