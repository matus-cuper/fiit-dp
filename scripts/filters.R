filterByDay <- function(dataset, selectDays) {
  if ("day" %in% names(dataset))
    return(dataset[dataset$day %in% selectDays, ])
  return(dataset[(as.POSIXlt(dataset$timestamp)$wday + 1) %in% selectDays, ])
}

filterByHoliday <- function(dataset, isHoliday) {
  if ("holiday" %in% names(dataset))
    return(dataset[dataset$holiday == isHoliday, ])
  return(dataset[(as.POSIXlt(dataset$timestamp)$wday + 1) %in% isHoliday, ])
}

filterWeekdays <- function(dataset) {
  return(filterByDay(dataset, 1:5))
}

filterWeekends <- function(dataset) {
  return(filterByDay(dataset, 6:7))
}

filterWorkdays <- function(dataset) {
  return(filterByHoliday(dataset, 0))
}

filterHolidays <- function(dataset) {
  return(filterByHoliday(dataset, 1))
}
