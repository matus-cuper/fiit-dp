# Extract frequency from data.frame just from timestamp column
getFrequency <- function(dataset) {
  df <- data.frame(list(
    timestamp = as.character(dataset$timestamp), 
    date = as.character(strptime(as.POSIXlt(dataset$timestamp), "%Y-%m-%d"))
  ))

  return(max(aggregate(timestamp ~ date, data = df, FUN = length)$timestamp))
}
