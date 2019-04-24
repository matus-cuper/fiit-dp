# Extract frequency from data.frame just from timestamp column
getFrequency <- function(dataset) {
  df <- data.frame(list(
    timestamp = as.character(dataset$timestamp), 
    date = as.character(strptime(as.POSIXlt(dataset$timestamp), "%Y-%m-%d"))
  ))

  return(max(aggregate(timestamp ~ date, data = df, FUN = length)$timestamp))
}

# Return upper bound of IQR rule
getIQRrule <- function(df) {
  return(df[df > quantile(df, probs = 0.75) + 1.5 * IQR(df)])
}
