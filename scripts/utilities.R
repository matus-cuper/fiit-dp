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

# Accept list of object, return them as named data.frame
unlistAndTransform <- function(dataset, dimensions, columns.names, rows.names = DATACOLUMNS) {
  df <- unlist(dataset, use.names = FALSE)
  dim(df) <- dimensions
  df <- data.frame(df)
  names(df) <- columns.names
  rownames(df) <- rows.names

  return(df)
}

# Compute cluster validation index for given data.frame
computeCVI <- function(df, clusterCount = 10) {
  clu <- tsclust(
    t(data.frame(lapply(df[, -1:-2], norm_fun))),
    k = clusterCount,
    distance = "dtw_basic",
    args = tsclust_args(dist = list(window.size = clusterCount))
  )
  return(cvi(clu))
}
