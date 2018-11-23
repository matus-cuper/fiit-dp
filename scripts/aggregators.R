# Groups data points next to each other and aggregate load
groupByAggregate <- function(dataset, groupSize) {
  # Sum load by generated group
  dataset$id <- ceiling(c(1:nrow(dataset)) / groupSize)
  agg <- aggregate(cbind(load) ~ id, data = dataset, sum)

  # Join dataset and rename columns
  dataset <- dataset[seq(1 + ceiling((groupSize / 2) - 1), nrow(dataset), groupSize), c("timestamp", "date", "time", "day", "holiday") ]
  dataset$load <- agg$load
  dataset <- dataset[, c(1, 6, 2, 3, 4, 5)]

  return(dataset)
}

# Add new column with group ID, where same group ID refers to first value of each window
groupByAddColumn <- function(dataset, windowSize) {
  dataset$group <- c(0:(nrow(dataset) - 1)) %% windowSize + 1
  return(dataset)
}
