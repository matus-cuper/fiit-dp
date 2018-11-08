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

groupByAddColumn <- function(dataset, groupSize) {
  dataset$group <- c(1:nrow(dataset)) %% groupSize + 1
  return(dataset)
}
