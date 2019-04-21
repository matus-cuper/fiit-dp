filterAnomalies <- function(df, lambda = 1.5) {
  c1 <- quantile(df$sum_1, probs = 0.75) + lambda * IQR(df$sum_1)
  c2 <- quantile(df$sum_1, probs = 0.25) - lambda * IQR(df$sum_1)
  c3 <- quantile(df$cross., probs = 0.75) + lambda * IQR(df$cross.)

  result <- c(
    rownames(df[which(df$sum_1 > c1), ]),
    rownames(df[which(df$sum_1 < c2), ]),
    rownames(df[which(df$cross. > c3), ])
  )

  return(unique(sort(result)))
}

scoreSuspiciousTS <- function(dataset, minorPenalty = c(0.10)) {
  scores <- list()
  for (i in 1:length(dataset)) {
    subscores <- c(rep(1, length(table(dataset[[i]]$cluster))))
    for (p in minorPenalty) {
      q <- as.numeric(quantile(table(dataset[[i]]$cluster), probs = p))
      subscores[table(dataset[[i]]$cluster) < q] <- subscores[table(dataset[[i]]$cluster) < q] + 1
    }
    scores[[i]] <- subscores[dataset[[i]]$cluster] * dataset[[i]]$cldist / dataset[[i]]$clusinfo$av_dist[dataset[[i]]$cluster]
  }
  scores <- data.frame(matrix(unlist(scores), nrow = length(dataset), byrow = TRUE))
  names(scores) <- NULL

  return(scores)
}

filterSuspiciousTS <- function(dataset, columns, q1 = 0.10, q2 = 0.33) {
  suspiciousTS <- c()
  for (i in 1:length(dataset)) {
    clustersCount <- table(dataset[[i]]$cluster)
    quantile1 <- as.numeric(quantile(clustersCount, probs = q1))
    suspicious <- data.frame(t(dataset[[i]]$cluster))
    names(suspicious) <- columns
    suspiciousTS <- c(
      suspiciousTS,
      names(suspicious[dataset[[i]]$cluster %in% names(clustersCount[clustersCount < quantile1])])
    )
  }

  suspiciousCount <- table(table(suspiciousTS))
  quantile2 <- quantile(suspiciousCount, probs = q2)

  return(suspiciousTS[table(suspiciousTS) %in% names(suspiciousCount[suspiciousCount < quantile2])])
}

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
