# Prepare dataset
IRELAND <- loadIrelandIntoDF("~/data/load/")
COLUMNS <- colnames(IRELAND)
DATACOLUMNS <- colnames(IRELAND)[c(8:length(colnames(IRELAND)))]
METADATACOLUMNS <- colnames(IRELAND)[c(1:7)]
IRELAND[DATACOLUMNS] <- lapply(lapply(IRELAND[DATACOLUMNS], unlist), as.double)
write.csv2(IRELAND, "~/data/ireland.csv", row.names = FALSE)

# Load variables
IRELAND <- read.csv2("~/data/ireland.csv")
IRELAND <- IRELAND[with(IRELAND, order(timestamp)), ]
IRELAND$id <- NULL
IRELAND <- IRELAND[241:nrow(IRELAND), ]
IRELAND <- IRELAND[1:(nrow(IRELAND)-240), ]
row.names(IRELAND) <- NULL
COLUMNS <- colnames(IRELAND)
DATACOLUMNS <- colnames(IRELAND)[c(7:length(colnames(IRELAND)))]
METADATACOLUMNS <- colnames(IRELAND)[c(1:6)]
write.csv2(IRELAND, "~/data/ireland.csv", row.names = FALSE)

if(!exists("IRELAND"))
  IRELAND <- read.csv2("~/data/ireland.csv", stringsAsFactors = FALSE)
WEEKS <- round(nrow(IRELAND) / (MPD * WEEK))


# IRELAND <- loadIrelandIntoDF("~/data/load/")


CLUSTERING_WORKDAYS <- movingWindowWorkdays(windowSize = 2, clusterCount = 25, distanceMetric = "gak")
CLUSTERING_HOLIDAYS <- movingWindowHolidays(windowSize = 2, clusterCount = 25, distanceMetric = "gak")

SUSPICIOUS_WORKDAYS <- scoreSuspiciousTS(CLUSTERING_WORKDAYS, minorPenalty = c(0.1, 0.25))
SUSPICIOUS_HOLIDAYS <- scoreSuspiciousTS(CLUSTERING_HOLIDAYS, minorPenalty = c(0.1, 0.25))


ANOMALIES_WORKDAYS_AGG <- anomalyDetectionWrapper(SUSPICIOUS_WORKDAYS, list(aggregations = c(3, 4), numberOfAnomalies = c(0.49), alphaOfAnomalies = c(0.001)))
ANOMALIES_WORKDAYS <- anomalyDetectionWrapper(SUSPICIOUS_WORKDAYS, list(aggregations = c(0, 1, 2), numberOfAnomalies = c(0.05, 0.1), alphaOfAnomalies = c(0.05)))
ANOMALIES_HOLIDAYS <- anomalyDetectionWrapper(SUSPICIOUS_WORKDAYS, list(aggregations = c(3, 4), numberOfAnomalies = c(0.49), alphaOfAnomalies = c(0.001)), computeHolidays = TRUE)


current <- SUSPICIOUS_WORKDAYS[1]
dataset <- IRELAND[c(METADATACOLUMNS, current)]
result <- ANOMALIES_WORKDAYS[[current]]
step <- 480
for (i in seq(1, nrow(dataset), by = step)) {
  s <- i
  e <- i + step - 1

  df <- data.frame(list(
    timestamp = as.character(dataset[s:e, "timestamp"]),
    load = dataset[s:e, current],
    points = dataset[s:e, current],
    score = rowSums(result[s:e, ])
  ), stringsAsFactors = FALSE)
  df$points[df$score <= 0] <- NA

  p <- plot_ly(df, x = ~timestamp, y = ~load, name = 'load', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~points, name = 'anomalies', mode = 'markers', color = ~score)

  print(p)

  Sys.sleep(2)
}
