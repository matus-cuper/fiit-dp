if (!require("cluster")) install.packages("cluster")
if (!require("clusterCrit")) install.packages("clusterCrit")
if (!require("data.table")) install.packages("data.table")
if (!require("devtools")) install.packages("devtools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("TSrepr")) install.packages("TSrepr")
if (!require("zoo")) install.packages("zoo")

devtools::install_github("twitter/AnomalyDetection", quiet = TRUE)

library(AnomalyDetection)


WEEK <- 7


source("~/r/fiit-dp/scripts/loaders.R")
source("~/r/fiit-dp/scripts/aggregators.R")
source("~/r/fiit-dp/scripts/visualizators.R")
source("~/r/fiit-dp/scripts/analyzators.R")
source("~/r/fiit-dp/scripts/clusteringByLaurinec.R")





findAnomaliesSum <- function(dataset, days = 4 * WEEK, aggregations = c(0, 1, 2, 3, 4)) {
  freq <- max(aggregate(load ~ date, data = dataset, FUN = length)$load)
  datasetSize <- freq * days
  
  dataset <- dataset[1:datasetSize, ]
  anomalies <- data.frame(matrix(0, nrow = nrow(dataset), ncol = length(aggregations)))
  
  for (a in aggregations) {
    ratio <- 2**a
    # TODO create new func
    train <- groupByAggregate(dataset, ratio)
    train <- train[1:(datasetSize / ratio), ]
    
    res <- AnomalyDetectionVec(
      train$load,
      max_anoms=0.1,
      direction='both',
      plot=TRUE,
      period = freq / ratio,
      longterm_period = freq * WEEK / ratio
    )
    
    print(paste("period", freq / ratio))
    print(paste("long period", freq * WEEK / ratio))
    
    plot(ts(train$load, frequency = freq / ratio))
    # plot(res$plot)
    Sys.sleep(1)
    plot(res$plot)
    Sys.sleep(2)
    
    for (j in 1:ratio) {
      anomalies[res$anoms$index * ratio + j, i + 1] <- 1
    }
  }
  
  anomalies$score <- apply(anomalies, 1, FUN = function(x) sum(x))
  return(anomalies)
}