for (f in c("1000.csv", "1001.csv", "1002.csv", "1003.csv", "1004.csv", "1005.csv", "1006.csv", "1009.csv", "1013.csv", "1014.csv", "1015.csv")){
  f <- paste("~/r/fiit-dp/data/ireland/", f, sep = "")

  if (!file.exists(f)) {
    next
  }

  train <- loadDataset(f)
  print(f)

  train$date <- strftime(train$timestamp, "%Y-%m-%d")
  freq <- max(aggregate(load ~ date, data = train, FUN = length)$load)
  train$date <- NULL

  start <- 1
  step <- freq * WEEK * 1
  end <- freq * WEEK * 2

  while (end < nrow(train) && start > 0) {
    res = AnomalyDetectionVec(train$load[start:end], max_anoms = 0.1, direction = "both", plot = TRUE, period = freq, longterm_period = freq * WEEK)
    plot(res$plot)
    Sys.sleep(0)

    i <- readline(prompt="Press [enter] to continue")

    if (i == "n") {
      start <- start + step
      end <- end + step
      next
    }
    if (i == "p") {
      start <- start - step
      end <- end - step
      next
    }

    if (i == "+") {
      step <- floor(step / 2)
      freq <- floor(freq / 2)
      end <- start + floor(end - start) / 2
      next
    }
    if (i == "-") {
      step <- step * 2
      freq <- freq * 2
      end <- start + floor(end - start) * 2
      next
    }

    if (i == "N" || i == "q" ) {
      break
    }

    start <- start + step
    end <- end + step
  }
  if (exists("i") && i == "q" ) {
    break
  }
}
