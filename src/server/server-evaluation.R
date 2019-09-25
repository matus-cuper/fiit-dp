week <- 10
base <- (week - 1) * MPD * WEEK

IRELAND_SYNTHETIC <- data.frame(IRELAND)

swapped <- list()
for (i in 1:10) {
  k <- sample(c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12), 4)
  s <- base + (k - 1) * MPD + 1
  e <- base + k * MPD
  swap <- IRELAND_SYNTHETIC[s[1]:e[1], tup[i, 1]]
  IRELAND_SYNTHETIC[s[1]:e[1], tup[i, 1]] <- IRELAND_SYNTHETIC[s[2]:e[2], tup[i, 2]]
  IRELAND_SYNTHETIC[s[2]:e[2], tup[i, 2]] <- swap
  swap <- IRELAND_SYNTHETIC[s[3]:e[3], tup[i, 1]]
  IRELAND_SYNTHETIC[s[3]:e[3], tup[i, 1]] <- IRELAND_SYNTHETIC[s[4]:e[4], tup[i, 2]]
  IRELAND_SYNTHETIC[s[4]:e[4], tup[i, 2]] <- swap
  
  swapped[[i[1]]] <- k
}

week <- 10
s <- (week - 1) * (MPD * WEEK) + 1
e <- WEEK * MPD * (week + 2 - 1)

CLUSTERING_SYNTHETIC <- list()
for (i in 1:5)
  CLUSTERING_SYNTHETIC[[i]] <- movingWindowWorkdaysSynthetic(windowSize = 2, clusterCount = 25, distanceMetric = "gak")

SUSPICIOUS_SYNTHETIC <- scoreSuspiciousReal(CLUSTERING_SYNTHETIC)
SUSPICIOUS_SYNTHETIC <- sapply(SUSPICIOUS_SYNTHETIC, function(x) mean(x))

LOESS_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION)
  LOESS_SYNTHETIC[[o]] <- predict(loess(IRELAND_SYNTHETIC[[o]] ~ c(1:nrow(IRELAND_SYNTHETIC)), span = 0.0006))

TWITTER_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION)
  TWITTER_SYNTHETIC[[o]] <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND_SYNTHETIC$timestamp), load = LOESS_SYNTHETIC[[o]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = FALSE)

SMOOTH_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION) {
  df <- merge(x = data.frame(timestamp = IRELAND$timestamp), y = TWITTER_SYNTHETIC[[o]]$anoms, by = "timestamp", all.x = TRUE)
  df$anoms[!is.na(df$anoms)] <- 1
  df$anoms[is.na(df$anoms)] <- 0
  SMOOTH_SYNTHETIC[[o]] <- predict(loess(df$anoms ~ c(1:length(df$anoms)), span = 0.001))
}

week <- 10
s <- (week - 1) * (MPD * WEEK) + 1
e <- WEEK * MPD * (week + 2 - 1)

RESULT_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION)
  RESULT_SYNTHETIC[[o]] <- SUSPICIOUS_SYNTHETIC[[o]] + SMOOTH_SYNTHETIC[[o]][s:e]

TWITTER_REAL_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION)
  TWITTER_REAL_SYNTHETIC[[o]] <- AnomalyDetectionTs(data.frame(timestamp = as.POSIXlt(IRELAND_SYNTHETIC$timestamp), load = IRELAND_SYNTHETIC[[o]]), max_anoms = 0.49, alpha = 0.001, direction = "both", plot = FALSE)


week <- 10
s <- (week - 1) * (MPD * WEEK) + 1
e <- WEEK * MPD * (week + 2 - 1)

VYSLEDOK_SHESD <- list()
for (o in SYNTHETIC_SELECTION) {
  df <- merge(x = data.frame(IRELAND[c('X2812.csv', 'timestamp')]), y = TWITTER_REAL_SYNTHETIC$X2812.csv$anoms, by = 'timestamp', all.x = TRUE)
  VYSLEDOK_SHESD[[o]] <- data.frame(N = 1:(14*MPD), predicted = as.integer(!is.na(df$anoms[s:e])), real = VYSLEDOK_CASA[[o]]$real)
}

save(RESULT_SYNTHETIC, MPD, swapped, s, e, tup, SYNTHETIC_SELECTION, file = 'result_synthetic.rds')
VYSLEDOK_CASA <- list()
for (o in SYNTHETIC_SELECTION) {
  VYSLEDOK_CASA[[o]] <- data.frame(N = 1:(14*MPD), predicted = as.integer(RESULT_SYNTHETIC[[o]] > 1.35), real = rep(0, 14*MPD))
  
  s <- (swapped[[match(o, tup[, 1])]][1] - 1) * MPD + 1
  e <- MPD * swapped[[match(o, tup[, 1])]][1]
  VYSLEDOK_CASA[[o]]$real[s:e] <- 1
  
  s <- (swapped[[match(o, tup[, 1])]][3] - 1) * MPD + 1
  e <- MPD * swapped[[match(o, tup[, 1])]][3]
  VYSLEDOK_CASA[[o]]$real[s:e] <- 1
}

RESULT_CASA <- data.frame(predicted = unlist(lapply(VYSLEDOK_CASA, function(x) x$predicted), use.names = FALSE), 
                          real = unlist(lapply(VYSLEDOK_CASA, function(x) x$real), use.names = FALSE))
RESULT_SHESD <- data.frame(predicted = unlist(lapply(VYSLEDOK_SHESD, function(x) x$predicted), use.names = FALSE), 
                           real = unlist(lapply(VYSLEDOK_SHESD, function(x) x$real), use.names = FALSE))

RESULT_CASA$match <- as.integer(RESULT_CASA$predicted == RESULT_CASA$real)
RESULT_SHESD$match <- as.integer(RESULT_SHESD$predicted == RESULT_SHESD$real)

table(pre = RESULT_CASA$predicted, real = RESULT_CASA$real)
table(pre = RESULT_SHESD$predicted, real = RESULT_SHESD$real)

F1_Score(RESULT_CASA$real, RESULT_CASA$predicted, positive = 1)
F1_Score(RESULT_SHESD$real, RESULT_SHESD$predicted, positive = 1)



x1 <- c(RESULT_CASA$match)
y1 <- c(RESULT_SHESD$match)
y1 <- c(RESULT_SDEWMA$match)
y1 <- c(RESULT_PEWMA$match)
y1 <- c(RESULT_KNN$match)

table(x1)
table(y1)

####################### test signifikancie ... hypoteza h0 = porovnavane vzorky maju rovanku distribuciu, 
#                                                       h1 = porovnavane vzorky nemaju rovanku distribuciu a priemer distribucie x1 je posunuty vzhladom na priemer distribucie y1 smerom do prava... teda x1 ma castejsie pravdu

res1 <- wilcox.test(x = x1, y = y1, alternative = "greater", paired = TRUE)

######## p hodnota 
res1$p.value

res1$p.value <= 0.05   ### ak je TRUE, tak je nasa metoda signifikantne lepsia

2*TP / (2*TP + FP + FN)



week <- 10
s <- (week - 1) * (MPD * WEEK) + 1
e <- WEEK * MPD * (week + 2 - 1)

library(otsad)

KNN_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION) {
  t0 <- Sys.time()
  df <- data.frame(timestamp = IRELAND_SYNTHETIC$timestamp[1:e], value = IRELAND_SYNTHETIC[[o]][1:e])
  
  KNN_SYNTHETIC[[o]] <- CpKnnCad(
    data = df$value,
    n.train = s,
    threshold = 1,
    l = 48,
    k = 27,
    ncm.type = 'ICAD',
    reducefp = TRUE
  )
  KNN_SYNTHETIC[[o]] <- cbind(df, KNN_SYNTHETIC[[o]])
  print(Sys.time() - t0)
}

RESULT_KNN <- data.frame(predicted = as.integer(unlist(lapply(KNN_SYNTHETIC, function(x) x$is.anomaly[s:e])), use.names = FALSE),
                         real = RESULT_CASA$real)
RESULT_KNN$match <- as.integer(RESULT_KNN$predicted == RESULT_KNN$real)
table(pre = RESULT_KNN$predicted, real = RESULT_KNN$real)
F1_Score(RESULT_KNN$real, RESULT_KNN$predicted) * 100


  
PEWMA_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION) {
  df <- data.frame(timestamp = IRELAND_SYNTHETIC$timestamp[1:e], value = IRELAND_SYNTHETIC[[o]][1:e])
  print(o)
  PEWMA_SYNTHETIC[[o]] <- OcpPewma(
    data = df$value,
    n.train = s,
    alpha0 = 0.8,
    beta = 0.1,
    l = 3
  )
  PEWMA_SYNTHETIC[[o]] <- cbind(df, PEWMA_SYNTHETIC[[o]])
}

RESULT_PEWMA <- data.frame(predicted = unlist(lapply(PEWMA_SYNTHETIC, function(x) x$is.anomaly[s:e]), use.names = FALSE),
                           real = RESULT_CASA$real)
RESULT_PEWMA$match <- as.integer(RESULT_PEWMA$predicted == RESULT_PEWMA$real)
table(pre = RESULT_PEWMA$predicted, real = RESULT_PEWMA$real)
F1_Score(RESULT_PEWMA$real, RESULT_PEWMA$predicted, positive = 1) * 100


SDEWMA_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION) {
  df <- data.frame(timestamp = IRELAND_SYNTHETIC$timestamp[1:e], value = IRELAND_SYNTHETIC[[o]][1:e])
  print(o)
  SDEWMA_SYNTHETIC[[o]] <- OcpSdEwma(
    data = df$value,
    n.train = s,
    threshold = 0.000075,
    l = 3
  )
  SDEWMA_SYNTHETIC[[o]] <- cbind(df, SDEWMA_SYNTHETIC[[o]])
}

RESULT_SDEWMA <- data.frame(predicted = unlist(lapply(SDEWMA_SYNTHETIC, function(x) x$is.anomaly[s:e]), use.names = FALSE),
                            real = RESULT_CASA$real)
RESULT_SDEWMA$match <- as.integer(RESULT_SDEWMA$predicted == RESULT_SDEWMA$real)
table(pre = RESULT_SDEWMA$predicted, real = RESULT_SDEWMA$real)
F1_Score(RESULT_SDEWMA$real, RESULT_SDEWMA$predicted) * 100





F1_Score(RESULT_CASA$real, RESULT_CASA$predicted, positive = 1)
F1_Score(RESULT_SHESD$real, RESULT_SHESD$predicted, positive = 1)
F1_Score(RESULT_PEWMA$real, RESULT_PEWMA$predicted, positive = 1)
F1_Score(RESULT_SDEWMA$real, RESULT_SDEWMA$predicted, positive = 1)

ConfusionMatrix(RESULT_CASA$real, RESULT_CASA$predicted)

KNN_LDCD_SYNTHETIC <- list()
for (o in SYNTHETIC_SELECTION) {
  t0 <- Sys.time()
  df <- data.frame(timestamp = IRELAND_SYNTHETIC$timestamp[1:e], value = IRELAND_SYNTHETIC[[o]][1:e])
  
  KNN_LDCD_SYNTHETIC[[o]] <- CpKnnCad(
    data = df$value,
    n.train = s,
    threshold = 1,
    l = 48,
    k = 27,
    ncm.type = 'LDCD',
    reducefp = TRUE
  )
  KNN_LDCD_SYNTHETIC[[o]] <- cbind(df, KNN_LDCD_SYNTHETIC[[o]])
  print(Sys.time() - t0)
}

RESULT_KNN_LDCD <- data.frame(predicted = as.integer(unlist(lapply(KNN_LDCD_SYNTHETIC, function(x) x$is.anomaly[s:e])), use.names = FALSE),
                         real = RESULT_CASA$real)
RESULT_KNN_LDCD$match <- as.integer(RESULT_KNN_LDCD$predicted == RESULT_KNN_LDCD$real)
table(pre = RESULT_KNN_LDCD$predicted, real = RESULT_KNN_LDCD$real)
F1_Score(RESULT_KNN_LDCD$real, RESULT_KNN_LDCD$predicted) * 100

all(RESULT_CASA$real == RESULT_SDEWMA$real)
RESULT_ALL <- data.frame(
  real = RESULT_CASA$real,
  CASA = RESULT_CASA$predicted,
  SHESD = RESULT_SHESD$predicted,
  PEWMA = RESULT_PEWMA$predicted,
  SDEWMA = RESULT_SDEWMA$predicted,
  KNN_CAD = RESULT_KNN$predicted,
  KNN_LDCD = RESULT_KNN_LDCD$predicted
)
