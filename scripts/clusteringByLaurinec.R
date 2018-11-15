# Expected input dataset should contains file names stored as column names, data should represents load

clusteringLaurinecByMean <- function(dataset, clusterRangeMin = 2, clusterRangeMax = 7) {
  freq <- getFrequency(dataset)

  dataset$timestamp <- NULL
  result <- data.frame(matrix(0, nrow = length(colnames(dataset)), ncol = 0))
  result$filename <- colnames(dataset)
  dataset <- t(dataset)

  # Normalize dataset
  data_seasprof <- repr_matrix(dataset, func = repr_seas_profile, args = list(freq = freq, func = mean), normalise = TRUE, func_norm = norm_z)

  # Find optimal number of clusters
  clusterings <- lapply(c(clusterRangeMin:clusterRangeMax), function(x) pam(data_seasprof, x))
  DB_values <- sapply(seq_along(clusterings), function(x) intCriteria(data_seasprof, as.integer(clusterings[[x]]$clustering), c("Davies_Bouldin")))

  # Plot internal evaluation
  plot(ggplot(data.table(Clusters = clusterRangeMin:clusterRangeMax, DBindex = unlist(DB_values)), aes(Clusters, DBindex)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         theme_bw())

  input <- readline(prompt="Enter number of clusters ")
  numberOfCluster <- as.numeric(input)
  result$clusterId <- clusterings[[numberOfCluster]]$clustering

  # prepare data for plotting
  data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[numberOfCluster]]$clustering), data_seasprof)))
  data_plot[, Time := rep(1:ncol(data_seasprof), each = nrow(data_seasprof))]
  data_plot[, ID := rep(1:nrow(data_seasprof), ncol(data_seasprof))]

  # prepare medoids
  centers <- data.table(melt(clusterings[[numberOfCluster]]$medoids))
  setnames(centers, c("Var1", "Var2"), c("class", "Time"))
  centers[, ID := class]

  # plot the results
  plot(ggplot(data_plot, aes(Time, value, group = ID)) +
         facet_wrap(~class, ncol = 2, scales = "free_y") +
         geom_line(color = "grey10", alpha = 0.65) +
         geom_line(data = centers, aes(Time, value), color = "firebrick1", alpha = 0.80, size = 1.2) +
         labs(x = "Time", y = "Load (normalised)") +
         theme_bw())

  return(result)
}

clusteringLaurinecByGAM <- function(dataset, clusterRangeMin = 2, clusterRangeMax = 7, windowSize = 7) {
  freq <- getFrequency(dataset)

  dataset$timestamp <- NULL
  result <- data.frame(matrix(0, nrow = length(colnames(dataset)), ncol = 0))
  result$filename <- colnames(dataset)
  dataset <- t(dataset)

  # Normalize dataset
  data_gam <- repr_matrix(dataset, func = repr_gam, args = list(freq = c(freq, freq * windowSize)), normalise = TRUE, func_norm = norm_z)

  # Find optimal number of clusters
  clusterings <- lapply(c(clusterRangeMin:clusterRangeMax), function(x) pam(data_gam, x))
  DB_values <- sapply(seq_along(clusterings), function(x) intCriteria(data_gam, as.integer(clusterings[[x]]$clustering), c("Davies_Bouldin")))

  # Plot internal evaluation
  plot(ggplot(data.table(Clusters = clusterRangeMin:clusterRangeMax, DBindex = unlist(DB_values)),aes(Clusters, DBindex)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         theme_bw())

  input <- readline(prompt="Enter number of clusters ")
  numberOfCluster <- as.numeric(input)
  result$clusterId <- clusterings[[numberOfCluster]]$clustering

  # prepare data for plotting
  data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[numberOfCluster]]$clustering), data_gam)))
  data_plot[, Time := rep(1:ncol(data_gam), each = nrow(data_gam))]
  data_plot[, ID := rep(1:nrow(data_gam), ncol(data_gam))]

  # prepare medoids
  centers <- data.table(melt(clusterings[[numberOfCluster]]$medoids))
  setnames(centers, c("Var1", "Var2"), c("class", "Time"))
  centers[, ID := class]

  # plot the results
  plot(ggplot(data_plot, aes(Time, value, group = ID)) +
         facet_wrap(~class, ncol = 2, scales = "free_y") +
         geom_line(color = "grey10", alpha = 0.65) +
         geom_line(data = centers, aes(Time, value), color = "firebrick1", alpha = 0.80, size = 1.2) +
         geom_vline(xintercept = 46, color = "dodgerblue2",  size = 1.4, linetype = 5, alpha = 0.8) +
         labs(x = "Time", y = "Load (normalised)") +
         theme_bw())

  return(result)
}

clusteringLaurinecByDFT <- function(dataset, clusterRangeMin = 2, clusterRangeMax = 7) {
  freq <- getFrequency(dataset)

  dataset$timestamp <- NULL
  result <- data.frame(matrix(0, nrow = length(colnames(dataset)), ncol = 0))
  result$filename <- colnames(dataset)
  dataset <- t(dataset)

  # Normalize dataset
  data_dft <- repr_matrix(elec_load, func = repr_dft, args = list(coef = freq), normalise = TRUE, func_norm = norm_z)

  # Find optimal number of clusters
  clusterings <- lapply(c(clusterRangeMin:clusterRangeMax), function(x) pam(data_dft, x))
  DB_values <- sapply(seq_along(clusterings), function(x) intCriteria(data_dft, as.integer(clusterings[[x]]$clustering), c("Davies_Bouldin")))

  # Plot internal evaluation
  plot(ggplot(data.table(Clusters = clusterRangeMin:clusterRangeMax, DBindex = unlist(DB_values)), aes(Clusters, DBindex)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         theme_bw())

  input <- readline(prompt="Enter number of clusters ")
  numberOfCluster <- as.numeric(input)
  #result$clusterId <- clusterings[[numberOfCluster]]$clustering

  # prepare data for plotting
  data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[numberOfCluster]]$clustering), data_dft)))
  data_plot[, Time := rep(1:ncol(data_dft), each = nrow(data_dft))]
  data_plot[, ID := rep(1:nrow(data_dft), ncol(data_dft))]

  # prepare medoids
  centers <- data.table(melt(clusterings[[numberOfCluster]]$medoids))
  setnames(centers, c("Var1", "Var2"), c("class", "Time"))
  centers[, ID := class]

  # plot the results
  plot(ggplot(data_plot, aes(Time, value, group = ID)) +
         facet_wrap(~class, ncol = 2, scales = "free_y") +
         geom_line(color = "grey10", alpha = 0.65) +
         geom_line(data = centers, aes(Time, value), color = "firebrick1", alpha = 0.80, size = 1.2) +
         labs(x = "Time", y = "Load (normalised)") +
         theme_bw())

  return(result)
}

clusteringLaurinecByClip <- function(dataset, clusterRangeMin = 2, clusterRangeMax = 7) {
  freq <- getFrequency(dataset)

  dataset$timestamp <- NULL
  result <- data.frame(matrix(0, nrow = length(colnames(dataset)), ncol = 0))
  result$filename <- colnames(dataset)
  dataset <- t(dataset)

  # Normalize dataset
  data_feaclip <- repr_matrix(elec_load, func = repr_feaclip, windowing = TRUE, win_size = freq)

  # Find optimal number of clusters
  clusterings <- lapply(c(clusterRangeMin:clusterRangeMax), function(x) pam(data_feaclip, x))
  DB_values <- sapply(seq_along(clusterings), function(x) intCriteria(data_feaclip, as.integer(clusterings[[x]]$clustering), c("Davies_Bouldin")))

  # Plot internal evaluation
  plot(ggplot(data.table(Clusters = clusterRangeMin:clusterRangeMax, DBindex = unlist(DB_values)), aes(Clusters, DBindex)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         theme_bw())

  input <- readline(prompt="Enter number of clusters ")
  numberOfCluster <- as.numeric(input)
  #result$clusterId <- clusterings[[numberOfCluster]]$clustering

  # prepare data for plotting
  data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[numberOfCluster]]$clustering), data_feaclip)))
  data_plot[, Time := rep(1:ncol(data_feaclip), each = nrow(data_feaclip))]
  data_plot[, ID := rep(1:nrow(data_feaclip), ncol(data_feaclip))]

  # prepare medoids
  centers <- data.table(melt(clusterings[[numberOfCluster]]$medoids))
  setnames(centers, c("Var1", "Var2"), c("class", "Time"))
  centers[, ID := class]

  # plot the results
  plot(ggplot(data_plot, aes(Time, value, group = ID)) +
         facet_wrap(~class, ncol = 2, scales = "free_y") +
         geom_line(color = "grey10", alpha = 0.65) +
         geom_line(data = centers, aes(Time, value), color = "firebrick1", alpha = 0.80, size = 1.2) +
         labs(x = "Time", y = "Load (normalised)") +
         theme_bw())

  return(result)
}
