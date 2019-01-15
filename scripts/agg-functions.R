computeCVI <- function(df, clusterCount = 10) {
  clu <- tsclust(
    t(data.frame(lapply(df[, -1:-2], norm_fun))),
    k = clusterCount,
    distance = "dtw_basic",
    args = tsclust_args(dist = list(window.size = clusterCount))
  )
  return(cvi(clu))
}
