# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")
# install.packages("zoo")

library(AnomalyDetection)


setwd("~/r/fiit-dp/data/ddc/data/unused/")

for (f in c("38_9792.csv", "38_52481.csv", "38_52477.csv", "38_52476.csv", "38_52327.csv", "38_52480.csv", "38_52474.csv", "38_52473.csv", "38_52478.csv", "38_52328.csv", "38_9789.csv", "38_9797.csv", "38_52468.csv", "38_9799.csv", "38_9788.csv", "38_52472.csv", "38_0.csv", "38_9787.csv", "38_52323.csv", "38_52326.csv", "38_52470.csv", "38_10126.csv", "38_9790.csv", "38_9798.csv", "38_9791.csv", "38_52482.csv", "38_9795.csv", "38_52333.csv", "38_52329.csv", "38_9801.csv", "38_9794.csv", "38_52471.csv", "38_52324.csv", "38_9796.csv", "38_52325.csv", "38_52479.csv", "38_52332.csv", "38_52475.csv", "38_52322.csv", "38_9793.csv", "38_52467.csv", "38_52469.csv", "38_9719.csv", "38_9764.csv", "38_9685.csv", "896.csv", "38_9705.csv", "38_56031.csv", "38_9708.csv", "38_9684.csv", "38_56728.csv", "38_9741.csv", "38_56734.csv", "38_56727.csv", "884.csv", "38_9756.csv", "38_9739.csv", "38_9740.csv", "38_9680.csv", "38_9766.csv", "38_9749.csv", "38_10117.csv", "38_56739.csv", "38_56731.csv", "38_9730.csv", "38_9742.csv", "38_52306.csv", "38_9695.csv", "38_9681.csv", "38_56736.csv", "38_9761.csv", "38_9712.csv", "38_9729.csv", "38_9715.csv", "38_9683.csv", "38_10119.csv", "38_9752.csv", "38_9759.csv", "38_9682.csv", "38_56034.csv", "38_56738.csv", "38_9733.csv", "38_9753.csv", "38_9754.csv", "38_10115.csv", "38_56741.csv", "38_10112.csv", "38_56729.csv", "38_9732.csv", "38_56032.csv", "38_56744.csv", "38_9738.csv", "38_56732.csv", "38_56030.csv", "38_10123.csv", "38_9716.csv", "38_9710.csv", "38_9727.csv", "38_9763.csv", "38_9751.csv", "38_10121.csv", "38_10114.csv", "38_9760.csv", "38_9704.csv", "38_10113.csv", "38_52375.csv", "38_10107.csv", "38_9679.csv", "38_9726.csv", "38_56735.csv", "38_56742.csv", "38_9743.csv", "38_56743.csv", "38_56730.csv", "38_9737.csv", "38_59654.csv", "38_10110.csv", "935.csv", "38_9711.csv", "38_9699.csv", "878.csv", "38_9693.csv", "38_9758.csv", "38_10109.csv", "38_9678.csv", "38_9707.csv", "38_9717.csv", "38_59804.csv", "38_9728.csv", "38_10116.csv", "38_10118.csv", "38_10108.csv", "38_9709.csv", "38_9720.csv", "38_10106.csv", "38_9703.csv", "38_56737.csv", "38_56033.csv", "38_9757.csv", "38_9706.csv", "925.csv", "38_9718.csv", "38_9747.csv", "38_9731.csv", "38_9713.csv", "38_56740.csv", "887.csv", "38_9698.csv", "38_9714.csv", "38_9688.csv", "38_9694.csv", "38_52379.csv", "38_10111.csv", "38_10124.csv", "38_9765.csv", "872.csv", "38_9748.csv", "38_9762.csv", "38_10125.csv", "38_10120.csv", "38_9702.csv", "38_9755.csv", "38_56733.csv", "38_9686.csv", "38_10122.csv", "38_9696.csv", "38_9697.csv", "38_9725.csv", "930.csv", "38_2.csv", "38_1.csv", "881.csv", "334_61.csv", "38_9687.csv", "911.csv", "938.csv", "38_56973.csv", "928.csv", "2.csv", "863.csv", "902.csv", "920.csv", "234_203.csv", "38_9689.csv", "869.csv", "875.csv", "890.csv")){
  
  start <- 1
  step <- 1344
  end <- 2*1344
  
  if (!file.exists(f))
    next

  train <- read.csv2(f, sep = ',')
  train$Values <- as.numeric(train$Values)
  train$Timestamp <- as.POSIXct(train$Timestamp)
  train[train$Values == 1, ]$Values <- NA
  train <- train[!is.na(train$Values),]
#  train$Values <- diff(train$Values)[1:nrow(train)]
  
  while (end < nrow(train)) {
    plot(ts(train$Values[start:end]))
    i <- readline(prompt="Press [enter] to continue")
    if (i == "x" ) {
      print(f)
      break
    }
    if (i == "n" || i == "q" ) {
      break
    }
    start <- start + step
    end <- end + step
  }
  if (exists("i") && i == "q" ) {
    break
  }
}
