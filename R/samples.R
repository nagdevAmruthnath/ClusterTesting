#This source has been written by Nagdev Amruthnath as a part of Research by Nagdev Amruthnath
#' @rdname sampleSize
#' @title create sample sizes
#' @description Creates a sample size of 30 for each group. If in case the population group sample is less than 30 then, the sample size will be the minimum of group sample size.
#' @param data input the dataset that was used for clustering
#' @param cluster enter the cluster column of the data
#' @return returns a dataframe with equal samples
#' @examples
#' # this code will run
#'
#' # data(iris)
#' # data = iris[,1:4]
#'
#' # cluster = kmeans(data,4)$cluster
#'
#' # sampleSize(data, cluster)
#' @export




sampleSize = function(data, cluster){
  #Required Libraries
  library(vegan)
  library(foreach)
  library(parallelDist)
  library(caret)
  #get a minimum size of cluster for sampling
  minSize = min(table(cluster))
  if(minSize>=30){ minSize <- 30}

  #get equal sample size for the data
  scaledData = data.frame(data,cluster=cluster)

  sampleData = foreach(i=1:max(cluster), .combine = rbind) %do% {
                          set.seed(123)
                          data <- subset(scaledData,
                                         cluster==i)
                          #sample <- sample(nrow(data), size = minSize)
                          data[1:minSize,]
  }
  return(sampleData)
}






