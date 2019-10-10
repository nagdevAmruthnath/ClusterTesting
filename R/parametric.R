#This source has been written by Nagdev Amruthnath as a part of Research by Nagdev Amruthnath
#' @rdname nonParametric
#' @title Perform non-parametric test
#' @description This function performs non-parametric test on clustered data. Name your cluster column as 'cluster'.
#' @param data input the dataset that was used for clustering along with cluster results in column called cluster
#' @return returns a list with non-parametric test results and p-values
#' @examples
#' # this code will run
#'
#' # data(iris)
#' # data = iris[,1:4]
#'
#' # cluster = kmeans(data,4)$cluster
#'
#' # nonParametric(data)
#' @export


nonParametric = function(data){
  #Required Libraries
  library(vegan)
  library(foreach)
  library(parallelDist)
  library(caret)
  sampleData = data
  #make cluster as factor
  sampleData$cluster = as.factor(sampleData$cluster)
  
  #one hot encoding over factor ie., cluster
  dmy = dummyVars(" ~ cluster", data = sampleData)
  oneHotCluster = data.frame(predict(dmy, newdata = sampleData))
  
  #calculate distance matrix
  distance1 <- parDist(as.matrix(oneHotCluster),
                       method = "euclidean")
  
  model1 = betadisper(distance1,
                      sampleData$cluster)
  
  return(model1)
  
}

#' @rdname parametric
#' @example 
#' # this code will run
#'
#' data(iris)
#' data = iris[,1:4]
#'
#' cluster = kmeans(data,4)$cluster
#'
#' parametric(data)
#' @export
parametric = function (data){
  prametricResults = lm(cluster ~ . , data)
  return(prametricResults)
}