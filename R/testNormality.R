#This source has been written by Nagdev Amruthnath as a part of Research by Nagdev Amruthnath
#' @rdname TestNormality
#' @title Test normality
#' @description This function performs Shapiro-Wilk test is used to test the normality of the sample population.
#' 
#' Null Hypothesis H0 for this test is: the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality
#' @param data input the dataset that was used for clustering
#' @return returns a list with normality test results and p-values
#' @examples
#' # this code will run
#'
#' # data(iris)
#' # data = iris[,1:4]
#'
#' # cluster = kmeans(data,4)$cluster
#'
#' # TestNormality(data)
#' @export


TestNormality = function(data){
  #Required Libraries
  library(vegan)
  library(foreach)
  library(parallelDist)
  library(caret)
  # test for normality
  res.aov3 = aov(as.numeric(cluster) ~ .,
                 data)
  # Extract the residuals
  aov_residuals = residuals(object = res.aov3)
  # Run Shapiro-Wilk test
  testResults = shapiro.test(aov_residuals)
  
  return(testResults)
  
}