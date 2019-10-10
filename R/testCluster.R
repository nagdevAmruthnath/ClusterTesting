#' @rdname testClusters
#' @title Test for Validity of Clusters using Hypothesis Testing
#' @description This function uses data and cluster information to create a sample size for analysis. Then a normality test is performed to see if the data is normally distributed. If it is, then a parametric approach is uses to test the validity of the results. If not, then a non parameteric test is used to validate the results.
#'
#' Non-Prametric: this uses the method proposed by Anderson(2006) to use distance matrix to test the variance among groups. Here for categorical groups, one-hot encoding approach is performed.
#' 
#' Parametric: this uses ANOVA procedure to and TukeyHSD test to validate the significance among clusters.
#' @param data a numerical data frame with data used for clustering analysis
#' @param cluster categorical vector with cluster information
#' @param conf confidence level for performing the test
#' @return returns a list with normality test results and p-values
#' @references 
#' Anderson, M.J. (2006) Distance-based tests for homogeneity of multivariate dispersions. Biometrics 62, 245-253.
#' Anderson, M.J., Ellingsen, K.E. & McArdle, B.H. (2006) Multivariate dispersion as a measure of beta diversity. Ecology Letters 9, 683-693
#' @examples
#' # this code will run
#'
#' # data(iris)
#' # data = iris[,1:4]
#'
#' # cluster = kmeans(data,4)$cluster
#'
#' # testClusters(data = data, cluster = cluster, conf = 0.95)
#' @export


testClusters = function(data, cluster, conf){
  #Required Libraries
  library(vegan)
  library(foreach)
  library(parallelDist)
  library(caret)
  # 
  #create samples first
  samples = sampleSize(data, cluster)
  
  #test for normality
  normalityResults = TestNormality(samples)
  
  #check to go with parametric or non parametric
  if(normalityResults$p.value < 0.1){
    testResults = nonParametric(samples)
    anovaTest = anova(testResults)
    if(anovaTest$`Pr(>F)`[1] < (1-conf)){
      tukeyResults = TukeyHSD(testResults, conf.level = conf)
      tukeyGroup = data.frame(tukeyResults$group)
      insignificant = subset.data.frame(tukeyGroup, p.adj > (1-conf))
      if(nrow(insignificant)>0) {
        results = paste0("There is a significant difference among group dispersion. Hence, we reject hull hypothesis. In postHoc test, there is no significant difference among these cluster pairs ", paste(unlist(row.names(insignificant)), collapse=' '), " at your mentioned confidence level of ", conf)
      }
      else{
        results = paste0("There is a significant difference among group dispersion. Hence, we reject hull hypothesis. Also, all the groups are different")
      }
    }
    else{
      results = paste("It was found that there is no significant difference between group dispersion. Hence, fail to reject hull hypothesis. ")
    }
  }
  else {
    testResults = parametric(samples)
    anovaTest = anova(testResults)
    if(anovaTest$`Pr(>F)`[1] < (1-conf)){
      samples$cluster = as.factor(samples$cluster)
      
      # These are the variable names:
      measurevar = "cluster"
      groupvars  = colnames(data)
      
      formula = as.formula(paste(paste(groupvars, collapse=" + "), measurevar, sep=" ~ "))
      
      aovModel =aov( formula, samples)
      tukeyResults = TukeyHSD(aovModel)
      tukeyGroup = data.frame(tukeyResults$cluster)
      insignificant = subset.data.frame(tukeyGroup, p.adj > (1-conf))
      if(nrow(insignificant)>0) {
        results = paste0("There is a significant difference among group means. Hence, we reject hull hypothesis. In postHoc test, there is no significant difference among these cluster pairs ", paste(unlist(row.names(insignificant)), collapse=' '), " at your mentioned confidence level of ", conf)
      }
      else{
        results = paste0("There is a significant difference among group means. Hence, we reject hull hypothesis. Also, all the groups are different")
      }
      
    }
    else{
      results = paste("It was found that there is no significant difference between group means. Hence, fail to reject hull hypothesis. ")
    }
    
    
  }
  return(results)
}