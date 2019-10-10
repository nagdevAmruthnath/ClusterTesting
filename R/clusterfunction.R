#This source has been written by Nagdev Amruthnath as a part of Research by Nagdev Amruthnath

#Required Libraries
library(vegan)
library(foreach)
library(parallelDist)
library(caret)

sampleSize = function(data, cluster){
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

TestNormality = function(data){
  # test for normality
  res.aov3 = aov(as.numeric(cluster) ~ .,
                 data)
  # Extract the residuals
  aov_residuals = residuals(object = res.aov3)
  # Run Shapiro-Wilk test
  testResults = shapiro.test(aov_residuals)

  return(testResults)

}

nonParametric = function(data){
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

parametric = function (data){
  prametricResults = lm(cluster ~ . , data)
  return(prametricResults)
}

testClusters = function(data, cluster, conf){

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
