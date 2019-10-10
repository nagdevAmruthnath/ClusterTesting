# ClusterTesting
An R package to test the integrity of the clusters developed using Machine Learning models. 

## Package installation
```
# install devtools
install.packages("devtools")

# install cluster testing package from github
devtools::install_github("nagdevAmruthnath/ClusterTesting")
```

## Example usage

The below examples of each function will explain how to use the functions. Iris data is used for this purpose. 

### Normality Test
Check the data if it follows normal distribution using shaprio-wilk test.

```
# load iris data set
data(iris)

# remove categorical column
data = iris[,1:4]

# cluster the data using k-means model and save the cluster information to list 
cluster = kmeans(data,4)$cluster

# perform normality test
TestNormality(data)

#	Shapiro-Wilk normality test
#
# data:  aov_residuals
# W = 0.82, p-value = 2.674e-12
```

### Create samples
Creates a sample size of 30 for each group. If in case the population group sample is less than 30 then, the sample size will be the minimum of group sample size.

```
sampleSize(data, cluster)

#    Sepal.Length Sepal.Width Petal.Length Petal.Width cluster
#1            5.1         3.5          1.4         0.2       1
#5            5.0         3.6          1.4         0.2       1
#6            5.4         3.9          1.7         0.4       1
#8            5.0         3.4          1.5         0.2       1
#11           5.4         3.7          1.5         0.2       1
```
### Test clustered data
This function uses data and cluster information to create a sample size for analysis. Then a normality test is performed to see if the data is normally distributed. If it is, then a parametric approach is uses to test the validity of the results. If not, then a non parameteric test is used to validate the results.

```
testClusters(data = data, cluster = cluster, conf = 0.95)

#[1] "There is a significant difference among group dispersion. Hence, we reject hull hypothesis. In postHoc test, there #is no significant difference among these cluster pairs 3-2 4-2 4-3 at your mentioned confidence level of 0.95"
```
Because there is a significant difference among the groups, we can use non-parametric test.

### Non-paramteric test
This function performs non-parametric test on clustered data. Name your cluster column as 'cluster'.

```
data$cluster = cluster

nonParametric(data)

#	Homogeneity of multivariate dispersions
#
#Call: betadisper(d = distance1, group = sampleData$cluster)
#
#No. of Positive Eigenvalues: 3
#No. of Negative Eigenvalues: 0
#
#Average distance to median:
#        1         2         3         4 
#2.022e-16 3.470e-16 2.633e-16 6.441e-16 
#
#Eigenvalues for PCoA axes:
#PCoA1 PCoA2 PCoA3 
#50.07 32.26 23.95
```