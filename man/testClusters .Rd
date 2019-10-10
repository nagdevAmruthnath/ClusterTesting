\name{testClusters}
\alias{testClusters}
\title{Test for Validity of Clusters using Hypothesis Testing}
\usage{
testClusters(data, cluster, conf)
}
\arguments{
   \item{data}{a numerical data frame with data used for clustering analysis}
   \item{cluster}{a categorical vector with cluster information}
   \item{conf}{confidence level for performing the test}
}
\description{
This function uses data and cluster information to create a sample size for analysis. Then a normality test is performed to see if the data is normally distributed. If it is, then a parametric approach is uses to test the validity of the results. If not, then a non parameteric test is used to validate the results.

Non-Prametric: this uses the method proposed by Anderson(2006) to use distance matrix to test the variance among groups. Here for categorical groups, one-hot encoding approach is performed.

Parametric: this uses ANOVA procedure to and TukeyHSD test to validate the significance among clusters.

}
\references{
Anderson, M.J. (2006) Distance-based tests for homogeneity of multivariate dispersions. Biometrics 62, 245-253.

Anderson, M.J., Ellingsen, K.E. & McArdle, B.H. (2006) Multivariate dispersion as a measure of beta diversity. Ecology Letters 9, 683-693
}
\examples{
## this code will run

data(iris)
data = iris[,1:4]

cluster = kmeans(data,4)$cluster

testClusters(data, cluster, conf = 0.95)
}
