\name{sampleSize}
\alias{sampleSize}
\title{Sample Size}
\usage{
sampleSize(data, cluster)
}
\arguments{
   \item{data}{a numerical data frame}
   \item{cluster}{a numerical list or vector containing cluster information as numbers}
}
\description{
Creates a sample size of 30 for each group. If in case the population group sample is less than 30 then, the sample size will be the minimum of group sample size.
}
\examples{
## this code will run

data(iris)
data = iris[,1:4]

cluster = kmeans(data,4)$cluster

sampleSize(data, cluster)
}
