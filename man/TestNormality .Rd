\name{TestNormality}
\alias{TestNormality}
\title{Test for Normality}
\usage{
TestNormality(data)
}
\arguments{
   \item{data}{a numerical data frame with data and column called cluster with cluster information}

}
\description{
This function performs Shapiro-Wilk test is used to test the normality of the sample population.

Null Hypothesis H0 for this test is: the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality
}
\examples{
## this code will run

data(iris)
data = iris[,1:4]

data$cluster = kmeans(data,4)$cluster

TestNormality(data)
}
