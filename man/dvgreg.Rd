\name{dvgreg}
\alias{dvgreg}
\title{dvgreg
}
\description{
The dvgreg function is designed to estimate dynamic panel models where observations are not available for all time periods, resulting in missing values in the dependent variable.
}
\usage{
dvgreg(formula, x, id, time)
}
\arguments{
  \item{formula}{A simple regression formula (y ~ x1 + x2...); see formula}
  \item{x}{A data.frame object containing the variables specified in the formula, including the dependent variable, independent variables, panel identifier, and time variable.}
  \item{x}{The data.frame object with columns that correspond to the names of the other arguments}
  \item{id}{A numeric variable indicating the time of observation for each data point.}
  \item{time}{A character or numeric variable identifying the panel or group to which each observation belongs.}
}
\details{
The dvgreg function estimates dynamic panel models by iterating through different values of ρ and reweighting the dependent and independent variables. It selects the most efficient value of ρ based on the model's R-squared, which is reported along with other estimates. The reweighted values can be accessed through the model matrix.
}

\value{
}
\references{
Vernby, K., & Lindgren, K. O. (2009). Estimating dynamic panel models when there are gaps in the dependent variable. Uppsala Universitet, Department of Government Studies in Political Economy and Welfare Working Paper Series, 1.
}
\author{
}
\note{
The dvgreg function is a preliminary implementation of the method and namesake Stata package  developed by Kare Vernby and Karl-Oskar Lindgren. It should be noted that the function is currently in the early stages of development and may undergo further refinements.
}
\seealso{
}
\examples{

##  Load data

library(foreign)

panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")

##adding 'missing values'

panel$y[sample(nrow(panel),nrow(panel)*.2)]<-NA

##Run regression

m1<-dvgreg(y~x1+x2+x3, panel, country, year)

## See results via

summary(m1)
{
  }
}

