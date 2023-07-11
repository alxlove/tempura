\name{polydata}
\alias{polydata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{polydata}
%%  ~~function to do ... ~~
\description{
Adds time polynomials to the data set.
}
\usage{
polydata(x, id, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{data}{A data.frame object containing the variables specified in the formula, including the dependent variable, independent variables, panel identifier, and time variable.}
  \item{id}{A numeric or character variable indicating the panel ID or group to which each observation belongs.}
  \item{time}{A numeric variable indicating the time of observation for each data point within each panel or group.}
}
\details{
'polydata' is used mainly by the 'dvpaul' to create the model frame it uses in a linear model. Users interested in exploring more advanced models can add time polynomials to their data using the  polydata this function.
}
\value{
}
\references{
}
\author{

}
\note{

}

\seealso{
See; dvpaul
}
\examples{
##  Load data

library(foreign)

panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")

##Modify data

panel2<-polydata(panel, country, year)

{
  }
}

