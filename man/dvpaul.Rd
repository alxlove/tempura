\name{dvpaul}
\alias{dvpaul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dvpaul}
%%  ~~function to do ... ~~
\description{
The 'dvpaul' function estimates panel models by incorporating time polynomials to capture the the passage of time. It assigns a 't' value to each observation based on the time passed since the first observation of its respective the panel ID or group. 't2' and 't3' represent the squared and cubed of the 't'. The function then runs a regression with these terms, excluding panel ids or tim.
}
\usage{
dvpaul(formula, x, id, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A simple regression formula (e.g., y ~ x1 + x2) specifying the model to be estimated.}
  \item{data}{A data.frame object containing the variables specified in the formula, including the dependent variable, independent variables, panel identifier, and time variable.}
  \item{id}{A numeric or character variable indicating the panel ID or group to which each observation belongs.}
  \item{time}{A numeric variable indicating the time of observation for each data point within each panel or group.}
}
\details{
The 'dvpaul' function estimates dynamic panel models by incorporating id-specific cubic polynomials to model the passage of time, based on the earliest observation for each id. It produces a cubic polynomial (t_id + t_id^2 + t_id^3) that is added to a linear model along with the main predictors.

The main advantages of this method is its ability to captures and smooth temporal trends, even in the presence of gaps between observations, and to address autocorrelation within panel data. The approach has been applied both to dichotomous hazard models and for explaining continuous outcomes.

Users should exercise discretion regarding the relevance of polynomials based on the evolution of their dependent variable. The approach is more suitable for capturing prolonged periods of change rather than rapid changes. More sensitive measures, such as splines or higher-order polynomials, can better account for rapid intervals of change but may overfit in cases of gradual time trends.
}
\value{
}
\references{
Carter, D. B., & Signorino, C. S. (2010). Back to the future: Modeling time dependence in binary data. Political Analysis, 18(3), 271-292.

Torche, F., & Rauf, T. (2021). The political context and infant health in the United States. American Sociological Review, 86(3), 377-405.
}
\author{

}
\note{

}

\seealso{
See; polydata
}
\examples{
##  Load data

library(foreign)

panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")

##Run regression

m1<-dvpaul(y~x1+x2+x3, panel, country, year)

## See results via

summary(m1)
{
  }
}

