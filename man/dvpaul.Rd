\name{dvpaul}
\alias{dvpaul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dvpaul}
%%  ~~function to do ... ~~
\description{
The \code{dvpau} function estimates panel models by incorporating time polynomials to capture the the passage of time. It assigns a \var{t} value to each observation based on the time passed since the first observation of its respective the panel ID or group. \var{t2} and \var{t3} represent the squared and cubed of the \var{t}. The function then runs a regression with these terms, excluding panel ids or tim.
}
\usage{
dvpaul(formula, idvar,timevar, dataset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A simple regression formula (y ~ x1 + x2...); see formula}
  \item{idvar}{A character or numeric variable identifying the panel or group to which each observation belongs.}
  \item{timevar}{A numeric variable indicating the time of observation for each data point.}
  \item{dataset}{A data.frame object containing the variables specified in the formula, including the dependent variable, independent variables, as well as the panel-identifier and time variables.}
}
\details{
\code{dvpaul()}estimates dynamic panel models by incorporating id-specific cubic polynomials to model the passage of time, based on the earliest observation for each id. It produces a cubic polynomial (t_id + t_id^2 + t_id^3) that is added to a linear model along with the main predictors.

The main advantages of this method is its ability to captures and smooth temporal trends, even in the presence of gaps between observations, and to address autocorrelation within panel data. The approach has been applied both to dichotomous hazard models and for explaining continuous outcomes.

Users should exercise discretion regarding the relevance of polynomials based on the evolution of their dependent variable. The approach is more suitable for capturing prolonged periods of change rather than rapid changes. More sensitive measures, such as splines or higher-order polynomials, can better account for rapid intervals of change but may overfit in cases of gradual time trends.
}
\references{
* \href{https://doi.org/10.1093/pan/mpq013}{Carter, D. B., & Signorino, C. S. (2010).} Back to the future: Modeling time dependence in binary data. Political Analysis, 18(3), 271-292.

* \href{https://doi.org/10.1177/00031224211000710}{Torche, F., & Rauf, T. (2021).} The political context and infant health in the United States. American Sociological Review, 86(3), 377-405.
}

\seealso{
\code{\link{polydata}}
}
\examples{
##  Load data

panel_data <-haven::read_dta("https://www.lisdatacenter.org/wp-content/uploads/CWS-stata-2020.dta")

##Run regression

m<-dvpaul(rred ~ leftcum + ud, idn, year, panel_data)

## See results via

summary(m)
}

