\name{dvgreg}
\alias{dvgreg}
\title{dvgreg
}
\description{
The \code{dvgreg} function is designed to estimate dynamic panel models where observations are not available for all time periods, resulting in missing values in the dependent variable.
}
\usage{
dvgreg(formula, idvar,timevar, dataset)
}
\arguments{
  \item{formula}{A simple regression formula (y ~ x1 + x2...); see formula}
  \item{idvar}{A character or numeric variable identifying the panel or group to which each observation belongs.}
  \item{timevar}{A numeric variable indicating the time of observation for each data point.}
  \item{dataset}{A data.frame object containing the variables specified in the formula, including the dependent variable, independent variables, as well as the panel-identifier and time variables.}
}
\details{
\code{dvgreg()}estimates dynamic panel models by iterating through different values of ρ and reweighting the dependent and independent variables. It selects the most efficient value of ρ based on the model's \var{R-squared}, which is reported along with other estimates. The reweighted values can be accessed through the model matrix.

The dynamic method behind the dvgreg function uses the rho rather than 1 as the model’s intercept. To differentiate the two, \code{dvgreg}’s output includes a \var{constant} coefficient rather than an \var{intercept}.

Since \code{lm()}defaults to treating the constant term as a predictor, which could bias goodness-of-fit estimates, \code{dvgreg()}produces objects belonging to an lm subclass called \kbd{dvgreglm}. Objects in this class have a unique method for executing the \code{summary()}function.
}
\references{
\href{https://karevernby.files.wordpress.com/2011/06/dynamicpanels.pdf}{Vernby, K., & Lindgren, K. O. (2009).} Estimating dynamic panel models when there are gaps in the dependent variable. Uppsala Universitet, Department of Government Studies in Political Economy and Welfare Working Paper Series, 1.
}
\note{
The \code{dvgreg()}function implements the method and namesake Stata package developed by Kare Vernby and Karl-Oskar Lindgren.
}
\examples{
##  Load data

panel_data <-haven::read_dta("https://www.lisdatacenter.org/wp-content/uploads/CWS-stata-2020.dta")

##Run regression

m<-dvgreg(rred ~ leftcum + ud, idn, year, panel_data)

## See results via

summary(m)
}
