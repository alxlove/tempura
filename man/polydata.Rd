\name{polydata}
\alias{polydata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{polydata}
%%  ~~function to do ... ~~
\description{
Adds time polynomials to the data set.
}
\usage{
polydata(dataset,idvar,timevar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{dataset}{A data.frame object containing the variables specified in the formula, including the dependent variable, independent variables, as well as the panel-identifier and time variables.}
  \item{idvar}{A character or numeric variable identifying the panel or group to which each observation belongs.}
  \item{timevar}{A numeric variable indicating the time of observation for each data point.}
}
\details{
'polydata' is used mainly by the 'dvpaul' to create the model frame it uses in a linear model. Users interested in exploring more advanced models can add time polynomials to their data using the  polydata this function.
}

\seealso{
\code{\link{dvpaul}}
}
\examples{
##  Load data

panel_data <-haven::read_dta("https://www.lisdatacenter.org/wp-content/uploads/CWS-stata-2020.dta")

##Modify data

panel_data2<-polydata(panel_data, idn, year)

}

