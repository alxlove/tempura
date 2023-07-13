# Tempura: Tools for Dynamic Panel Models

***
Tempura is an R package that provides a set of functions for estimating dynamic panel models, with a focus on dealing with missing data and autocorrelation. The package includes three main functions:


* `dvgreg()`: Estimates dynamic panel models where observations are not available for all time periods, resulting in missing values in the dependent variable.
* `dvpaul()`: Estimates panel models by incorporating time polynomials to capture the passage of time.
* `polydata()`: Adds time polynomials to the dataset.

The tempura package is designed to be easy to use and to provide researchers with a ready-to-use methods for modeling panel data. The documentation for the package includes examples of how to use the functions to estimate a variety of models.

## Installation
***
You can install the Tempura package from GitHub using the following command:


``` r
remotes::install_github("machtei/tempura")
```
#### A note on Stata
***

Stata users can also use these tools by downloading the .ado files and placing them in their main ado folder _(C:\/ado)_. The Stata folder contains the original **dvgreg.ado** program developed by Vernby & Lindgren, and a **dvpaul.ado** created as part of the tempura project.

## Usage
***
The tempura package can be used to estimate dynamic panel models as follows:
``` r
library(tempura)

# Load data
data(panel)

# Estimate dynamic panel model
model <- dvgreg(y ~ x1 + x2 + x3, panel, country, year)

# Summarize results
summary(model)

# Estimate dynamic panel model with time polynomials
model2 <- dvpaul(y ~ x1 + x2 + x3, panel, country, year)

# Summarize results
summary(model2)

```
#### Stata Users
***
The stata version follows a similar notation:

    . dvgreg y_var x1_var x2_var x3_var, id (id_var) time (time_var)
    . dvpaul y_var x1_var x2_var x3_var, id (id_var) time (time_var)


## References
***
* Vernby, K., and Lindgren, K. O. (2009). Estimating dynamic panel models when there are gaps in the dependent variable. Uppsala Universitet, Department of Government Studies in Political Economy and Welfare Working Paper Series, 1.
Stata.
* Carter, David B., and Christopher S. Signorino. 2010. "Back to the Future: Modeling Time Dependence in Binary Data." Political Analysis 18 (3): 271-92.


## Acknowledgements
***
The underlying approach of `dvgreg` for estimating dynamic panel models with DV gaps was developed by Vernby, K., and Lindgren, who also implemented this method in a Stata package of the same name. The `tempura` project was created to make this method available for replication and original work in R. I would like to express our gratitude to Vernby and Lindgren for their valuable contribution. The namesake Stata program is Vernby and Lindgren's original file and is included here with the authors' approval.

## Contributing
***
Future contributions are welcome. If you would like to improve or add to this package, please use GitHub's [issue tracker](https://github.com/machtei/tempura/issues), submit a [pull request](https://github.com/machtei/tempura/pulls) or email [@machtei](mailto:machtei@unc.edu).



