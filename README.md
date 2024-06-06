
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SurveyPrevRshinyWHO

<!-- badges: start -->
<!-- badges: end -->

This RShiny app facilitates the mapping of health indicators prevalence
using data from Demographic and Health Surveys (DHS). Powered by the R
package SurveyPrev, our Shiny app revolutionizes the acquisition of
prevalence estimates through Small Area Estimation (SAE) techniques. It
is tailored for users of all backgrounds, enabling the performance of
complex statistical analyses without prior knowledge spatial data
management or modelling.

The application streamlines the analytical process into clear,
manageable steps. It guides users through model fitting and enhances
their experience with interactive visualization tools. Users can
dynamically explore the spatial distribution of health and demographic
indicators and directly interpret statistical outputs within the app’s
interface. Additionally, the application supports result exports,
facilitating detailed examinations and sharing in both spreadsheet and
graphical formats.

## Installation

You can install the development version of SurveyPrevRshinyWHO from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wu-thomas/WHO_saeforhealth")
```

## Launch the app

You can launch the Shiny app with the following command:

``` r
library(SurveyPrevRshinyWHO)
SurveyPrevRshinyWHO::run_app()
```
