#' BER roof models
#'
#' Log-normally distributed roof areas extracted from BER dataset (to 2023)
#'
#' @format A data frame with 24 rows and 5 variables:
#' \describe{
#'   \item{region}{geographic region}
#'   \item{housing_type}{Detached, semi-detached etc}
#'   \item{house_type}{number of storeys}
#'   \item{meanlog}{meanlog parameter. exp(meanlog)= median area.}
#'   \item{sdlog}{sdlog parameter}
#' }
#' @source \url{www.seai.ie} and analysis in BER.R
"ber_roof_models"
