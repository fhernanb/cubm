#' Evaluation of the Orientation Services 2002.
#'
#' A sample survey on students evaluation of the Orientation services 
#' was conducted across the 13 Faculties of University of Naples 
#' Federico II in five waves: participants were asked to express 
#' their ratings on a 7 point scale (1 = "very unsatisfied", 
#' 7 = "extremely satisfied"). The dataset was taken from CUB package.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{faculty}{A factor variable, with levels ranging from 1 to 13 indicating the coding for the different university faculties}
#'   \item{freqserv}{A factor with levels: 0 = for not regular users, 1 = for regular users}
#'   \item{age}{Variable indicating the age of the respondent in years}
#'   \item{lage}{logarithm of age}
#'   \item{gender}{A factor with levels: 0 = man, 1 = woman}
#'   \item{diploma}{A factor with levels: 1 = classic studies, 2 = scientific studies, 3 = linguistic, 4 = Professional, 5 = Technical/Accountancy, 6 = others}
#'   \item{residence}{A factor with levels: 1 = city NA, 2 = district NA, 3 = others}
#'   \item{changeFa}{A factor with levels: 1 = changed faculty, 2 = not changed faculty}
#'   \item{informat}{Level of satisfaction about the collected information}
#'   \item{willingn}{Level of satisfaction about the willingness of the staff}
#'   \item{officeho}{Judgment about the Office hours}
#'   \item{compete}{Judgement about the competence of the staff}
#'   \item{global}{Global satisfaction}
#' }
#' @source \url{https://cran.r-project.org/web/packages/CUB/CUB.pdf}
"univer"