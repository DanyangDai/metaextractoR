#' Sample abstracts
#'
#' This dataset includes sample abstracts downloaded from one of my Covidence project.
#' Pre-processing was done by running the clean_name function from janitor package.
#'
#' @format A data frame with 20 rows and 16 variables:
#' \describe{
#'   \item{title}{Title of the manuscript}
#'   \item{authors}{Authors}
#'   \item{abstract}{The column that contains all abstracts}
#'   \item{published_year}{Year of the publication}
#'   \item{published_month}{Month of the publication}
#'   \item{journal}{Journal's name}
#'   \item{volume}{Volume number}
#'   \item{issue}{Issue number}
#'   \item{pages}{Page number}
#'   \item{accession_number}{Accession number}
#'   \item{doi}{doi number}
#'   \item{ref}{Reference}
#'   \item{covidence_number}{Covidence number}
#'   \item{study}{Study}
#'   \item{notes}{Notes}
#'   \item{tags}{Tags}
#' }
#' @source Created for demonstration purposes
"abstracts"
