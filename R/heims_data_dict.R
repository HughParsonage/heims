#' HEIMS data dictionary
#' @format A named list each containing 5 elements:
#' \describe{
#' \item{\code{long_name}}{a human-readable version of the variable; \code{orig_name} the element number;}
#' \item{\code{mark_missing}}{a vectorized-function returning \code{TRUE} on values of the variable which should be coded as \code{NA};}
#' \item{\code{ad_hoc_prepare}}{a function to apply before validation;}
#' \item{\code{validate}}{a single-value function returning \code{TRUE} or \code{FALSE} on vectors which comply with the variable's coding rules.}
#' \item{\code{ad_hoc_validation_note}}{If the data dictionary did not cover elements in the file, how the \code{validate} function was altered to suffer them.}
#' \item{\code{valid}}{a vectorized function returning \code{TRUE} or \code{FALSE} on vectors which do not comply with the variable's coding rules.}
#' \item{\code{decoder}}{A function of the \code{data.table} decoding the variable decoded.}
#' \item{\code{post_fst}}{A function of the \code{data.table} returned by fst to be used (for example to reset attributes).}
#' }
#' @source \url{http://heimshelp.education.gov.au/sites/heimshelp/dictionary/pages/data-element-dictionary}
#' @details Abbreviations in \code{long_name}:
#' \describe{
#' \item{\code{amt}}{Amount}
#' \item{\code{cd}}{Code}
#' \item{\code{det}}{Detail(s)}
#' \item{\code{FOE}}{Field of education}
#' \item{\code{Maj}}{Major}
#' }
#' @import data.table

"heims_data_dict"
