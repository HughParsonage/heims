#' HEIMS data dictionary
#' @format A named list each containing 4 elements:
#' the \code{long_name}, a human-readable version of the variable; \code{orig_name} the element number;
#' \code{mark_missing} a vectorized-function returning \code{TRUE} on values of the variable which should be coded as \code{NA};
#' \code{validate} a single-value function returning \code{TRUE} or \code{FALSE} on vectors which comply with the variable's coding rules.
#' \code{ad_hoc_validation_note} If the data dictionary did not cover elements in the file, how the \code{validate} function was altered to suffer them.
#' \code{valid} a vectorized function returning \code{TRUE} or \code{FALSE} on vectors which do not comply with the variable's coding rules.
#' @source \url{http://heimshelp.education.gov.au/sites/heimshelp/dictionary/pages/data-element-dictionary}
#' @details Abbreviations in \code{long_name}:
#' \describe{
#' \item{\code{amt}}{Amoun}
#' \item{\code{cd}}{Code}
#' \item{\code{det}}{Detail(s)}
#' \item{\code{FOE}}{Field of education}
#' \item{\code{Maj}}{Major}
#' }

"heims_data_dict"
