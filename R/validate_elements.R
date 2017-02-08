#' Validate HEIMS elements
#' @description Return TRUE or FALSE on whether or not each variable in a data.table complies with the HEIMS code limits
#' @param DT The data.table whose variables are to be validated.
#' @param .progress_cat Should the progress of the function be displayed on the console? If \code{TRUE} the name of the element about to be validated is shown.
#' @return A named logical vector, whether or not the variable complies with the style requirements. A value of \code{NA} indicates the variable
#' was not checked (perhaps because it is absent from \code{heims_data_dict}).
#' @details For early detection of invalid results, the type of the variable (in particular integer vs double) is considered first,
#' vetoing a \code{TRUE} result if different.
#' @examples
#' X <- data.frame(E306 = c(0, 1011, 999, 9998))
#' validate_elements(X)  # FALSE
#' X <- data.frame(E306 = as.integer(c(0, 1011, 999, 9998)))
#' validate_elements(X)  # TRUE
#'
#' @export validate_elements prop_elements_valid
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else


validate_elements <- function(DT, .progress_cat = FALSE){
  out <- rep_len(NA, ncol(DT))
  # These suffixes define the insert method/event, not the variable
  noms <- gsub("A$", "", gsub("_[12]", "", names(DT)))

  # e550 == E550
  noms <- gsub("^e([0-9]+)$", "E\\1", noms)

  for (n in seq_along(DT)){
    nom <- noms[n]
    if (.progress_cat){
      cat(nom, ".", sep = "")
    }
    if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$validate)){
      DTn <- DT[[n]]
      out[n] <- heims_data_dict[[nom]]$validate(DTn[!is.na(DTn)])
    }
  }
  if (.progress_cat){
    cat("\n")
  }
  names(out) <- names(DT)
  out
}

prop_elements_valid <- function(DT){
  out <- rep_len(NA_real_, ncol(DT))

  noms <- gsub("A$", "", gsub("_[12]", "", names(DT)))

  # e550 == E550
  noms <- gsub("^e([0-9]+)$", "E\\1", noms)

  for (n in seq_along(DT)){
    nom <- noms[n]
    if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$validate)){
      DTn <- DT[[n]]
      if (heims_data_dict[[nom]]$validate(DTn[!is.na(DTn)])){
        out[n] <- 1
      } else {
        if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$valid)){
          out[n] <- mean(heims_data_dict[[nom]]$valid(DTn))
        }
      }
    }
  }
  out
}
