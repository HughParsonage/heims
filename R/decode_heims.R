#' Decode HEIMS elements
#' @param DT A \code{data.table} with the original HEIMS column names.
#' @param show_progress Display the progress of the function (which is likely to be slow).
#' @return DT with the values decoded and the names renamed.
#' @details Each variable in \code{DT} is validated according \code{\link{heims_data_dict}} before being decoded. Any failure stops the validation.
#'
#' This function takes a long time to finish.
#' @export decode_heims

decode_heims <- function(DT, show_progress = FALSE){
  orig_key <- key(DT)
  `_order` <- NULL
  DT[, `_order` := seq_len(.N)]
  DTnoms <- names(DT)

  # do ad_hoc_prepares
  for (j in seq_along(DT)){
    k <- DTnoms[j]
    # Consult the dictionary. If there is an ad_hoc_prepare
    # element, apply it now, otherwise leave as is.
    if ("ad_hoc_prepare" %in% names(heims_data_dict[[k]]) && is.function(heims_data_dict[[k]]$ad_hoc_prepare)){
      set(DT, j = j, value = heims_data_dict[[k]]$ad_hoc_prepare(DT[[j]]))
    }
  }

  if (show_progress){
    progress <- 0
    n_names <- length(DTnoms)
  }

  for (orig in DTnoms){
    if (show_progress){
      progress <- progress + 1
      cat(orig, "\t\t", as.character(Sys.time()), "\t", formatC(progress, width = nchar(n_names)), "/", n_names, "\n", sep = "")
    }
    if (orig %in% names(heims_data_dict)){
      dict_entry <- heims_data_dict[[orig]]

      origcol_not_na <-
        DT[[orig]]

      origcol_not_na <- origcol_not_na[!is.na(origcol_not_na)]

      if (length(origcol_not_na) > 0 && !dict_entry$validate(origcol_not_na)){
        stop(orig, " was not validated.")
      }

      if ("decoder" %in% names(dict_entry)){
        if (is.data.table(dict_entry[["decoder"]])){
          DT_decoder <- dict_entry[["decoder"]]
          setkeyv(DT, key(DT_decoder))
          DT <- DT_decoder[DT, roll=TRUE]
        } else {
          if (is.function(dict_entry[["decoder"]])){
            decoder_fn <- dict_entry[["decoder"]]
            DT <- decoder_fn(DT)
          }
        }
        # Drop the original column
        if (orig %in% names(DT)){
          DT[, (orig) := NULL]
        }
      } else {
        if ("mark_missing" %in% names(dict_entry)){
          switch(class(DT[[orig]]),
                 "logical" = {
                   DT[, (orig) := if_else(dict_entry$mark_missing(DT[[orig]]), NA, DT[[orig]])]
                 },
                 "integer" = {
                   DT[, (orig) := if_else(dict_entry$mark_missing(DT[[orig]]), NA_integer_, DT[[orig]])]
                 },
                 "numeric" = {
                   DT[, (orig) := if_else(dict_entry$mark_missing(DT[[orig]]), NA_real_, DT[[orig]])]
                 },
                 "character" = {
                   DT[, (orig) := if_else(dict_entry$mark_missing(DT[[orig]]), NA_character_, DT[[orig]])]
                 })
        }

      }
    }
  }

  rename_heims(DT)

  setcolorder(DT, names(DT)[order(vapply(DT, uniqueN, integer(1)))])
  setcolorder(DT, c(intersect(names(DT),
                              c("CHESSN", "HE_Provider_name", "Student_id")),
                    setdiff(names(DT),
                            c("CHESSN", "HE_Provider_name", "Student_id"))))

  setkeyv(DT, orig_key)
  DT %>%
    setorderv("_order") %>%
    .[, `_order` := NULL] %>%
    .[]
}
