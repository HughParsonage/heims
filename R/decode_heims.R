#' Decode HEIMS elements
#' @param DT A \code{data.table} with the original HEIMS column names.
#' @param show_progress Display the progress of the function (which is likely to be slow).
#' @param check_valid Check the variable is valid before decoding. Setting to \code{FALSE} is faster, but should only be done when you know the data has been validated.
#' @param selector Original HEIMS names to restrict the decoding to. Other names will be preserved.
#' @return DT with the values decoded and the names renamed.
#' @details Each variable in \code{DT} is validated according \code{\link{heims_data_dict}} before being decoded. Any failure stops the validation.
#'
#' This function takes a long time to finish.
#' @export decode_heims

decode_heims <- function(DT, show_progress = FALSE, check_valid = TRUE, selector){
  had.key <- haskey(DT)
  if (had.key){
    orig_key <- key(DT)
  } else {
    `_order` <- NULL
    orig_key <- "_order"
    DT[, `_order` := seq_len(.N)]
    DTnoms <- names(DT)
    setkeyv(DT, "_order")
  }

  setnames(DT, old = names(DT), new = gsub("^e", "E", names(DT)))

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
    if (orig %in% if (!missing(selector)) intersect(names(heims_data_dict), selector) else names(heims_data_dict)){
      dict_entry <- heims_data_dict[[orig]]

      origcol_not_na <-
        DT[[orig]]

      origcol_not_na <- origcol_not_na[!is.na(origcol_not_na)]

      if (check_valid && length(origcol_not_na) > 0 && !dict_entry$validate(origcol_not_na)){
        stop(orig, " was not validated.")
      }

      if ("decoder" %in% names(dict_entry)){
        if (is.data.table(dict_entry[["decoder"]])){
          DT_decoder <- dict_entry[["decoder"]]
          stopifnot(haskey(DT_decoder))
          # Limit the reordering to as few as columns as possible
          D.T <- DT[, .SD, .SDcols = c(key(DT), key(DT_decoder))]
          setkeyv(D.T, key(DT_decoder))
          D.T <- DT_decoder[D.T, roll=TRUE]
          setkey(D.T, key(DT))
          DT <- D.T[DT]
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

  if (!had.key){
    DT[, "_order" := NULL]
  }
  setkeyv(DT, orig_key)
  DT
}
