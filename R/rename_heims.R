#' Make HEIMS element nos human-readable
#' @name element_decoders
#' @param DT The data table with original names
#' @param v A vector of element names.
#' @return DT with the new names or the vector with the names translated.
#' @details See \code{\link{heims_data_dict}}.


#' @rdname element_decoders
#' @export
rename_heims <- function(DT){
  DTnoms <- names(DT)
  decoder <-
    lapply(heims_data_dict, function(x) data.table(long_name = x[names(x) == "long_name"],
                                                   orig_name = x[names(x) == "orig_name"])) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[orig_name %in% DTnoms]

  setnames(DT,
           old = decoder[["orig_name"]],
           new = decoder[["long_name"]])
  DT
}

#' @rdname element_decoders
#' @export element2name
element2name <- function(v){
  decoder <-
    lapply(heims_data_dict, function(x){
      data.table(long_name = unlist(x[names(x) == "long_name"]),
                 v. = unlist(x[names(x) == "orig_name"]))
    }) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[v. %in% v]

  decoder %>%
    .[data.table(v. = v), on = "v."] %>%
    # Retain the name if join unsuccessful
    .[, long_name := if_else(is.na(long_name), v., long_name)] %>%
    .[["long_name"]]
}


