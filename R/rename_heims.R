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
  long_name <- orig_name <- NULL
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
  long_name <- v. <- NULL

  v <- gsub("^e([0-9]+)$", "E\\1", v)

  decoder <-
    lapply(heims_data_dict, function(x){
      data.table(long_name = unlist(x[names(x) == "long_name"]),
                 v. = unlist(x[names(x) == "orig_name"]))
    }) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[v. %in% v]

  is_initial <- NULL
  input <-
    data.table(v. = v) %>%
    # A at the end means 'initial'
    .[, is_initial := grepl("^E[0-9]+A$", v., perl = TRUE)] %>%
    .[, v. := gsub("^(E[0-9]+)A$", "\\1", v., perl = TRUE)]

  decoder %>%
    .[input, on = "v."] %>%
    # Retain the name if join unsuccessful
    .[, long_name := if_else(is.na(long_name), v., long_name)] %>%
    .[, long_name := if_else(is_initial, paste0(long_name, "_init"), long_name)] %>%
    .[["long_name"]]
}


