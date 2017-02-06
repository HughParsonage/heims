#' Make HEIMS element nos human-readable
#' @param DT The data table with original names
#' @return DT with the new names
#' @details See \code{\link{heims_data_dict}}.
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


