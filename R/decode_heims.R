#' Decode HEIMS elements
#' @param DT A \code{data.table} with the original column names.
#' @return DT with the values decoded and the names renamed.
#' @export decode_heims

decode_heims <- function(DT){
  DT[, `_order` := seq_len(.N)]
  DTnoms <- names(DT)
  for (orig in DTnoms){
    dict_entry <- heims_data_dict[[orig]]
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
    }
  }
  DT %>%
    setorderv("_order") %>%
    .[, `_order` := NULL] %>%
    .[]
}
