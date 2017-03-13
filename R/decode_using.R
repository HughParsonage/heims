#' Decode using data table
#' @param DT A \code{data.table} to decode.
#' @param using A keyed \code{data.table} with two columns. The key needs to be present in \code{DT}. The other column is column to remain.
#' @export

decode_using <- function(DT, using){
  if (key(using) %in% names(DT)){
    out <-
      using[DT] %>%
      .[, key(using) := NULL]
  } else {
    out <- DT
  }
  out
}
