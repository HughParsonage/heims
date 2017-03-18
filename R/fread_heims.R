#' Read raw heims file
#' @param filename A text-delimeted file, passed to \code{fread} from \code{data.table}.
#' @return A \code{data.table}.
#' @export

fread_heims <- function(filename){
  file <- gsub("^.*((enrol)|(completions)|(load)).*$", "\\1", filename)
  fread(filename,
        na.strings = c("", "NA", "?", ".", "*", "**",
                             # CHESSN
                             "ZZZZZZZZZZ"),
        # course file does not contain "E313"
        colClasses = switch(file,
                            "enrol" = list(character = c("E313", "E347")),
                            "completions" = list(character = c("E313", "E347")),
                            "load" = list(character = c("E313")),
                            NULL))
}
