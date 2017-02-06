

E490_decoder <-
  data.table::fread("./data-raw/decoders/E490-decoders.txt")

devtools::use_data(E490_decoder, internal = FALSE)


