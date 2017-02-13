library(data.table)
library(dplyr)
library(dtplyr)
library(magrittr)

E490_decoder <-
  data.table::fread("./data-raw/decoders/E490-decoders.txt")

E306_decoder <-
  HE_Provider_decoder <-
  fread("./data-raw/decoders/Providers-by-code-Appendix-A.csv") %>%
  setnames("Provider Code", "HE_Provider_cd") %>%
  setnames("Provider Name", "HE_Provider_name") %>%
  setnames("Provider Type", "HE_Provider_type") %>%
  setkey("HE_Provider_cd")

E310_decoder <-
  fread("./data-raw/decoders/E310-decoder.csv") %>%
  mutate(Course_type = factor(Course_type, levels = unique(.$Course_type), ordered = TRUE)) %>%
  mutate(Course_type_short = factor(Course_type_short, levels = unique(.$Course_type_short), ordered = TRUE)) %>%
  as.data.table %>%
  setkey(E310)

E312_decoder <-
  fread("./data-raw/decoders/E312-decoder.csv", na.strings = "") %>%
  .[, .(E312, Special_course)] %>%
  setkey(E312)

E316_decoder <-
  fread("./data-raw/decoders/E316-decoder.csv") %>%
  setkey(E316)

E348_decoder <-
  fread("./data-raw/decoders/E348-decoder.csv", na.strings = "") %>%
  setkey(E348)

devtools::use_data(E490_decoder,
                   E306_decoder, HE_Provider_decoder,
                   E310_decoder,
                   E312_decoder,
                   E316_decoder,
                   E348_decoder,
                   internal = FALSE, overwrite = TRUE)


