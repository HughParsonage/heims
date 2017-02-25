library(data.table)
library(dplyr)
library(dtplyr)
library(magrittr)

# In case library(fastmatch) in effect
coalesce <- function(...) dplyr::coalesce(...)

E490_decoder <-
  data.table::fread("./data-raw/decoders/E490-decoders.txt")

E306_decoder <-
  HE_Provider_decoder <-
  fread("./data-raw/decoders/Providers-by-code-Appendix-A.csv") %>%
  setnames("Provider Code", "E306") %>%
  setnames("Provider Name", "HE_Provider_name") %>%
  setnames("Provider Type", "HE_Provider_type") %>%
  setkey("E306")

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

E329_decoder <-
  fread("./data-raw/decoders/E329-ModeAttendance-decoder.tsv") %>%
  setkey(E329)

E330_decoder <-
  fread("./data-raw/decoders/E330-decoder.csv") %>%
  setkey(E330)

force_integer <- function(x){
  suppressWarnings(as.integer(x))
}

E346_decoder <-
  fread("./data-raw/decoders/ABS-country-codes.csv", na.strings = "") %>%
  mutate(Region_code = as.integer(force_integer(V1) * 1000),
         Country_broad_code = as.integer(force_integer(V2) * 100)) %>%
  .[] %>%
  mutate(
    Region_name = if_else(!is.na(V1), V2, NA_character_),
    Country_name = coalesce(V4, V3, V2),
    # Region_name = zoo::na.locf(Region_name, na.rm = FALSE),
    # Country_name = zoo::na.locf(Country_name, na.rm = FALSE),
    Country_code = force_integer(V3)
    # Country_code = zoo::na.locf(Country_code, na.rm = FALSE)
  ) %>%
  .[, Country_code := coalesce(Region_code, Country_broad_code, force_integer(V3))] %>%
  .[, .(Country_code, Country_name)] %>%
  .[complete.cases(.)] %>%
  .[, E346 := Country_code] %>%
  rbind(data.table(E346 = c(9998L, 9999L),
                   Country_code = c(9998L, 9999L),
                   Country_name = c(NA_character_, NA_character_))) %>%
  rbind(fread("./data-raw/decoders/ABS-country-code-2006-2nd-edn.csv"), use.names = TRUE) %>%
  rbind(fread("./data-raw/decoders/ABS-country-code-1998-suppl.csv"), use.names = TRUE) %>%
  .[, Country_code := NULL] %>%
  setkey(E346) %>%
  .[]

E348_decoder <-
  fread("./data-raw/decoders/E348-decoder.csv", na.strings = "") %>%
  setkey(E348)

E358_decoder <-
  fread("./data-raw/decoders/E358-decoder.txt") %>%
  setkey(E358)

E386_decoder <-
  # sort(unique(enrols_2015$E386)
  CJ(E386 = c(0L, 2L, 10000000L, 10000001L, 10000002L, 10000010L, 10000011L,
              10000012L, 10000100L, 10000101L, 10000102L, 10000110L, 10000111L,
              10000112L, 10001000L, 10001001L, 10001002L, 10001011L, 10001012L,
              10001101L, 10001102L, 10001111L, 10001112L, 10010000L, 10010001L,
              10010002L, 10010011L, 10010012L, 10010101L, 10010102L, 10010111L,
              10010112L, 10011001L, 10011002L, 10011011L, 10011012L, 10011101L,
              10011102L, 10011111L, 10011112L, 10100000L, 10100001L, 10100002L,
              10100010L, 10100011L, 10100012L, 10100101L, 10100102L, 10100111L,
              10100112L, 10101001L, 10101002L, 10101011L, 10101012L, 10101101L,
              10101102L, 10101111L, 10101112L, 10110001L, 10110002L, 10110011L,
              10110012L, 10110101L, 10110102L, 10110111L, 10110112L, 10111001L,
              10111002L, 10111011L, 10111012L, 10111101L, 10111102L, 10111111L,
              10111112L, 11000000L, 11000001L, 11000002L, 11000011L, 11000012L,
              11000101L, 11000102L, 11000111L, 11000112L, 11001001L, 11001002L,
              11001011L, 11001012L, 11001101L, 11001102L, 11001111L, 11001112L,
              11010001L, 11010002L, 11010011L, 11010012L, 11010101L, 11010102L,
              11010111L, 11010112L, 11011001L, 11011002L, 11011011L, 11011012L,
              11011101L, 11011102L, 11011111L, 11011112L, 11100001L, 11100002L,
              11100011L, 11100012L, 11100101L, 11100102L, 11100111L, 11100112L,
              11101001L, 11101002L, 11101011L, 11101101L, 11101102L, 11101111L,
              11110001L, 11110002L, 11110011L, 11110012L, 11110101L, 11110102L,
              11110111L, 11110112L, 11111001L, 11111002L, 11111011L, 11111012L,
              11111101L, 11111102L, 11111111L, 11111112L, 20000000L, 20000001L,
              20000002L),
     digits = 1:8,
     v = 0:2) %>%
  mutate(E386 = if_else(E386 == 2L, 20000000L, E386)) %>%
  merge(data.table(digits = 1:8,
                   disability = paste0(c("any", "hearing", "learning", "mobility", "visual", "medical", "other", "wants_services"), "_disability")),
        by = "digits") %>%
  merge(data.table(v = 0:2,
                   answer = c(NA, FALSE, TRUE)), by = "v") %>%
  filter(nth_digit_of(E386, n = 9 - digits) == v) %>%
  select(E386, disability, answer) %>%
  unique %>%
  dcast.data.table(E386 ~ disability) %>%
  setkey(E386)

E461_decoder <-
  fread("./data-raw/decoders/E461-FOE-decoder.tsv") %>%
  setkey(E461)

E562_decoder <-
  fread("./data-raw/decoders/E562-decoder.txt") %>%
  setkey(E562)

FOE_uniter <-
  fread("./data-raw/foegrattan-ittima.csv") %>%
  setnames("foecode", heims_data_dict$E461$long_name) %>%
  setkeyv(heims_data_dict$E461$long_name)

devtools::use_data(E490_decoder,
                   E306_decoder, HE_Provider_decoder,
                   E310_decoder,
                   E312_decoder,
                   E316_decoder,
                   E329_decoder,
                   E330_decoder,
                   E346_decoder,
                   E348_decoder,
                   E358_decoder,
                   E386_decoder,
                   E461_decoder,
                   E562_decoder,
                   FOE_uniter,
                   internal = FALSE, overwrite = TRUE)


