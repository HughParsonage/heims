context("Decode heims")

test_that("Given a raw load data file, correctly decoded", {
  skip_if_not(file.exists("~/Students/heims-tests/load_2015_sample50.csv"))
  raw_load <-
    fread("~/Students/heims-tests/load_2015_sample50.csv")

  # renamed_dt <- copy(raw_dt) %>% rename_heims

  decode_heims(copy(raw_load))
})

test_that("Given a raw completions data file, correctly decoded", {
  skip_if_not(file.exists("~/Students/heims-tests/completion_2015_sample50.csv"))
  raw_enrol <-
    fread("~/Students/heims-tests/completion_2015_sample50.csv")

  decode_heims(copy(raw_enrol))

})

test_that("Given a raw enrol data file, correctly decoded", {
  skip_if_not(file.exists("~/Students/heims-tests/enrol_2015_sample50.csv"))
  raw_enrol <-
    fread("~/Students/heims-tests/enrol_2015_sample50.csv")

  decode_heims(copy(raw_enrol))

})

test_that("Given the enrol 2015 file, won't error", {
  file <- "~/Students/enrol2015/enrol2015.csv"
  skip_if_not(file.exists(file))
  raw_enrol2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**"))

  decode_heims(copy(raw_enrol2015))
})

test_that("Given the completions 2015 file, won't error", {
  file <- "~/Students/completions2015/completions2015.csv"
  skip_if_not(file.exists(file))
  raw_completions2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**"))

  decode_heims(copy(raw_completions2015))
})

test_that("Given the course 2015 file, won't error", {
  file <- "~/Students/course2015/course2015.csv"
  skip_if_not(file.exists(file))
  raw_course2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**"))

  decode_heims(copy(raw_course2015))
})

test_that("Given the load 2015 file, won't error", {
  file <- "~/Students/load2015/load2015.csv"
  skip_if_not(file.exists(file))
  raw_load2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**"), colClasses = list(character = c("E313")))

  decode_heims(copy(raw_load2015))
})


