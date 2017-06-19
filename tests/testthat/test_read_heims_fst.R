context("read_heims_fst")

test_that("Correctly reads decoded enrol file", {
  skip_if_not(file.exists("~/Students/2011/enrol2011.csv"))
  enrol2011 <- fread_heims("~/Students/2011/enrol2011.csv")
  enrol2011_decoded <- decode_heims(enrol2011)
  tempf <- tempfile(fileext = ".fst")
  fst::write.fst(enrol2011_decoded, tempf)
  enrol2011_fst <- read_heims_fst(tempf)
  expect_true("integer64" %in% class(enrol2011_fst$CHESSN))
  expect_true("Date" %in% class(enrol2011_fst$CHESSN))
})

test_that("Correctly reads decoded load file", {
  skip_if_not(file.exists("~/Students/2011/load2011.csv"))
  load2011 <- fread_heims("~/Students/2011/load2011.csv")
  load2011_decoded <- decode_heims(load2011)
  tempfile(fileext = ".fst")
  tempf <- tempfile(fileext = ".fst")
  fst::write.fst(load2011_decoded, tempf)
  load2011_fst <- read_heims_fst(tempf)
  expect_true("Semester" %in% names(load2011_fst))
  expect_true("Census_date" %in% names(load2011_fst))

  semester1 <-
    load2011_fst %>%
    .[, .(Semester, Census_date)] %>%
    unique %>%
    .[Census_date == "2011-03-24"] %>%
    .[["Semester"]]

  semester2 <-
    load2011_fst %>%
    .[, .(Semester, Census_date)] %>%
    unique %>%
    .[Census_date == "2011-08-25"]

  expect_equal(semester1, 1)
  expect_equal(semester2, 2)
})
