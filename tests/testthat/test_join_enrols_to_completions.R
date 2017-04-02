test_that("Joined known example", {
  # a la enrols
  enrols <-
    data.table(HE_Provider_name = c("a", "a", "a", "b", "b", "c", "c", "e"),
               Student_id = 1:8,
               CHESSN = 11:18,
               Enrol_Grattan_id = 1:8,
               enrol_Year = as.integer(c(2005, 2006, 2007, 2010, 2008, 2009, 2010, 2011)))

  enrol_Year_by_id <-
    enrols %>%
    select(Enrol_Grattan_id, enrol_Year)

  completions <-
    data.table(HE_Provider_name = c("a", "a", "b", "b", "c", "c"),
               Student_id = c(1, 2, 1, 2, 1, 2),
               CHESSN = c(11:16),
               Completions_Grattan_id = 11:16,
               completion_Year = as.integer(c(2006, 2006, 2006, 2010, 2005, 2004)))

  completion_Year_by_id <-
    completions %>%
    select(Completions_Grattan_id, completion_Year)

  # a la completions
  completions_by_Student_id <-
    completions %>%
    select(HE_Provider_name, Student_id, Completions_Grattan_id, completion_Year)

  completions_by_CHESSN <-
    completions %>%
    select(CHESSN, Completions_Grattan_id, completion_Year)

  join_result <- join_enrols_to_completions(enrolTable = enrols,
                                            enrolYearById = enrol_Year_by_id,
                                            enrol.id = "Enrol_Grattan_id",
                                            completionYearById = completion_Year_by_id,
                                            completion.id = "Completions_Grattan_id",
                                            completionTables = list(list(DT = completions_by_Student_id,
                                                                         on_key = c("HE_Provider_name", "Student_id", "completion_Year>=enrol_Year")),
                                                                    list(DT = completions_by_CHESSN,
                                                                         on_key = c("CHESSN", "completion_Year>=enrol_Year"))))

  expect_identical(join_result,
                   data.table(Enrol_Grattan_id = 1:8,
                              Completions_Grattan_id = c(11L, 12L, NA, 14L, NA, NA, NA, NA),
                              completion_Year = as.integer(c(2006, 2006, NA, 2010, NA, NA, NA, NA))))
})

test_that("Join injective", {
  enrols <-
    data.table(Student_id = 1:3,
               CHESSN = c(1L, 1L, 2L),
               Enrol_Grattan_id = 1:3,
               enrol_Year = 2005L)

  enrol_Year_by_id <-
    enrols %>%
    select(Enrol_Grattan_id, enrol_Year)

  completions <-
    data.table(Student_id = c(1L, 4L),
               CHESSN = 1L,
               Completions_Grattan_id = 1:2,
               completion_Year = c(2006L, 2007L))

  completion_Year_by_id <-
    completions %>%
    select(Completions_Grattan_id, completion_Year)

  # a la completions
  completions_by_Student_id <-
    completions %>%
    select(Student_id, Completions_Grattan_id, completion_Year)

  completions_by_CHESSN <-
    completions %>%
    select(CHESSN, Completions_Grattan_id, completion_Year)

  join_result <-
    join_enrols_to_completions(enrolTable = enrols,
                               enrolYearById = enrol_Year_by_id,
                               enrol.id = "Enrol_Grattan_id",
                               completionYearById = completion_Year_by_id,
                               completion.id = "Completions_Grattan_id",
                               completionTables = list(list(DT = completions_by_Student_id,
                                                            on_key = c("Student_id", "completion_Year>=enrol_Year")),
                                                       list(DT = completions_by_CHESSN,
                                                            on_key = c("CHESSN", "completion_Year>=enrol_Year"))),
                               injective = TRUE)

  expect_identical(join_result,
                   data.table(Enrol_Grattan_id = 1:3,
                              Completions_Grattan_id = as.integer(c(1, 2, NA)),
                              completion_Year = c(2006L, 2007L, NA_integer_)))


})

