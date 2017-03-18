#' Join enrol to completion successively
#' @param enrols A data.table for enrolments (right table in the right outer joins)
#' @param enrolYearById Two-column data table: one column named the same as
#'   \code{enrol.id} and the other representing the enrolment year of that id.
#' @param enrol.id The unique identifier of each enrolment.
#' @param completionYearById Two-column data table: one column named the same as
#'   \code{completion.id} and the other representing the completion year of that id.
#' @param completion.id The unique identifier of each completion (possibly spread
#'   over multiple tables).
#' @param completion.year The column in \code{completionTables} referring to the
#'   year of completion.
#' @param completionTables A list. Each element contains two elements,
#'  the first, \code{DT}, is a data.table, the second is passed to the
#'  \code{on = } argument \code{data.table}. For example
#'  \code{c("HE_Provider_name", "Student_id", "completion_Year>=enrol_Year")}
#'   for a non-equi join.
#' @param debug Print intermediate tables.
#' @export
join_enrols_to_completions <- function(enrolTable,
                                       enrolYearById,
                                       enrol.id = "Enrol_Grattan_id",
                                       completionYearById,
                                       completion.id = "Completions_Grattan_id",
                                       completion.year = "completion_Year",
                                       completionTables,
                                       debug = FALSE){

  stopifnot(is.data.table(enrolTable),
            is.data.table(enrolYearById),
            enrol.id %in% names(enrolYearById),
            enrol.id %in% names(enrolTable),
            is.integer(dplyr::select_(enrolYearById, .dots = setdiff(names(enrolYearById), enrol.id)) %>% .[[1]]),
            is.list(completionTables))

  check_completionTable <- function(el){
    stopifnot(length(el) == 2L,
              is.data.table(el[[1]]),
              is.character(el[[2]]))
    invisible(NULL)
  }

  invisible(lapply(completionTables, check_completionTable))

  the_enrolYearById <- enrolYearById[enrolTable, .SD, .SDcols = names(enrolYearById), on = enrol.id]

  # The join is intended to keep only those
  # with Year greater in the completion file than in the enrol file
  join_two <- function(enrol, complete, on_key){
    complete[enrol, on = on_key] %>%
      .[the_enrolYearById, on = enrol.id] %>%
      # At this point "completion.year" is in fact the enrol year
      .[, (completion.year) := NULL] %>%
      completionYearById[., on = completion.id]
  }

  out <- copy(enrolTable)

  for (i in seq_along(completionTables)){
    completions_tbl <- copy(completionTables[[i]][[1]])
    # Do not attempt match if match already made
    if (i == 1L){
      out <- join_two(out, completions_tbl, on_key = c(completionTables[[i]][[2]]))
      out[, .no_match := is.na(.SD[[completion.year]])]
      if (debug){
        cat("First join:")
        print(out)
        cat('\n\n\n\n')
      }
    } else {
      completions_tbl[, .no_match := TRUE]
      out <- join_two(out, completions_tbl, on_key = c(".no_match", completionTables[[i]][[2]]))
      if (debug){
        cat("\n\n\n\n ", i, "\n")
        print(out)
      }
      out[, .no_match := and(is.na(out[[completion.year]]), out[[".no_match"]])]
      if (paste0("i.", completion.year) %in% names(out)){
        out[, (completion.year) := coalesce(out[[paste0("i.", completion.year)]], out[[completion.year]])]
        out[, (paste0("i.", completion.year)) := NULL]
      }
    }
  }

  out[, .SD, .SDcols = c(enrol.id, completion.year)]
}

