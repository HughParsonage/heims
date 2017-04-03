#' Join enrol to completion successively
#' @param enrolTable A data.table for enrolments (right table in the right outer joins)
#' @param enrolYearById Two-column data table: one column named the same as
#'   \code{enrol.id}, which must be an integer column, and the other representing the enrolment year of that id.
#' @param enrol.id The unique identifier of each enrolment.
#' @param completionYearById Two-column data table: one column named the same as
#'   \code{completion.id}, which must be an integer column, and the other representing the completion year of that id.
#' @param completion.id The unique identifier of each completion (possibly spread
#'   over multiple tables).
#' @param completion.year The column in \code{completionTables} referring to the
#'   year of completion.
#' @param completionTables A list. Each element contains two elements,
#'  the first, \code{DT}, is a data.table, the second is passed to the
#'  \code{on = } argument \code{data.table}. For example
#'  \code{c("HE_Provider_name", "Student_id", "completion_Year>=enrol_Year")}
#'   for a non-equi join.
#' @param injective Does the join need to be injective? If \code{TRUE}, the result will be unique on \code{completion.id}, using the earliest match.
#' @param imatch Column name indicating the list element on which the successful join occurred.
#' @param keep_imatch Should the column \code{imatch} be present in the output?
#' @param debug Print intermediate tables.
#' @export
join_enrols_to_completions <- function(enrolTable,
                                       enrolYearById,
                                       enrol.id = "Enrol_Grattan_id",
                                       completionYearById,
                                       completion.id = "Completions_Grattan_id",
                                       completion.year = "completion_Year",
                                       completionTables,
                                       injective = FALSE,
                                       imatch = "imatch",
                                       keep_imatch = FALSE,
                                       debug = FALSE){

  stopifnot(is.data.table(enrolTable),
            is.data.table(enrolYearById),
            enrol.id %in% names(enrolYearById),
            enrol.id %in% names(enrolTable),
            is.integer(enrolYearById[[enrol.id]]),
            is.integer(completionYearById[[completion.id]]),
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

  # After a join, common columns from the left have prefix `i.`
  # The function retains the non-missing values in those columns
  # over those without prefix, then drops the columns with prefix `i.` so
  # that subsequent joins can use the `i.` prefix property again.
  coalesce_i <- function(DT, nom){
    inom <- paste0("i.", nom)
    if (inom %in% names(DT)){
      # Set completion id to the i.Completion_id (from previous join) or Completion_id if i. is missing
      DT[, (nom) := coalesce(out[[inom]], out[[nom]])]
      DT[, (inom) := NULL]
    }
    DT
  }

  out <- copy(enrolTable)

  `_order` <- NULL
  out[, `_order` := 1:.N]


  .no_match <- NULL

  for (i in seq_along(completionTables)){
    completions_tbl <- copy(completionTables[[i]][[1]])
    completions_tbl[, (imatch) := i]
    # Do not attempt match if match already made
    if (i == 1L){
      out <- join_two(out, completions_tbl, on_key = c(completionTables[[i]][[2]]))
      out[, .no_match := is.na(.SD[[completion.year]])]
      if (debug){
        cat("\nFirst join:\n\n")
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
      out %>%
        .[, .no_match := and(is.na(out[[completion.year]]), out[[".no_match"]])] %>%
        coalesce_i(completion.id) %>%
        coalesce_i(completion.year) %>%
        coalesce_i(imatch)
    }
  }

  if (injective){
    out <-
      out %>%
      setorderv(imatch) %>%
      # Remove rows where completion id is duplicate (except for NAs).
      .[, ok := (is.na(.[[completion.id]]) | !duplicated(., by = completion.id))] %>%
      .[, (completion.id) := if_else(ok, .[[completion.id]], NA_integer_)] %>%
      .[, (completion.year) := if_else(ok, .[[completion.year]], NA_integer_)] %>%
      .[, ok := NULL] %>%
      .[]
  }

  setorderv(out, "_order")
  out[, .SD, .SDcols = c(enrol.id, completion.id, completion.year, if (keep_imatch) imatch else NULL)]
}

