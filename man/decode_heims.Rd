% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decode_heims.R
\name{decode_heims}
\alias{decode_heims}
\title{Decode HEIMS elements}
\usage{
decode_heims(DT, show_progress = FALSE, check_valid = TRUE, selector)
}
\arguments{
\item{DT}{A \code{data.table} with the original HEIMS column names.}

\item{show_progress}{Display the progress of the function (which is likely to be slow on real data).}

\item{check_valid}{Check the variable is valid before decoding. Setting to \code{FALSE} is faster, but should only be done when you know the data has been validated.}

\item{selector}{Original HEIMS names to restrict the decoding to. Other names will be preserved.}
}
\value{
\code{DT} with the values decoded and the names renamed.
}
\description{
Decode HEIMS elements
}
\details{
Each variable in \code{DT} is validated according \code{\link{heims_data_dict}} before being decoded. Any failure stops the validation.

If \code{DT} has a key, the output will have a key, but set on the \strong{decoded} columns and
the ordering will most likely change (to reflect the decoded values).

This function will, on the full HEIMS data, take a long time to finish. Typically in the order of
10 minutes for the enrol file.
}
\examples{
\dontrun{
# (E488 is made up so won't work if validation is attempted.)
decode_heims(dummy_enrol)
}
decode_heims(dummy_enrol, show_progress = TRUE, check_valid = FALSE)
}
