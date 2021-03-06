% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rwebq.R
\name{webq_report}
\alias{webq_report}
\title{Generate an RMarkdown Report of WebQ Survey Responses}
\usage{
webq_report(
  survey_id,
  participant_ids = NULL,
  question_ids = NULL,
  output_dir = getwd(),
  ...
)
}
\arguments{
\item{survey_id}{The survey for which to produce a report.}

\item{participant_ids}{A vector of participant IDs to include. If NULL, will include all participants.}

\item{question_ids}{A vector of question IDs to include. If NULL, will include all questions.}

\item{output_dir}{The output file directory (default \code{getwd()})}

\item{...}{Additional arguments to be passed to \code{rmarkdown::render}, including output_file for file name, output_format, etc.}
}
\value{
The output file path, invisibly.
}
\description{
Generate an RMarkdown Report of WebQ Survey Responses
}
\examples{
\dontrun{webq_report('236008', participant_ids = c('20700916', '12996001'), output_dir = '~/myreports/', output_file = 'report.html')}
}
