% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_chartdata.R
\name{mlr_chart_data}
\alias{mlr_chart_data}
\title{Converts key:value pairs into ChartData specification written by developers at BigDatr}
\usage{
mlr_chart_data(row_key1, row_key2, row_value1, row_value2, label_value)
}
\arguments{
\item{row_key}{the key for the key:value pair}

\item{row_value}{the value for the key:value pair}
}
\value{
Outputs a JSON structure with Key:Value pairs in rows and columns. This JSON
structure can then be written to a file, s3, database etc. This function will not perform
this final step.
}
\description{
Converts key:value pairs into ChartData specification written by developers at BigDatr
}
\examples{
no_of_users.json <- mlr_chart_data("date", "count", no_of_users$date, no_of_users$no_of_users, "NoOfUsers")
}
