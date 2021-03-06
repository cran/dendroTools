% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_transform.R
\name{data_transform}
\alias{data_transform}
\title{data_transform}
\usage{
data_transform(
  input,
  format = "daily",
  monthly_aggregate_function = "auto",
  date_format = "ymd"
)
}
\arguments{
\item{input}{typical daily data format: Data frame with two columns, first column represents date,
second column represents variable, such as mean temperature, precipitation, etc. Date should be in
format Year-Month-Day (e.g. "2019-05-15")}

\item{format}{character string indicating the desired output format. Should be "daily" or "monthly".
Daily format returns a data frame with 366 columns (days), while monthly format returns data frame
with 12 columns (months). Years are indicated as row names.}

\item{monthly_aggregate_function}{character string indicating, how to aggregate daily into monthly
data. It can be "mean" or "sum". Third option is "auto" (default). In this case function will try
to guess whether input is temperature or precipitation data. For temperature, it will use "mean",
for precipitation "sum".}

\item{date_format}{Describe the format of date. It should be one of "ymd", "ydm", "myd", "mdy",
"dmy", "dym".}
}
\value{
env_data suitable for daily or monthly analysis with dendroTools.
}
\description{
Transforms daily data with two columns (date and variable) into data frame suitable for daily or
monthly analysis with dendroTools.
}
\examples{
data(swit272_daily_temperatures)
proper_daily_data <- data_transform(swit272_daily_temperatures, format = "daily",
   date_format = "ymd")

proper_monthly_data <- data_transform(swit272_daily_temperatures, format = "monthly",
   date_format = "ymd")

data(swit272_daily_precipitation)
proper_daily_data <- data_transform(swit272_daily_precipitation, format = "daily",
   date_format = "ymd")

proper_monthly_data <- data_transform(swit272_daily_precipitation, format = "monthly",
   date_format = "ymd")
}
