% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataPrepFunctions.R
\name{CheckForMissingDates}
\alias{CheckForMissingDates}
\title{CheckForMissingDates}
\usage{
CheckForMissingDates(
  AGG_TS_DATA,
  DATE_COLUMN = "SalesDate",
  TARGET_COLUMN = "vals",
  TIME_FRAME = "day",
  DEBUG = TRUE
)
}
\arguments{
\item{AGG_TS_DATA}{The raw dataset that will be aggregated}

\item{DATE_COLUMN}{The column name containing date value}

\item{TARGET_COLUMN}{The column name containing target value}

\item{TIME_FRAME}{The date values in the data (day, week, or month)}

\item{DEBUG}{If TRUE, the function will run in debug mode}
}
\value{
returns a data table with information on whether there are missing dates
}
\description{
This function will check a time series data set for missing dates
}
\seealso{
Other Data Preparation: 
\code{\link{AddDays}()},
\code{\link{AddMissingDates}()},
\code{\link{AddMonths}()},
\code{\link{AddWeeks}()},
\code{\link{AggregateTimeSeriesData}()},
\code{\link{CreateOutputDates}()},
\code{\link{DoDataPartition}()},
\code{\link{FakeDataGenerator}()},
\code{\link{FlattenLongData}()},
\code{\link{RemoveSpecialChars}()}
}
\author{
Abraham Mathew
}
\concept{Data Preparation}
