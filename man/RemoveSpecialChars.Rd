% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataPrepFunctions.R
\name{RemoveSpecialChars}
\alias{RemoveSpecialChars}
\title{RemoveSpecialChars}
\usage{
RemoveSpecialChars(STRING_VEC, IS_COLNAME = TRUE, DEBUG = TRUE)
}
\arguments{
\item{STRING_VEC}{a vector of strings}

\item{IS_COLNAME}{If TRUE, the function replace column names}

\item{DEBUG}{If TRUE, the function will run in debug mode}
}
\value{
A string value that has been cleaned
}
\description{
This function will take a string and clean it
}
\examples{

\donttest{

     GoodeR::RemoveSpecialChars("This is&\%?? is a sting")

}

}
\seealso{
Other Data Preparation: 
\code{\link{AddDays}()},
\code{\link{AddMissingDates}()},
\code{\link{AddMonths}()},
\code{\link{AddWeeks}()},
\code{\link{AggregateTimeSeriesData}()},
\code{\link{CheckForMissingDates}()},
\code{\link{CreateOutputDates}()},
\code{\link{DoDataPartition}()},
\code{\link{FakeDataGenerator}()},
\code{\link{FlattenLongData}()}
}
\author{
Abraham Mathew
}
\concept{Data Preparation}
