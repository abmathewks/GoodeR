% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelingFunctions.R
\name{RunAllModels}
\alias{RunAllModels}
\title{RunAllModels}
\usage{
RunAllModels(
  USE_THESE_FORMULAS,
  MODEL_DATA,
  DATE_COLUMN = "date_ymd",
  TARGET_COLUMN = "visits",
  DEBUG = TRUE
)
}
\arguments{
\item{USE_THESE_FORMULAS}{A list of formulas}

\item{MODEL_DATA}{The column names containing character values that need to be encoded}

\item{DATE_COLUMN}{Drop any levels that are missing (should be FALSE)}

\item{TARGET_COLUMN}{Drop any levels that are missing (should be FALSE)}

\item{DEBUG}{If TRUE, the function will run in debug mode}
}
\value{
returns the originak data table with all the new features appended to the dataset
}
\description{
This function will loop through a list of regression formulas and generate models using lm
}
\seealso{
Other Modeling Functions: 
\code{\link{DoOneHotEncoding}()},
\code{\link{mae}()},
\code{\link{rmse}()}
}
\author{
Abraham Mathew
}
\concept{Modeling Functions}
