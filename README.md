
<p align = "center">
  <img width = "500" src = "https://github.com/abmathewks/GoodeR/blob/main/images/Logo.png">
</p>

<p align = "center">
  The GoodeR package contains a variety of functions that streamline common tasks performed by data scientists and
  other analytics professionals who use the R programming language. From data exploration to model evaluation, this 
  package offers a set of functions that will simplify your work and construct baseline models quicker. GoodeR has 
  been developed over the past few years and has enabled me to deliver exceptionally high quality solutions in minimal 
  time. As a result, I can focus more time on the other aspects of projects that are critical for generating a high ROI 
  and high adoption rate.
</p>


# Contents

- [Installation](#installation)
- [Project Initialization](#project-initialization)
- [Data Preparation](#data-preparation)
- [Data Acquisition](#data-acquisition)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Statistical Modeling](#statistical-modeling)
- [Time Series Forecasting](#time-series-forecasting)
- [Machine Learning](#machine-learning)
- [Model Evaluation](#model-evaluation)


# Installation

The GoodeR package can be installed directly from GitHub using the devtools package. 

```r

devtools::install_github("abmathewks/GoodeR", dependencies = TRUE)

```

# Project Initialization

GoodeR contains a number of function to initialize and set up a project so that directories and files are ready for use.

- CreateProjectFiles: This function allows a user to create a project and utilize a standard folder structure for a project.

```r

CreateProjectFiles(PROJECT_PATH = rstudioapi::getActiveProject(),
                   FOLDERS_TO_CREATE = c("data", "docs", "figs", "logs",
                                         "output", "queries", "R", "tests"),
                   OPEN_NEW_SESSION = TRUE,
                   DEBUG = TRUE)
                               
```

<p align = "center">
  <img width = "500" src = "https://github.com/abmathewks/GoodeR/blob/main/images/FolderStructure.png">
</p>

- PackageCheck: This function takes a vector of package names, checks whether they are installed, and imports all of them into the current R session.

```r

GoodeR::PackageCheck(c("ggplot2","data.table","lubridate","rms"))
                         
```                         

<p align = "center">
  <img width = "500" src = "https://github.com/abmathewks/GoodeR/blob/main/images/PackageCheck.png">
</p>





