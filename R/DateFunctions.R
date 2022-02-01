### DateFunctions.R


###################################################################################################


#' @title AddMonths
#'
#' @description This function will take a single date value and add N months to it.
#'
#' @author Abraham Mathew
#' @family Data Preparation
#' 
#' @param DATE_VAL  The original date value that will be added to. Must be a date format 
#' @param ADD_N  Number of months to add to DATE_VAL
#' @param DEBUG If TRUE, the function will run in debug mode 
#'  
#' @return A single date value
#' 
#' @examples
#' 
#' \donttest{
#' 
#'     AddMonths(as.Date("2020-01-01"), 1)
#' 
#' }
#' @export
#' 
AddMonths <- function(DATE_VAL, 
                       ADD_N = NULL,
                       DEBUG = TRUE){ 
    
    if(DEBUG) message("AddMonths: Function initialized  \n")
  
    if(!lubridate::is.Date(DATE_VAL)){
        stop("AddMonths: DATE_VAL is not a date format  \n")
    }
  
    if(is.null(ADD_N)){
        stop("AddMonths: Argument ADD_N is missing  \n")
    }

    FUNCTION_OUTPUT <- list()

    FUNCTION_OUTPUT[["ORIGINAL_DATE"]] <- DATE_VAL
    FUNCTION_OUTPUT[["ADD_N"]] <- ADD_N

    new_date <- seq(DATE_VAL, by = paste(ADD_N, "months"), length = 2)[2]
    
    if(DEBUG) message("AddMonths: Collecting final output  \n")

    FUNCTION_OUTPUT[["NEW_DATE"]] <- new_date
  
    if(!is.null(new_date) || !is.na(new_date)){
        if(DEBUG) message("AddMonths: Function run completed  \n")
    
        return(FUNCTION_OUTPUT)
    } else {
        stop("AddMonths: Dates not created  \n")
    }
}


###################################################################################################


#' @title AddWeeks
#'
#' @description This function will take a single date value and add N weeks to it.
#'
#' @author Abraham Mathew
#' @family Data Preparation
#' 
#' @param DATE_VAL  The original date value that will be added to. Must be a date format 
#' @param ADD_N  Number of weeks to add to DATE_VAL
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return A single date value
#' 
#' @examples
#' 
#' \donttest{
#' 
#'     AddWeeks(as.Date("2020-01-01"), 1)
#' 
#' }
#' @export
#' 
AddWeeks <- function(DATE_VAL, 
                      ADD_N = NULL,
                      DEBUG = TRUE){ 
  
    if(DEBUG) message("AddWeeks: Function initialized  \n")
  
    if(!lubridate::is.Date(DATE_VAL)){
        stop("AddWeeks: DATE_VAL is not a date format  \n")
    }

    if(is.null(ADD_N)){
        stop("AddWeeks: Argument ADD_N is missing  \n")
    }

    FUNCTION_OUTPUT <- list()

    FUNCTION_OUTPUT[["ORIGINAL_DATE"]] <- DATE_VAL
    FUNCTION_OUTPUT[["ADD_N"]] <- ADD_N

    new_date <- seq(DATE_VAL, by = paste(ADD_N, "weeks"), length = 2)[2]
    
    if(DEBUG) message("AddWeeks: Collecting final output  \n")

    FUNCTION_OUTPUT[["NEW_DATE"]] <- new_date

    if(!is.null(new_date) || !is.na(new_date)){
        if(DEBUG) message("AddWeeks: Function run completed  \n")
    
        return(FUNCTION_OUTPUT)
    } else {
        stop("AddWeeks: Dates not created  \n")
    }
}


###################################################################################################


#' @title AddDays
#'
#' @description This function will take a single date value and add N days to it.
#'
#' @author Abraham Mathew
#' @family Data Preparation
#' 
#' @param DATE_VAL  The original date value that will be added to. Must be a date format 
#' @param ADD_N  Number of days to add to DATE_VAL
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return A single date value
#' 
#' @examples
#' 
#' \donttest{
#' 
#'     AddDays(as.Date("2020-01-01"), 1)
#' 
#' }
#' @export
#' 
AddDays <- function(DATE_VAL, 
                     ADD_N = NULL,
                     DEBUG = TRUE){ 
  
    if(DEBUG) message("AddDays: Function initialized  \n")
  
    if(!lubridate::is.Date(DATE_VAL)){
        stop("AddDays: DATE_VAL is not a date format  \n")
    }

    if(is.null(ADD_N)){
        stop("AddDays: Argument ADD_N is missing  \n")
    }

    FUNCTION_OUTPUT <- list()

    FUNCTION_OUTPUT[["ORIGINAL_DATE"]] <- DATE_VAL
    FUNCTION_OUTPUT[["ADD_N"]] <- ADD_N

    new_date <- seq(DATE_VAL, by = paste(ADD_N, "days"), length = 2)[2]
    
    if(DEBUG) message("AddDays: Collecting final output  \n")

    FUNCTION_OUTPUT[["NEW_DATE"]] <- new_date

    if(!is.null(new_date) || !is.na(new_date)){
        if(DEBUG) message("AddDays: Function run completed  \n")
    
        return(FUNCTION_OUTPUT)
    } else {
        stop("AddDays: Dates not created  \n")
    }
}



###################################################################################################


#' @title CreateOutputDates
#'
#' @description This function will collect date values for the output of the forecasting output 
#'
#' @author Abraham Mathew
#' @family Data Preparation
#' 
#' @param FULL_DATA  The full data set that will be used in the forecasting models
#' @param DATE_COLUMN  The column name of the date variable
#' @param DATA_TYPE_COLUMN  The column name of the data type variable
#' @param FORECAST_HORIZON  Forecast horizon for the prediction
#' @param TIME_FRAME  Is the forecast data daily, weekly, or monthly data
#' @param EVAL_MODE  If TRUE, only perform train and test on the actual data 
#' @param DEBUG If TRUE, the function will run in debug mode 
#' 
#' @export
#' 
CreateOutputDates <- function(FULL_DATA,
                              DATE_COLUMN = "SalesDate",
                              DATA_TYPE_COLUMN = "DataType",
                              FORECAST_HORIZON = 30,
                              TIME_FRAME = "day",
                              EVAL_MODE = TRUE,
                              DEBUG = TRUE){
          
    if(DEBUG) message("CreateOutputDates: Function initialized  \n")

    if(!data.table::is.data.table(FULL_DATA)){
        stop("CreateOutputDates: One one the input datasets is not a data.table  \n")
    }
  
    if(!c(DATE_COLUMN, DATA_TYPE_COLUMN) %chin% colnames(FULL_DATA)){
        stop("CreateOutputDates: Columm name inputs are not in the data  \n")
    }
    
    if(TIME_FRAME %chin% c("day","week","month")){
        stop("CreateOutputDates: The time frame input is not accepted  \n")
    }
  
    FUNCTION_OUTPUT <- list()
    
    FUNCTION_OUTPUT[["EVAL_MODE"]] <- EVAL_MODE
    FUNCTION_OUTPUT[["DATE_COLUMN"]] <- DATE_COLUMN
    FUNCTION_OUTPUT[["DATA_TYPE_COLUMN"]] <- DATA_TYPE_COLUMN
    FUNCTION_OUTPUT[["TIME_FRAME"]] <- TIME_FRAME
    FUNCTION_OUTPUT[["FORECAST_HORIZON"]] <- FORECAST_HORIZON

    FUNCTION_OUTPUT[["ALL_DATES"]] <- FULL_DATA[, get(DATE_COLUMN)]
    
    if(EVAL_MODE){

          if(DEBUG) message("CreateOutputDates: Executing eval mode process  \n")  
      
          model_train_dates <- FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN",  get(DATE_COLUMN)]
          
          fcast_dates <- FULL_DATA[get(DATA_TYPE_COLUMN) == "TEST", get(DATE_COLUMN)]

    } else {
        
        if(DEBUG) message("CreateOutputDates: Executing non eval mode process  \n")  
  
        if(TIME_FRAME == "month"){       
          
             model_train_dates <- FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN", get(DATE_COLUMN)]
  
             fcast_dates <- seq.Date(AddMonths(FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN"][.N, get(DATE_COLUMN)], 1)[["NEW_DATE"]],
                                     AddMonths(FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN"][.N, get(DATE_COLUMN)], FORECAST_HORIZON)[["NEW_DATE"]], 
                                     by = TIME_FRAME)
        }            
        
        if(TIME_FRAME == "week"){       
          
             model_train_dates <- FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN", get(DATE_COLUMN)]
             
             fcast_dates <- seq.Date(AddWeeks(FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN"][.N, get(DATE_COLUMN)], 1)[["NEW_DATE"]],
                                     AddWeeks(FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN"][.N, get(DATE_COLUMN)], FORECAST_HORIZON)[["NEW_DATE"]], 
                                     by = TIME_FRAME)
        }            
  
        
        if(TIME_FRAME == "day"){       
          
             model_train_dates <- FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN", get(DATE_COLUMN)]
             
             fcast_dates <- seq.Date(AddDays(FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN"][.N, get(DATE_COLUMN)], 1)[["NEW_DATE"]],
                                     AddDays(FULL_DATA[get(DATA_TYPE_COLUMN) == "TRAIN"][.N, get(DATE_COLUMN)], FORECAST_HORIZON)[["NEW_DATE"]], 
                                     by = TIME_FRAME)
        }            

    }

    if(DEBUG) message("CreateOutputDates: Collecting final output  \n")

    FUNCTION_OUTPUT[["MODEL_TRAIN_DATES"]] <- model_train_dates
    FUNCTION_OUTPUT[["FORECAST_DATES"]] <- fcast_dates

    if((!is.null(model_train_dates) || !is.na(model_train_dates)) || 
           (!is.null(fcast_dates) || !is.na(fcast_dates))){
        if(DEBUG) message("CreateOutputDates: Function run completed  \n")
    
        return(FUNCTION_OUTPUT)
    } else {
        stop("CreateOutputDates: Dates not created  \n")
    }
}



###################################################################################################


#' @title RemoveSpecialChars
#'
#' @description This function will take a string and clean it 
#'
#' @author Abraham Mathew
#' @family Data Preparation
#' 
#' @param STRING_VEC a vector of strings
#' @param IS_COLNAME If TRUE, the function replace column names 
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return A string value that has been cleaned
#' 
#' @examples
#' 
#' \donttest{
#' 
#'      RemoveSpecialChars("This is&%?? is a sting")
#' 
#' }
#' @export
#'  
RemoveSpecialChars <- function(STRING_VEC,
                                 IS_COLNAME = TRUE,
                                 DEBUG = TRUE){

    if(DEBUG) message("RemoveSpecialChars: Function Initialized  \n")
  
    FUNCTION_OUTPUT <- list()
  
    FUNCTION_OUTPUT[["ORIGINAL_STRING"]] <- STRING_VEC
    FUNCTION_OUTPUT[["IS_COLNAME"]] <- IS_COLNAME
    
    if(!IS_COLNAME){ 
        STRING_VEC <- gsub("[[:punct:]]", "", STRING_VEC)
        STRING_VEC <- tolower(STRING_VEC)
    } else {
        STRING_VEC <- gsub("[[:punct:]]", " ", STRING_VEC)
        STRING_VEC <- gsub(" ", "_", STRING_VEC)
        STRING_VEC <- tolower(STRING_VEC)
    }
  
    if(DEBUG) message("RemoveSpecialChars: Collecting final output  \n")
    
    FUNCTION_OUTPUT[["NEW_STRING"]] <- STRING_VEC
    
    if(nchar(STRING_VEC) >= 1){
          if(DEBUG) message("RemoveSpecialChars: Function run completed  \n")
      
          return(FUNCTION_OUTPUT)
    } else {
          stop("RemoveSpecialChars: Strings not cleaned  \n")
    }

}


###################################################################################################




###################################################################################################



###################################################################################################



###################################################################################################





