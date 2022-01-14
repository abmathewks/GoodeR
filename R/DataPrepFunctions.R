### data_prep_functions.R



###############################################################################


#' @title AggregateTimeSeriesData
#'
#' @description This function will take raw data and aggregate it by date
#'
#' @author Abraham Mathew
#' @family Data Preparation
#'
#' @param RAW_TS_DATA The raw dataset that will be aggregated
#' @param DATE_COLUMN The column name containing date value
#' @param TARGET_COLUMN The column name containing target value
#' @param OTHER_VARIABLES_TO_AGG Other columns that will be aggregated
#' @param DIMENSION_COLUMN_NAMES If value provided, aggregation will be done by this column
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return returns a data table with the aggregated data by date and any potential group variables
#' 
#' @export
#' 
AggregateTimeSeriesData <- function(
      RAW_TS_DATA,
      DATE_COLUMN = "SalesDate",
      TARGET_COLUMN = "TotalSales",
      OTHER_VARIABLES_TO_AGG = NULL,
      DIMENSION_COLUMN_NAMES = NULL,
      DEBUG = TRUE){
  
    if(DEBUG) message("AggregateTimeSeriesData: Function Initialized  \n")
    
    if(!is.data.table(RAW_TS_DATA)){
         stop("AggregateTimeSeriesData: The input data is not a data table  \n")
    }
  
    if(!class(RAW_TS_DATA[[DATE_COLUMN]]) %chin% c("Date")) {
         stop("AggregateTimeSeriesData: The date column is not saved in date format  \n")
    }
    
    FUNCTION_OUTPUT <- list()
    
    ALL_COLUMNS_TO_AGG <- unique(c(TARGET_COLUMN, OTHER_VARIABLES_TO_AGG))
  
    if(!all(sapply(RAW_TS_DATA[, mget(ALL_COLUMNS_TO_AGG)], class) %chin% c("numeric","integer"))){ 
         stop("AggregateTimeSeriesData: ALL_COLUMNS_TO_AGG contain non numeric values  \n")
    } else { 
      
        if(is.null(DIMENSION_COLUMN_NAMES)){
              
            if(DEBUG) message("AggregateTimeSeriesData: Aggregate value by DATE_COLUMN  \n")    
              
            AGG_TS_DATA  <- RAW_TS_DATA[, lapply(.SD, sum), 
                                        by = get(DATE_COLUMN), 
                                        .SDcols = ALL_COLUMNS_TO_AGG]
            colnames(AGG_TS_DATA)[1] <- DATE_COLUMN
              
        } else {
              
            if(DEBUG) message("AggregateTimeSeriesData: Aggregate value by BY_WHICH_GROUP and DATE_COLUMN  \n")    
              
            AGG_TS_DATA  <- RAW_TS_DATA[, lapply(.SD, sum), 
                                        by = mget(c(DIMENSION_COLUMN_NAMES, DATE_COLUMN)), 
                                        .SDcols = ALL_COLUMNS_TO_AGG]
                
        }
    }
    
    FUNCTION_OUTPUT[["DATE_COLUMN"]] <- DATE_COLUMN
    FUNCTION_OUTPUT[["TARGET_COLUMN"]] <- TARGET_COLUMN
    FUNCTION_OUTPUT[["OTHER_VARIABLES_TO_AGG"]] <- OTHER_VARIABLES_TO_AGG
    FUNCTION_OUTPUT[["ALL_COLUMNS_TO_AGG"]] <- ALL_COLUMNS_TO_AGG
    FUNCTION_OUTPUT[["DIMENSION_COLUMN_NAMES"]] <- ifelse(is.null(DIMENSION_COLUMN_NAMES), "NULL", DIMENSION_COLUMN_NAMES)
    
    if(DEBUG) message("AggregateTimeSeriesData: Collecting final output  \n")
    
    FUNCTION_OUTPUT[["FINAL_DATA"]] <- AGG_TS_DATA  
      
    if(!is.null(AGG_TS_DATA) || !nrow(AGG_TS_DATA) == 0){
        if(DEBUG) message("AggregateTimeSeriesData: Data acquisition completed  \n")
          
        return(FUNCTION_OUTPUT)
    } else {
        stop("AggregateTimeSeriesData: Function returned an empty data set  \n")
    }
  
}  


###############################################################################


#' @title FlattenLongData
#'
#' @description This function will take data in long format and flatten it
#'
#' @author Abraham Mathew
#' @family Data Preparation
#'
#' @param WHICH_LONG_DATA A data.table which contains data in long format
#' @param GROUP_BY_VAR The variables that will comprise the grouping columns
#' @param NEW_COLUMN_VAR The variables that will be added as new columns
#' @param VALUE_VAR The numeric variable that will be aggregated in the new columns
#' @param AGG_METHOD Aggregation method that will be used on each element in the ValueVar
#' @param DEBUG If TRUE, the function will be run in debug mode
#'
#' @return Returns a data table with the flattened dataset
#' 
#' @export
#' 
FlattenLongData <- function(
          WHICH_LONG_DATA,
          GROUP_BY_VAR = c("Date_new"),
          NEW_COLUMN_VAR = c("Currency"),
          VALUE_VAR = c("Amount_new"),
          AGG_METHOD = c("sum"),
          DEBUG = TRUE){
  
    if(DEBUG) message("FlattenLongData: Function Initialized  \n")
  
    if(!is.data.table(WHICH_LONG_DATA)){
         stop("FlattenLongData: The input data is not a data table  \n")
    }
    
    if(!all(c(GROUP_BY_VAR, NEW_COLUMN_VAR, VALUE_VAR) %chin% unique(colnames(WHICH_LONG_DATA)))){
         stop("FlattenLongData: Some column names are not present in the data  \n")
    }
  
    FUNCTION_OUTPUT <- list()
  
    if(length(GROUP_BY_VAR) == 1 && length(NEW_COLUMN_VAR) == 1){
  
          if(DEBUG) message("FlattenLongData: DCast Long Data to Wide  \n")
  
          WideData <- data.table::dcast.data.table(WHICH_LONG_DATA,
                                                   paste0(GROUP_BY_VAR, " ~ ",
                                                          paste0(NEW_COLUMN_VAR, collapse = " + ")),
                                                   value.var = VALUE_VAR,
                                                   fun.aggregate = eval(parse(text=AGG_METHOD)))
         
    } else if(length(GROUP_BY_VAR) >= 2 || length(NEW_COLUMN_VAR) >= 2){
  
          if(DEBUG) message("FlattenLongData: DCast Long Data to Wide  \n")
  
          WideData <- data.table::dcast.data.table(WHICH_LONG_DATA,
                                                   paste0(paste0(GROUP_BY_VAR, collapse = " + "),
                                                          " ~ ", paste0(NEW_COLUMN_VAR, collapse = " + ")),
                                                   value.var = VALUE_VAR,
                                                   fun.aggregate = eval(parse(text=AGG_METHOD)))
          
    }
  
    FUNCTION_OUTPUT[["GROUP_BY_VAR"]] <- GROUP_BY_VAR
    FUNCTION_OUTPUT[["NEW_COLUMN_VAR"]] <- NEW_COLUMN_VAR
    FUNCTION_OUTPUT[["VALUE_VAR"]] <- VALUE_VAR
    FUNCTION_OUTPUT[["AGG_METHOD"]] <- AGG_METHOD
    
    if(DEBUG) message("FlattenLongData: Collecting final output  \n")
  
    FUNCTION_OUTPUT[["FINAL_DATA"]] <- WideData  
  
    if(!is.null(WideData) || !nrow(WideData) == 0){
        if(DEBUG) message("FlattenLongData: Data acquisition completed  \n")
      
         return(FUNCTION_OUTPUT)
    } else {
       stop("FlattenLongData: Function returned an empty data set  \n")
    }

}




###############################################################################
 
 
#' @title CheckForMissingDates
#'
#' @description This function will check a time series data set for missing dates
#'
#' @author Abraham Mathew
#' @family Data Preparation
#'
#' @param AGG_TS_DATA The raw dataset that will be aggregated
#' @param DATE_COLUMN The column name containing date value
#' @param TARGET_COLUMN The column name containing target value
#' @param TIME_FRAME The date values in the data (day, week, or month)
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return returns a data table with information on whether there are missing dates
#' 
#' @export
#' 
CheckForMissingDates <- function(
            AGG_TS_DATA,
            DATE_COLUMN = "SalesDate",
            TARGET_COLUMN = "vals",
            TIME_FRAME = "day", 
            DEBUG = TRUE){

    if(DEBUG) message("CheckForMissingDates: Function Initialized  \n")

    if(!data.table::is.data.table(AGG_TS_DATA)){
        stop("CheckForMissingDates: The input data is not a data table  \n")
    }

    if(!class(AGG_TS_DATA[[DATE_COLUMN]]) %chin% c("Date")) {
        stop("CheckForMissingDates: The date column is not in date format  \n")
    }

    if(!all(c(DATE_COLUMN, TARGET_COLUMN) %in% colnames(AGG_TS_DATA))){
        stop("CheckForMissingDates: The date or target column is not in the data set  \n")
    }

    FUNCTION_OUTPUT <- list()

    FUNCTION_OUTPUT[["DATE_COLUMN"]] <- DATE_COLUMN
    FUNCTION_OUTPUT[["TARGET_COLUMN"]] <- TARGET_COLUMN
    
    all_date_info <- AGG_TS_DATA[, .(min_date = min(AGG_TS_DATA[[DATE_COLUMN]]),
                                     max_date = max(AGG_TS_DATA[[DATE_COLUMN]]),
                                     unique_dates = uniqueN(AGG_TS_DATA[[DATE_COLUMN]]),
                                     dates_between = length(unique(seq.Date(min(AGG_TS_DATA[[DATE_COLUMN]]),
                                                                           max(AGG_TS_DATA[[DATE_COLUMN]]),
                                                                           by = TIME_FRAME))))]

    all_date_info[, missing_dates := ifelse(unique_dates == dates_between, 0, 1)]

    if(DEBUG) message("CheckForMissingDates: Collecting final output  \n")
    
    FUNCTION_OUTPUT[["DATE_SUMMARY"]] <- all_date_info
    
    FUNCTION_OUTPUT[["MISSING_DATES_PRESENT"]] <- all_date_info$missing_dates == 1
    
    if(!is.null(FUNCTION_OUTPUT) || !length(FUNCTION_OUTPUT) == 0){
        if(DEBUG) message("CheckForMissingDates: Date check ompleted  \n ")
             
        return(FUNCTION_OUTPUT)
    } else {
        stop("CheckForMissingDates: Function returned an empty data set  \n")
    }
    
}


###############################################################################



#' @title AddMissingDates
#'
#' @description This function will add missing dates in a time series
#'
#' @author Abraham Mathew
#' @family Data Preparation
#'
#' @param AGG_TS_DATA The raw dataset that will be aggregated
#' @param DATE_COLUMN The column name containing date value
#' @param TARGET_COLUMN The column name containing target value
#' @param TIME_FRAME The date values in the data (day, week, or month)
#' @param IMPUTE_VALUE The value to impute missing dates with
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return returns a data table with all missing dates imputed with a imputed value
#' 
#' @export
#' 
AddMissingDates <- function(
            AGG_TS_DATA,
            DATE_COLUMN = "date",
            TARGET_COLUMN = "vals",
            TIME_FRAME = "day", 
            IMPUTE_VALUE = 0.1,
            DEBUG = TRUE){

    if(DEBUG) message("AddMissingDates: Function Initialized  \n")

    if(!data.table::is.data.table(AGG_TS_DATA)){
        stop("AddMissingDates: The input data is not a data table  \n")
    }

    if(!class(AGG_TS_DATA[[DATE_COLUMN]]) %chin% c("Date")) {
        stop("AddMissingDates: The date column is not saved in date format  \n")
    }

    if(!all(c(DATE_COLUMN, TARGET_COLUMN) %in% colnames(AGG_TS_DATA))){
        stop("AddMissingDates: The date or target column is not in the data set  \n")
    }

    FUNCTION_OUTPUT <- list()

    FUNCTION_OUTPUT[["DATE_COLUMN"]] <- DATE_COLUMN
    FUNCTION_OUTPUT[["TARGET_COLUMN"]] <- TARGET_COLUMN

    all_dates <- data.table::data.table(
                       date_ymd = seq.Date(min(as.Date(AGG_TS_DATA[[DATE_COLUMN]])),
                                           max(as.Date(AGG_TS_DATA[[DATE_COLUMN]])), 
                                           by = TIME_FRAME),
                       value = NA)
            
    colnames(all_dates)[1] <- DATE_COLUMN
    # all_dates

    AGG_TS_DATA <- data.table::merge.data.table(all_dates, 
                                    AGG_TS_DATA,
                                     by = DATE_COLUMN,
                                     all.x = TRUE)
     # AGG_TS_DATA

     AGG_TS_DATA[, paste0(TARGET_COLUMN, "_new", sep = "") := ifelse(is.na(get(TARGET_COLUMN)), IMPUTE_VALUE, get(TARGET_COLUMN))]
     # AGG_TS_DATA

     FINAL_DATA <- AGG_TS_DATA[, mget(c(DATE_COLUMN, TARGET_COLUMN, paste0(TARGET_COLUMN, "_new", sep = "")))]

     if(DEBUG) message("AddMissingDates: Collecting final output  \n")
     
     FUNCTION_OUTPUT[["FINAL_OUTPUT"]] <- FINAL_DATA

     if(!is.null(FUNCTION_OUTPUT) || !length(FUNCTION_OUTPUT) == 0){
         if(DEBUG) message("AddMissingDates: Date check ompleted  \n ")
             
         return(FUNCTION_OUTPUT)
    } else {
        stop("AddMissingDates: Function returned an empty data set  \n")
    }
}
 

###############################################################################




###############################################################################



###############################################################################



###############################################################################





