### modeling_functions.R



###############################################################################


#' @title rmse
#'
#' @description Caculates the rmse metric from model predictions
#'
#' @author Abraham Mathew
#' @family Modeling Functions
#'
#' @param actual Actual data from the test data
#' @param predicted Predicted values from the predictive model
#'
#' @return A single numeric value
#' 
#' @examples
#' \donttest{
#' 
#'     GoodeR::rmse(actual, predicted)
#' 
#' }
#' @export
#' 
rmse <- function(actual, predicted){
  error <- actual - predicted
  sqrt(mean(error^2))
}


###############################################################################


#' @title mae
#'
#' @description Caculate the mae metric from model predictions
#'
#' @author Abraham Mathew
#' @family Modeling Functions
#'
#' @param actual Actual data from the test data
#' @param predicted Predicted values from the predictive model
#'
#' @return A single numeric value
#' 
#' @examples
#' \donttest{
#' 
#'     GoodeR::mae(actual, predicted)
#' 
#' }
#' @export
#' 
mae <- function(actual, predicted){
  error <- actual - predicted
  mean(abs(error))
}





###############################################################################



#' @title RunAllModels
#'
#' @description This function will loop through a list of regression formulas and generate models using lm
#'
#' @author Abraham Mathew
#' @family Modeling Functions
#'
#' @param USE_THESE_FORMULAS A list of formulas 
#' @param MODEL_DATA The column names containing character values that need to be encoded
#' @param DATA_DIMENSION Remove the original character variable
#' @param DATE_COLUMN Drop any levels that are missing (should be FALSE)
#' @param TARGET_COLUMN Drop any levels that are missing (should be FALSE)
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return returns the originak data table with all the new features appended to the dataset
#' 
#' @examples
#' \donttest{
#' 
#'     GoodeR::RunAllModels(USE_THESE_FORMULAS = all_formulas,
#'                         MODEL_DATA = model_data,
#'                         DATA_DIMENSION = dim_name,
#'                         DATE_COLUMN = "date_ymd",
#'                         TARGET_COLUMN = "visits",
#'                         DEBUG = TRUE)
#' 
#' }
#' @export
#'
RunAllModels <- function(USE_THESE_FORMULAS = all_formulas,
                         MODEL_DATA = model_data,
                         DATA_DIMENSION = dim_name,
                         DATE_COLUMN = "date_ymd",
                         TARGET_COLUMN = "visits",
                         DEBUG = TRUE){
  
  if(DEBUG) message("RunAllModels: Function Initialized  \n")
  
  if(!is.list(USE_THESE_FORMULAS)){
    stop("RunAllModels: The input must be a list of character strings....please investigate!  \n")
  }
  
  if(!is.data.table(MODEL_DATA)){
    stop("RunAllModels: The input data is not a data table  \n")
  }
  
  FUNCTION_OUTPUT <- list()
  
  # each_formula = USE_THESE_FORMULAS[[1]]  
  for(each_formula in USE_THESE_FORMULAS){
    
    tryCatch({
      
        message("Executing Model: ", each_formula, "  \n")
        
        # if(!DO_PENALIZATION){ 
        
        MOD_FIT <- lm(each_formula, data = MODEL_DATA)
        
        MOD_RESULTS_DT <- data.table::setDT(broom::tidy(summary(MOD_FIT)))
        cols <- names(MOD_RESULTS_DT)[2:ncol(MOD_RESULTS_DT)]
        MOD_RESULTS_DT[ ,(cols) := round(.SD, 3), .SDcols = cols]
        MOD_RESULTS_DT[, mod_formula := each_formula]
        MOD_RESULTS_DT[, dimension_name := DATA_DIMENSION]
        # MOD_RESULTS_DT
        
        MODEL_EVAL_DT <- data.table(FULL_FORMULA = each_formula)
        MODEL_EVAL_DT[, c("TARGET", "PREDICTORS") := tstrsplit(FULL_FORMULA, "~", fixed=TRUE, fill="<NA>")]
        MODEL_EVAL_DT[, RSQUARED := summary(MOD_FIT)$r.squared]
        MODEL_EVAL_DT[, ADJ_RSQUARED := summary(MOD_FIT)$adj.r.squared]
        MODEL_EVAL_DT[, MSE := mean(MOD_FIT$residuals^2) ]
        MODEL_EVAL_DT[, RMSE := sqrt(mean(MOD_FIT$residuals^2)) ]
        MODEL_EVAL_DT[, DIMENSION_NAME := DATA_DIMENSION]
  
        
        PLT_DATA_NEW <- data.table(
            date_ymd = MODEL_DATA[[DATE_COLUMN]], 
            actuals = round(as.numeric(MODEL_DATA[[TARGET_COLUMN]], 0)),
            fittedvals = round(as.numeric(fitted(MOD_FIT), 0))
        )
          
        PLT_DATA_NEW[, mod_formula := each_formula]
        PLT_DATA_NEW[, dimension_name := DATA_DIMENSION]
  
        
        FUNCTION_OUTPUT[[each_formula]][["MODEL_FIT"]] <- MOD_FIT
        
        FUNCTION_OUTPUT[[each_formula]][["MODEL_RESULTS"]] <- MOD_RESULTS_DT
        
        FUNCTION_OUTPUT[[each_formula]][["MODEL_EVALUATION"]] <- MODEL_EVAL_DT
        
        FUNCTION_OUTPUT[[each_formula]][["ACTUALS_FITTED_DATA"]] <- PLT_DATA_NEW
      
      
    }, error = function(cond) {
      message(paste("RunAllModels", "eror", cond, sep = "  -  "))
    })
    
  }
  return(FUNCTION_OUTPUT)
}



###############################################################################



#' @title DoOneHotEncoding
#'
#' @description This function will take specified column names and append them as one hot encoded variables
#'
#' @author Abraham Mathew
#' @family Modeling Functions
#'
#' @param AGG_TS_DATA The processed dataset that will be used in the predictive model
#' @param COLS_TO_ENCODE The column names containing character values that need to be encoded
#' @param DROP_ORIGINAL_COLS Remove the original character variable
#' @param DROP_UNUSED_LEVELS Drop any levels that are missing (should be FALSE)
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return returns the originak data table with all the new features appended to the dataset
#' 
#' @examples
#' \donttest{
#' 
#'     GoodeR::DoOneHotEncoding(MODEL_DATA,
#'                       COLS_TO_ENCODE = c("LineOfBusiness", "Category"),
#'                       DROP_ORIGINAL_COLS = FALSE,
#'                       DROP_UNUSED_LEVELS = FALSE,
#'                       DEBUG = TRUE)
#' 
#' }
#' @export
#' 
DoOneHotEncoding <- function(MODEL_DATA,
                             COLS_TO_ENCODE = c("LineOfBusiness", "Category"),
                             DROP_ORIGINAL_COLS = FALSE,
                             DROP_UNUSED_LEVELS = FALSE,
                             DEBUG = TRUE){

    if(DEBUG) message("DoOneHotEncoding: Function Initialized  \n")
    
    if(!data.table::is.data.table(AGG_TS_DATA)){
         stop("DoOneHotEncoding: The input data is not a data table  \n")
    }
  
    FUNCTION_OUTPUT <- list()

    FUNCTION_OUTPUT[["COLS_TO_ENCODE"]] <- COLS_TO_ENCODE
    FUNCTION_OUTPUT[["ORIGINAL_DATA"]] <- MODEL_DATA

    if(DEBUG) message("DoOneHotEncoding: Collect necessary data  \n")

    # Build tempDT containing and ID column and 'cols' columns
    tempDT <- MODEL_DATA[, mget(COLS_TO_ENCODE)]
    tempDT[, ID := .I]
    for(col in COLS_TO_ENCODE){
        set(tempDT, 
            j = col, 
            value = factor(paste(col, tempDT[[col]], sep="_"),
                           levels = paste(col, levels(tempDT[[col]]), sep = "_")))
    }

    if(DEBUG) message("DoOneHotEncoding: Encode character variables  \n")
    
    # One-hot-encode
    melted <- melt(tempDT, id = 'ID', value.factor = TRUE, na.rm=TRUE)
    if(DROP_UNUSED_LEVELS){
      newCols <- dcast(melted, ID ~ value, drop = TRUE, fun.aggregate = length)
    } else{
      newCols <- dcast(melted, ID ~ value, drop = FALSE, fun.aggregate = length)
    }

    # Fill in potentially missing rows
    newCols <- newCols[tempDT[, list(ID)]]
    newCols[is.na(newCols[[2]]), setdiff(paste(colnames(newCols)), "ID") := 0L]

    if(DEBUG) message("DoOneHotEncoding: Clean final output  \n")

    #--------------------------------------------------
    # Clean Up

    # Combine binarized columns with the original dataset
    result <- cbind(MODEL_DATA, newCols[, !"ID"])
  
    # Reorder columns
    possible_colnames <- character(0)
    for(col in colnames(result)){
      possible_colnames <- c(possible_colnames, col)
      if(col %in% COLS_TO_ENCODE){
          possible_colnames <- c(possible_colnames, paste0(col, "_NA"))
          possible_colnames <- c(possible_colnames, paste(levels(tempDT[[col]])))
      }
    }
    sorted_colnames <- intersect(possible_colnames, colnames(result))
    setcolorder(result, sorted_colnames)
  
    # If dropCols = TRUE, remove the original factor columns
    if(DROP_ORIGINAL_COLS){
        result <- result[, !COLS_TO_ENCODE, with=FALSE]
    }
    
    if(DEBUG) message("DoOneHotEncoding: Collecting final output  \n")
    
    FUNCTION_OUTPUT[["FINAL_DATA"]] <- result  
      
    if(!is.null(result) || !nrow(result) == 0){
        if(DEBUG) message("DoOneHotEncoding: Data acquisition completed  \n")
          
         return(FUNCTION_OUTPUT)
    } else {
         stop("DoOneHotEncoding: Function returned an empty data set  \n")
    }
  
}



###############################################################################


#' @title DoDataPartition
#'
#' @description This function will take one dataset and return data for training and testing
#'
#' @author Abraham Mathew
#' @family Data Preparation
#'
#' @param DATA_DT A data.table which contains the main dataset
#' @param DATE_COLUMN The variable which contains date values
#' @param TARGET_COLUMN The variable which contains the target variable
#' @param WHICH_FEATURES The variables that will need to be in the final dataset
#' @param TRAIN_RATIO Percentage of rows that will be in the dataset
#' @param PARTITION_TYPE Partition by time or randomly
#' @param DEBUG If TRUE, the function will be run in debug mode
#'
#' @examples
#' \dontrun{
#'
#'    GoodeR::DoDataPartition(
#'            DATA_DT,
#'            DATE_COLUMN = NULL,
#'            TARGET_COLUMN = NULL,
#'            WHICH_FEATURES = NULL,
#'            TRAIN_RATIO = 0.80,
#'            PARTITION_TYPE = "time",  # time or random 
#'            DEBUG = TRUE)
#'      
#' 
#' @return Returns two separate datasets for training and testing
#' @export
#' 
DoDataPartition <- function(
       DATA_DT,
       DATE_COLUMN = "SalesDate",
       TARGET_COLUMN = "Sales",
       WHICH_FEATURES = NULL,
       TRAIN_RATIO = 0.80,
       PARTITION_TYPE = "time",  # time or random 
       DEBUG = TRUE){
  
  if(DEBUG) message("DoDataPartition: Function Initialized  \n")

  if(!is.data.table(DATA_DT)){
       stop("DoDataPartition: The input data is not a data table  \n")
  }

  if(!any(c(DATE_COLUMN, TARGET_COLUMN, WHICH_FEATURES) %chin% colnames(DATA_DT))){
       stop("DoDataPartition: Some variable names are missing in the dataset  \n")
  }
  
  FUNCTION_OUTPUT <- list()

  ALL_FEATURES <- unique(c(DATE_COLUMN, TARGET_COLUMN, WHICH_FEATURES))

  if(PARTITION_TYPE == "time"){
    
      if(DEBUG) message("DoDataPartition: Partition by time  \n")
                      
      smp_size <- floor(TRAIN_RATIO * nrow(DATA_DT))
              
      DATA_DT_Train <- DATA_DT[1:smp_size, ]
            
      DATA_DT_Test <- DATA_DT[(smp_size+1):nrow(DATA_DT), ]
                      
  } else if(PARTITION_TYPE == "random"){
          
      if(DEBUG) message("DoDataPartition: Random partition  \n")
            
      inTrain <- DATA_DT[, sample(.N, floor(.N*TRAIN_RATIO))]
              
      DATA_DT_Train <- DATA_DT[inTrain, ]
              
      DATA_DT_Test <- DATA_DT[-inTrain, ]
        
  } else {
        
      stop("DoDataPartition: NUM_DATA_SETS input is incorrect  \n")
        
  }
  
  FUNCTION_OUTPUT[["DATE_COLUMN"]] <- DATE_COLUMN
  FUNCTION_OUTPUT[["TARGET_COLUMN"]] <- TARGET_COLUMN
  FUNCTION_OUTPUT[["WHICH_FEATURES"]] <- WHICH_FEATURES
  FUNCTION_OUTPUT[["TRAIN_RATIO"]] <- TRAIN_RATIO
  
  if(DEBUG) message("DoDataPartition: Collecting final output  \n")
  
  FUNCTION_OUTPUT[["FINAL_TRAIN_DATA"]] <- DATA_DT_Train
  FUNCTION_OUTPUT[["FINAL_TEST_DATA"]] <- DATA_DT_Test

  if(!nrow(DATA_DT_Train) == 0 && !nrow(DATA_DT_Test) == 0){
      if(DEBUG) message("DoDataPartition: Data acquisition completed  \n")
    
       return(FUNCTION_OUTPUT)
  } else {
     stop("DoDataPartition: Function returned an empty data set  \n")
  }
  
}


###############################################################################





###############################################################################





###############################################################################




###############################################################################



###############################################################################

  


