### FakeDataGenerator.R


###################################################################################################



#' @title FakeDataGenerator
#'
#' @description Create fake data for examples
#'
#' @author Abraham Mathew
#' @family Data Preparation 
#'
#' @param NUM_ROWS Number of records
#' @param DATE_COLS Number of date columns
#' @param NUMERIC_COLS Number of numeric columns
#' @param CHARACTER_COLS Number of character columns
#' @param LOGICAL_COLS Number of logical columns
#' @param FACTOR_COLS Number of factor columns
#' @param DEBUG If TRUE, the function will run in debug mode
#' @examples
#' \dontrun{
#' 
#' data <- RemixAutoML::FakeDataGenerator(
#'                            NUM_ROWS = 10000,
#'                            DATE_COLS = 1,
#'                            NUMERIC_COLS = 3,
#'                            CHARACTER_COLS = 1,
#'                            LOGICAL_COLS = 0,
#'                            FACTOR_COLS = 2,
#'                            DEBUG = TRUE)
#' }
#' 
#' @export
FakeDataGenerator <- function(NUM_ROWS = 10000,
                              DATE_COLS = 1,
                              NUMERIC_COLS = 3,
                              CHARACTER_COLS = 1,
                              LOGICAL_COLS = 0,
                              FACTOR_COLS = 2,
                              DEBUG = TRUE) {
  
    if(DEBUG) message("FakeDataGenerator: Function initialized  \n")
  
    if(!all(is.numeric(c(NUM_ROWS, DATE_COLS, NUMERIC_COLS, CHARACTER_COLS, LOGICAL_COLS, FACTOR_COLS)))){
        stop("FakeDataGenerator: Function arguments are not numeric or integer values  \n")
    }
  
    FUNCTION_OUTPUT <- list()

    function_params <- data.table::data.table(
          metric = c("NUM_ROWS_VAL",
                     "DATE_COLS_VAL",
                     "NUMERIC_COLS_VAL",
                     "CHARACTER_COLS_VAL",
                     "LOGICAL_COLS_VAL",
                     "FACTOR_COLS_VAL"),
          value = c(NUM_ROWS,
                    DATE_COLS,
                    NUMERIC_COLS,
                    CHARACTER_COLS,
                    LOGICAL_COLS,
                    FACTOR_COLS)
    )
    
    FUNCTION_OUTPUT[["FUNCTION_PARAMETERS"]] <- function_params

    if(DEBUG) message("FakeDataGenerator: Create columns by type  \n")
    
    date_cols <- data.table::data.table(date_ymd = sample(seq.Date(as.Date("2011-01-01"), as.Date("2020-01-01"), by = "day"), NUM_ROWS, replace = TRUE))

    numeric_cols <- data.table::data.table(replicate(NUMERIC_COLS, sample(10:100, NUM_ROWS, replace = TRUE)))
      
    character_cols <- data.table::data.table(replicate(CHARACTER_COLS, sample(LETTERS[1:5], NUM_ROWS, replace = TRUE)))
    
    logical_cols <- data.table::data.table(replicate(LOGICAL_COLS, sample(c(TRUE,FALSE), NUM_ROWS, replace = TRUE)))
    
    factor_cols <- data.table::data.table(replicate(FACTOR_COLS, sample(as.factor(LETTERS[1:5]), NUM_ROWS, replace = TRUE)))
    
    if(DEBUG) message("FakeDataGenerator: Bind together all data tables  \n")

    results <- do.call(cbind, list(date_cols, 
                                      numeric_cols,
                                      character_cols,
                                      logical_cols,
                                      factor_cols))

    if(DEBUG) message("FakeDataGenerator: Collecting final output  \n")
    
    FUNCTION_OUTPUT[["FAKE_DATA"]] <- results
    
    if(!is.null(results) || !nrow(results) == 0){
        if(DEBUG) message("FakeDataGenerator: Data acquisition completed  \n")
    
        return(FUNCTION_OUTPUT)
    } else {
        stop("FakeDataGenerator: Function returned an empty data set  \n")
    }

}


###################################################################################################



###################################################################################################




