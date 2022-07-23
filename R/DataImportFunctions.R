### data_import_functions.R 


###################################################################################################


#' @title GetCsvData
#'
#' @description This function will grab data from all csv files within a directory
#'
#' @author Abraham Mathew
#' @family Data Import
#'
#' @param PROJECT_PATH The main project directory
#' @param WHICH_DATA_FOLDER The name of the data sub directory from where data will be pulled 
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return Returns all the csv files in the data folder within a project as a data.table
#' 
#' @export
#' 
GetCsvData <- function(PROJECT_PATH = rstudioapi::getActiveProject(),
                       WHICH_DATA_FOLDER = "processed",
                       DEBUG = TRUE){
  
    if(DEBUG) message("GetCsvData: Script Initialized  \n")

    if(!dir.exists(PROJECT_PATH)){
        stop("GetCsvData: Project path does not exist  \n")
    }

    if(!getwd() == PROJECT_PATH){
        stop("GetCsvData: Project path argument is different from the current working directory  \n")
    }
    
    if(!any(grepl("Rproj", list.files()))){ 
        stop("GetCsvData: Project is not present  \n")
    }

    FUNCTION_OUTPUT <- list()
  
    if(DEBUG) message("GetCsvData: Find all files in the data folder  \n")

    all_folders <- file.info(list.files(file.path(PROJECT_PATH, "data", WHICH_DATA_FOLDER), full.names = TRUE))
      
    which_folder <- rownames(all_folders)[which.max(all_folders$mtime)]
        
    filepaths <- list.files(which_folder, "\\.csv$", full.names = TRUE)

    if(DEBUG) message("GetCsvData: Import all files  \n")
            
    results <- do.call(data.table::rbindlist, lapply(filepath, function(path) {
         df <- data.table::fread(path)
         df[["source"]] <- rep(path, nrow(df))
         df}))
        
    FUNCTION_OUTPUT[["PROJECT_PATH"]] <- PROJECT_PATH
    FUNCTION_OUTPUT[["PROJECT_IN_PATH"]] <- any(grepl("Rproj", list.files()))
    FUNCTION_OUTPUT[["ALL_FOLDERS"]] <- all_folders
    FUNCTION_OUTPUT[["LATEST_FOLDER"]] <- which_folder
    FUNCTION_OUTPUT[["ALL_FILES"]] <- filepaths
    
    if(DEBUG) message("GetCsvData: Collecting final output  \n")

    FUNCTION_OUTPUT[["FULL_DATA"]] <- results

    if(!is.null(results) || !nrow(results) == 0){
        if(DEBUG) message("GetCsvData: Data acquisition completed  \n")
    
        return(FUNCTION_OUTPUT)
    } else {
        stop("GetCsvData: Function returned an empty data set  \n")
    }

}


###################################################################################################


#' @title GetSqlQuery
#'
#' @description This function will grab the conrents if a SQL file as a string
#'
#' @author Abraham Mathew
#' @family Data Import
#'
#' @param PROJECT_PATH  The main project directory
#' @param FILE_PATH The name of the SQL file in the queries sub-directory 
#' @param DEBUG If TRUE, the function will run in debug mode 
#' 
#' @return Returns the sql query as a srring
#' 
#' @export
#' 
GetSqlQuery <- function(PROJECT_PATH = rstudioapi::getActiveProject(),
                        FILE_NAME = "File.SQL",
                        DEBUG = TRUE){
 
    if(DEBUG) message("GetSqlQuery: Script Initialized  \n")
      
    FUNCTION_OUTPUT <- list()
  
    con = file(file.path(PROJECT_PATH, "queries", FILE_NAME), "r")
    
    sql_string <- ""
  
    if(DEBUG) message("GetSqlQuery: Import SQL query  \n")
    
    while(TRUE){
        line <- readLines(con, n = 1)
    
        if(length(line) == 0){
            break
        }
    
        line <- gsub("\\t", " ", line)
    
        if(grepl("--",line) == TRUE){
            line <- paste(sub("--","/*",line),"*/")
        }
    
        sql_string <- paste(sql_string, line)
    }
  
    FUNCTION_OUTPUT[["PROJECT_PATH"]] <- PROJECT_PATH
    FUNCTION_OUTPUT[["FILE_NAME"]] <- FILE_NAME
    FUNCTION_OUTPUT[["FULL_PATH"]] <- file.path(PROJECT_PATH, "queries", FILE_NAME)
    
    if(DEBUG) message("GetSqlQuery: Collecting final output  \n")
    
    FUNCTION_OUTPUT[["SQL_STRING"]] <- sql_string
  
    if(nchar(sql.string) >= 10){ 
        if(DEBUG) message("GetSqlQuery: SQL string acquisition completed  \n")
     
        return(FUNCTION_OUTPUT)
    } else {
        stop("GetSqlQuery: Empty SQL string returned  \n")    
    }

}



###################################################################################################


#' @title GetSqlData
#'
#' @description This function will grab data from a SQL database using a SQL file
#'
#' @author Abraham Mathew
#' @family Data Import
#'
#' @param DRIVER  Name of the SQL Platform
#' @param SERVER  Name of the server
#' @param DB_NAME  Name of the database
#' @param PROJECT_PATH  path to the project directory
#' @param DEBUG If TRUE, the function will run in debug mode 
#'
#' @return Returns the results of the sql query
#' 
#' @export
#' 
GetSqlData <- function(DRIVER = "SQL Server",
                         SERVER = "sdl02-vm161",
                         DB_NAME = "OpsDW",
                         PROJECT_PATH = NULL,
                         DEBUG = TRUE){
     
      if(DEBUG) message("GetSqlData: Function Initialized  \n")

      if(!is.null(PROJECT_PATH)){
          stop("GetSqlData: Project path is missing  \n")
      }
        
      start_time = Sys.time()
  
      FUNCTION_OUTPUT <- list()

      if(DEBUG) message("GetSqlData: Connect to database  \n")    
      
      con = dbConnect(odbc::odbc(),
                      Driver = DRIVER,
                      Server = SERVER,
                      Database = DB_NAME,
                      Trusted_Connection = "True")
      
      if(DEBUG) message("GetSqlData: Get SQL query  \n")    
      
      which_query <- GET_SQL_QUERY(PROJECT_PATH,
                                   FILE_NAME = "File.SQL",
                                   DEBUG = TRUE)
      
      if(DEBUG) message("GetSqlData: Pull data from database  \n")    
      
      temp <- dbSendQuery(con, which_query[["SQL_String"]])
          
      query_dat <- dbFetch(temp)

      elapsed_time <- Sys.time() - start_time
      
      FUNCTION_OUTPUT[["DRIVER"]] <- DRIVER
      FUNCTION_OUTPUT[["SERVER"]] <- SERVER
      FUNCTION_OUTPUT[["DB_NAME"]] <- DB_NAME 
      FUNCTION_OUTPUT[["QUERY_PATH"]] <- QUERY_PATH
      FUNCTION_OUTPUT[["QUERY_TEXT"]] <- which_query[["SQL_STRING"]]
      FUNCTION_OUTPUT[["ELAPSED_TIME"]] <- elapsed_time
      
      if(DEBUG) message("GetSqlData: Collecting final output  \n")
      
      FUNCTION_OUTPUT[["FULL_DATA"]] <- query_dat
    
      if(!is.null(query_dat) || !nrow(query_dat) == 0){
          if(DEBUG) message("GetSqlData: Data acquisition completed  \n")
    
          return(FUNCTION_OUTPUT)
      } else {
          stop("GetSqlData: Query returned an empty data set.  \n")
      }
          
      close(con)

} 



###################################################################################################


 

###################################################################################################



###################################################################################################



###################################################################################################





