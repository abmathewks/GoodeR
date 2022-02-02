### helper_functions.R



###################################################################################################


 
#' @title CreateProjectFiles
#'
#' @description Check a directory and create a project and folders if they don't exist
#'
#' @author Abraham Mathew
#' @family Project Preparation
#'
#' @param PROJECT_PATH The main project directory
#' @param FOLDERS_TO_CREATE The folders that will be created in the project path
#' @param OPEN_NEW_SESSION If TRUE, a new session will be generated
#' @param DEBUG If TRUE, the function will run in debug mode
#'
#' @return Creates a project and folders in a directory if a RStudio project is not present
#' 
#' @examples
#' 
#' \dontrun{
#' 
#'      GoodeR::CreateProjectFiles(PROJECT_PATH = rstudioapi::getActiveProject())
#' 
#' }
#' 
#' @export
#' 
CreateProjectFiles <- function(PROJECT_PATH = rstudioapi::getActiveProject(),
                               FOLDERS_TO_CREATE = c("data", "docs", "figs", "logs",
                                                     "output", "queries", "R", "tests"),
                               OPEN_NEW_SESSION = TRUE,
                               DEBUG = TRUE){

    if(DEBUG) message("CreateProjectFiles: Function initialized  \n")
  
    if(!dir.exists(PROJECT_PATH)){
        stop("CreateProjectFiles: Project path does not exist  \n")
    }
  
    if(!getwd() == PROJECT_PATH){
        stop("CreateProjectFiles: Project path argument is different from the current working directory  \n")
    }
  
    FUNCTION_OUTPUT <- list()
  
    FUNCTION_OUTPUT[["CURRENT_DIRECTORY"]] <- getwd()
    FUNCTION_OUTPUT[["PROJECT_PATH"]] <- PROJECT_PATH
    FUNCTION_OUTPUT[["DIRECTORY_EXISTS"]] <- dir.exists(PROJECT_PATH)
    FUNCTION_OUTPUT[["RPROJ_FILE_PRESENT"]] <- any(grepl("Rproj", list.files()))
    FUNCTION_OUTPUT[["FOLDERS_PRESENT_ORIG"]] <- all(FOLDERS_TO_CREATE %in% list.files(PROJECT_PATH))
  
    if(!any(grepl("Rproj", list.files()))){
  
        if(DEBUG) message("CreateProjectFiles: Project is not present  \n")
  
        usethis::create_project(PROJECT_PATH, open = OPEN_NEW_SESSION)
  
    }
  
    if(!all(FOLDERS_TO_CREATE %in% list.files(PROJECT_PATH))){
  
        if(DEBUG) message("CreateProjectFiles: Creating files  \n")
  
        for(each_folder in FOLDERS_TO_CREATE){
  
          dir.create(file.path(PROJECT_PATH, each_folder), showWarnings = FALSE)
  
          if(each_folder == "data"){
  
             dir.create(file.path(PROJECT_PATH, each_folder, "raw"), showWarnings = FALSE)
             dir.create(file.path(PROJECT_PATH, each_folder, "processed"), showWarnings = FALSE)
  
           }
       }
    } else {
  
        if(DEBUG) message("CreateProjectFiles: FOLDERS_TO_CREATE already present in the PROJECT_PATH  \n")
      
    }
  
    FUNCTION_OUTPUT[["FOLDERS_PRESENT_NEW"]] <- all(FOLDERS_TO_CREATE %in% list.files(PROJECT_PATH))
    FUNCTION_OUTPUT[["OPEN_NEW_SESSION"]] <- OPEN_NEW_SESSION
  
    if(all(FOLDERS_TO_CREATE %in% list.files(PROJECT_PATH))){
        if(DEBUG) message("CreateProjectFiles: Function run completed  \n")
  
        return(FUNCTION_OUTPUT)
    } else {
        stop("CreateProjectFiles: Folders not created  \n")
    }
}



###################################################################################################

 

#' @title PackageCheck
#'
#' @description Check if a vector of packages is installed. If they are not, install and load them
#'
#' @author Abraham Mathew
#' @family Project Preparation
#'
#' @param USE_THESE_PACKAGES A vector of strings which describe the packages that will be used during the project
#' @param DEBUG If TRUE, the function will run in debug mode
#'
#' @return Installs any missing packages and loads of them into the current workspace
#'
#' @examples
#' 
#' \donttest{
#'
#'      GoodeR::PackageCheck(c("ggplot2","data.table","lubridate","rms"))
#'
#' }
#' @export
#' 
PackageCheck <- function(USE_THESE_PACKAGES,
                         DEBUG = TRUE){
 
    if(DEBUG) message("PackageCheck: Function initialized  \n")

    if(any(USE_THESE_PACKAGES == "")){
         stop("PackageCheck: An empty string element was passed in USE_THESE_PACKAGES  \n")
    } 
    
    if(DEBUG) message("PackageCheck: Check for uninstalled packages  \n")

    FUNCTION_OUTPUT <- list()

    new_packages <- USE_THESE_PACKAGES[!(USE_THESE_PACKAGES %in% utils::installed.packages()[,"Package"])]

    if(length(new_packages)) {
          
        utils::install.packages(new_packages)

        if(DEBUG) message("PackageCheck: Importing all packages  \n")

        sapply(USE_THESE_PACKAGES[tolower(USE_THESE_PACKAGES) %in% tolower(.packages())], require, character.only = TRUE)

    } else {
      
        if(DEBUG) message("PackageCheck: Importing all packages  \n")

        sapply(USE_THESE_PACKAGES[tolower(USE_THESE_PACKAGES) %in% tolower(.packages())], require, character.only = TRUE)

    }

    if(DEBUG) message("PackageCheck: Collecting final output  \n")

    results <- data.table::data.table(
                     package_name = names(sapply(USE_THESE_PACKAGES, require, character.only = TRUE)),
                     loaded = sapply(USE_THESE_PACKAGES, require, character.only = TRUE))

    FUNCTION_OUTPUT[["USE_THESE_PACKAGES"]] <- USE_THESE_PACKAGES
    FUNCTION_OUTPUT[["NEW_PACKAGES_TO_INSTALL"]] <- new_packages
    FUNCTION_OUTPUT[["ALREADY_LOADED"]] <- USE_THESE_PACKAGES[tolower(USE_THESE_PACKAGES) %in% tolower((.packages()))]
    FUNCTION_OUTPUT[["FINAL_PACKAGE_STATUS"]] <- results

    if(!is.null(results) || !nrow(results) == 0){
        if(DEBUG) message("PackageCheck: Package installation completed  \n")

        return(FUNCTION_OUTPUT)
    } else {
        stop("PackageCheck: Function returned an empty data set  \n")
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
#'      GoodeR::RemoveSpecialChars("This is&%?? is a sting")
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

