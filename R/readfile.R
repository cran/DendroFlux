#' @title Reading dendrometer or sap flux data.
#'
#' @description This function reads dendrometer or sap flux data from .csv or .xlsx files.
#' 
#' @param file string file name or path of the file.
#' 
#' @param sheet the name or index of the sheet to read .xlsx data from.
#' 
#' @param sep string the separator of the files. Only if they are different than the standard separators such as comma for .csv file.
#' 
#' @param dec the character used in the file for decimal points.
#'
#' @return A dataframe with the dendrometer or sap flux data:
#' 
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#' @importFrom utils tail
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @export
readfile <- function(file, sheet=NULL, sep=NULL, dec=NULL){
  file_name <- basename(file)
  suffix <- tail(strsplit(file_name, split = '\\.')[[1]], 1)
  
  if (suffix %in% c("xlsx","csv")){ #read xlsx
    if (suffix == "xlsx"){
      data <- data.frame(readxl::read_excel(file, sheet))  
      return(data)
    } else{ #read csv
      data <- read.csv(file, header=TRUE, sep = ifelse(is.null(sep),',',sep), 
                       dec = ifelse(is.null(dec),'.',dec))
      data[[1]] <- as.POSIXct(data[[1]])
      return(data)
    }
  } else{
    stop("The file extension should be either '.xlsx' or '.csv'.")
  }
}
