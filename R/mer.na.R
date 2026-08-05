#' @title Merge the missing data with the original data
#'
#' @description This function merges time series gap(s) and \code{NA} values from other series - as indentified by \link{check.na} - into the original data.
#' 
#' @param df dataframe with the original data
#'
#' @param df.na dataframe with time series gap(s) and \code{NA} values from \link{check.na}
#'
#' @return A dataframe containing the data including original data and missing data.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(dendro_na)
#' dendro_na_if <-check.na(df=dendro_na, resolution=10)
#' dendro_na_merge <- mer.na(dendro_na,dendro_na_if)
#' head(dendro_na_merge)
#' 
#' @export
mer.na <- function(df,df.na){
  a <- unique(rbind(df,df.na))
  a <- a[order(a[[1]]),]
  rownames(a) <- c(1:length(rownames(a)))
  return(a)
}
