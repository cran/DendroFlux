#' @title Detection of missing values in dendrometer/sap flux data.
#'
#' @description This function detects gap(s) in a time series, inserts missing rows at the given temporal resolution corresponding to the data, assigns \code{NA} values to the missing values, and compiles the gap and \code{NA} information from multiple series into a single data frame.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with data for the same temporal resolution and time period.
#'
#' @param resolution integer, indicating the resolution of data in \strong{minutes}.
#'
#' @return A dataframe containing time series gap(s) and \code{NA} values from other series.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(dendro_na)
#' dendro_na_if <- check.na(df=dendro_na, resolution=10)
#' head(dendro_na_if)
#'
#' @export
check.na <- function(df,resolution){
  ######Is the time series missing
  n <- as.numeric(length(rownames(df)))
  d <- c()
  gap.time <- c()
  for (a in c(2:n)){
    b <- a - 1
    c <- difftime(df[a, 1], df[b, 1], units = "mins")
    if (c != resolution){
      d <- c / resolution -1 
      for (e in c(1:d)){
        g <- df[b,1] + e * resolution *60
        gap.time <- c(gap.time,g)
      }
    }
  }
  matnull <- matrix(NA, nrow = length(gap.time), ncol = length(colnames(df))-1)
  gap.time <- data.frame(cbind(gap.time,matnull))
  colnames(gap.time) <- colnames(df)
  gap.time[,1] <- as.POSIXct(gap.time[[1]])
  #######Is there any missing data
  chn <- colSums(is.na(df))[-1]
  if (all(chn != 0)){
    h <- as.numeric(length(colnames(df)))
    gap.data <- data.frame()
    for (i in c(2:h)){
      j <- subset(df, is.na(df[,i]) == TRUE)
      gap.data <- rbind(gap.data,j)
    }
  } else {
    gap.data <- data.frame()
  }
  #######Organize missing time and data together
  gap <- unique(rbind(gap.data,gap.time))
  if(length(rownames(gap)) >= 1){
    gap <- gap[order(gap[[1]]),]
    rownames(gap) <- c(1:length(rownames(gap)))
    message(paste("The number of time series gap(s) and NA value(s) is:", length(rownames(gap)), "row(s).", sep = " "))
    message(gap)
    return(gap)
  } else {
    message("There is no time series gap and NA value.")
  }
}
