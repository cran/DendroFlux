#' @title Interpolation of missing values in dendrometer/sap flux data.
#'
#' @description This function interpolates \code{NA} values based on existing data. The \code{NA} values can be replaced by spline interpolation using \code{\link[zoo:na.approx]{na.spline}} of the package \pkg{zoo} or seasonal interpolation considering the seasonality of the daily pattern using \code{\link[forecast:na.interp]{na.interp}} of the package \pkg{forecast}.
#' 
#' @param df dataframe from \link{mer.na}
#'
#' @param method string, 'spline' for the spline interpolation or 'seasonal' for the seasonal interpolation.
#'
#' @return A dataframe containing the data including gaps filled with interpolated values.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(dendro_na)
#' dendro_na_if <-check.na(df=dendro_na, resolution=10)
#' dendro_na_merge <- mer.na(dendro_na,dendro_na_if)
#' 
#' #The default method is spline interpolation
#' dendro_fill_spline <- fill.na(dendro_na_merge)
#' head(dendro_fill_spline)
#' 
#' #The seasonal interpolation
#' dendro_fill_season <- fill.na(dendro_na_merge, method = "seasonal")
#' head(dendro_fill_season)
#'
#' @importFrom zoo na.spline
#' @importFrom forecast na.interp
#'
#' @export
fill.na <- function(df,method = "spline"){
  if (method%in%c("spline","seasonal")){
    message("WARNING: If a gap in dataset lasting for more than 24 hours, the filling may not be reliable.")
    n <- length(colnames(df))
    c <- c()
    if (method == "spline"){
      for (a in c(2:n)){
        b <- zoo::na.spline(df[[a]])   
        c <- cbind(c,b)
      }
    } else {
        for (a in c(2:n)){
        b <- forecast::na.interp(df[a])   
        c <- cbind(c,b)
      }
    }
    df1 <- cbind(df[1],c)
    colnames(df1) <- colnames(df)
    return(df1)
  } else {
    stop("The method must be either 'spline' or 'seasonal'.")}
}
