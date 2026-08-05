#' @title Daily water consumption from sap flux
#'
#' @description This function roughly calculate the daily water consumption per square centimeter of water-conducting sapwood area, in cubic meters.
#' 
#' @details The daily water consumption of a tree is the product of the area under the daily sap flow curve and the water-conducting sapwood area. The more meticulous the \code{resolution}, the higher the accuracy.
#'
#' @param df dataframe containing date and sap flux density.
#' @param resolution integer, indicating the resolution of data in \strong{minutes}.
#' 
#' @return A dataframe containing time series and the daily water consumption per square centimeter of water-conducting sapwood area (Unit: cubic meter per day).
#' 
#' @importFrom stats na.omit
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(sapflux)
#' sapflux_baseline <- dtmaxcalcu(df=sapflux)
#' sapflux_fd <- sfcalcu(df=sapflux_baseline)
#' waterconsume_d <- dwaterconsu(sapflux_fd, resolution = 10)
#' head(waterconsume_d)
#'
#' @export
dwaterconsu <- function(df, resolution){
  a <- length(colnames(df))
  df$D <-  format(df[[1]], format="%Y-%m-%d")
  df2 <- data.frame(unique(df$D))
  colnames(df2) <- "D"
  for (b in c(2:a)){
    df0 <- df[c(1,b,(a+1))]
    df0[2][df0[2]==0] <- NA
    df0 <- na.omit(df0)
    c <- unique(df0$D)
    f <- data.frame()
    for (d in c){
      df1 <- subset(df0, df0$D == d)
      sf0 <- (sum(df1[2]) * (length(rownames(df1)) + 1) * resolution * 60)/1000000
      e <- c(d,sf0)
      f <- rbind(f,e)
    }
    g <- colnames(df)[c((a+1),b)]
    colnames(f) <- g
    df2 <- merge(df2,f,by="D")
  }
  return(df2)
}
