#' @title Determine the zero-flow conditions by moving window
#' 
#' @description This function determines zero-flow by environmental conditions.  \code{method} = "MAX" means the dT of the zero-flow condition is the maximum night dT, with a moving window. 
#' \code{method} = "MEAN" means the dT of the zero-flow condition is the moving window mean recalculated after omitting values lower than the mean.
#' 
#' @details 
#' The moving window method (MAX) defines dTmax as the maximum daily dTmax with a moving window (default is an eleven-day length). See more in details in Peters et al.(2018). 
#' 
#' The double regression method (MEAN) defines dTmax as the moving window (default is an eleven-day length) mean recalculated after omitting values lower than the mean. See more in details in Peters et al.(2018).
#' 
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with sap flux data for the same temporal resolution and time period.
#' @param method string, "MAX" for dTmax as the maximum daily dTmax with a moving window (default is an eleven-day length). "MEAN" for dTmax as the moving window (default is an eleven-day length) mean recalculated after omitting values lower than the mean. Default is "MAX".
#' @param win.size Size of moving window, default is 11.
#' @param fill logical, if TRUE it fills the NA values using spline interpolation. Default is FALSE.
#' 
#' @return A dataframe containing time series , dT series and corresponding dTmax series.
#' @importFrom zoo na.spline
#' 
#' @references Peters et al.(2018). Quantification of uncertainties in conifer sap flow measured with the thermal dissipation method. New Phytologist, 219(4),1283-1299.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(sapflux)
#' sapflux_baseline <- dtmaxcalcu(df=sapflux,method= "PD")
#' 
#' #The moving window method (MAX)
#' sapflux_bl_moving <- dtmaxcalcu.moving(df=sapflux_baseline, method = "MAX")
#' head(sapflux_bl_moving)
#' 
#' #The double regression method (MEAN)
#' sapflux_bl_moving <- dtmaxcalcu.moving(df=sapflux_baseline, method = "MEAN")
#' head(sapflux_bl_moving)
#' 
#' @export
dtmaxcalcu.moving <- function(df,method = "MAX", win.size = 11, fill = FALSE){
  if (method %in% c("MAX","MEAN")){
    a <- length(colnames(df))
    df$D <-  format(df[[1]], format="%Y-%m-%d") 
    df0 <- unique(df[, c(seq(3,a,2),a+1)])
    b <- length(rownames(df0))
    f <- length(colnames(df0))
    dfr <- df[,c(1,a+1)]
    dfr_o <- df[,c(1,a+1)]
    if (b > win.size){
      if(is.integer(win.size / 2)){
        d <- win.size/2    #Half of the even d time window
        for (g in c(1:(f-1))){#A certain column
          h <- df0[,c(g,f)]
          dff1 <- data.frame()
          if (method == "MAX"){ #The maximum value of swiping the window
            for (e in c((d+1):(b-d+1))){
              h1 <- max(h[c((e-d):(e+d-1)),1])
              t0 <- h[e,2]
              dff0 <- c(h1,t0)
              dff1 <- rbind(dff1,dff0)
            }
          } else {#The mean after swiping the window's mean value
            for (e in c((d+1):(b-d+1))){
              h1 <- h[c((e-d):(e+d-1)),]
              h2 <- mean(h[c((e-d):(e+d-1)),1])#Sliding window mean
              h1 <- subset(h1,h1[1]>= h2) #Filter the parts greater than or equal to the mean
              h2 <- mean(h1[,1])#Maximum temperature difference on another day
              t0 <- h[e,2]
              dff0 <- c(h2,t0)
              dff1 <- rbind(dff1,dff0)#----------------Interpolation afterwards
            }
          }
          colnames(dff1) <- colnames(h)
          dff1 <- merge(dff1,df0[f], by="D",all =T)
          dff1_o <- dff1
          dff1[2]<- zoo::na.spline(dff1[[2]])
          dfr <- merge(dfr,dff1,by ="D",)##ronghe--------------------
          dfr_o <- merge(dfr_o,dff1_o,by ="D",)
        }########1
      } else {
        d <- floor(win.size/2) #odd number
        for (g in c(1:(f-1))){#A certain column#####2---------------
          h <- df0[,c(g,f)]
          dff1 <- data.frame()
          if (method == "MAX"){ #The maximum value of swiping the window
            for (e in c((d+1):(b-d))){
              h1 <- max(h[c((e-d):(e+d)),1])
              t0 <- h[e,2]
              dff0 <- c(h1,t0)
              dff1 <- rbind(dff1,dff0)
            }
          } else {#The mean after swiping the window's mean value
            for (e in c((d+1):(b-d))){
              h1 <- h[c((e-d):(e+d)),]
              h2 <- mean(h[c((e-d):(e+d)),1])#Sliding window mean
              h1 <- subset(h1,h1[1]>= h2) #Filter the parts greater than or equal to the mean
              h2 <- mean(h1[,1])#Maximum temperature difference on another day
              t0 <- h[e,2]
              dff0 <- c(h2,t0)
              dff1 <- rbind(dff1,dff0)#----------------Interpolation afterwards
            }
          }
          colnames(dff1) <- colnames(h)
          dff1 <- merge(dff1,df0[f], by="D",all =T)
          dff1_o <- dff1
          dff1[2]<- zoo::na.spline(dff1[[2]])
          dfr <- merge(dfr,dff1,by ="D",) ##ronghe--------------------
          dfr_o <- merge(dfr_o,dff1_o,by ="D",)
        }###2--------------------------------------
      }
      dfr <- dfr[-1]
      dfr_o <- dfr_o[-1]
      df_o <- df
      df[c(colnames(df[seq(3,a,2)]))] <- dfr[c(colnames(dfr[c(2:(length(colnames(dfr))))]))]
      df <- df[-(a+1)]
      df_o[c(colnames(df_o[seq(3,a,2)]))] <- dfr_o[c(colnames(dfr_o[c(2:(length(colnames(dfr_o))))]))]
      df_o <- df_o[-(a+1)]
      original <- df_o
      interpolated <- df
      ifelse(fill == FALSE, return(original), return(interpolated))
      
    } else {
      c <- paste("The 'win. size' time window is too large, please be less than:",b-1,".",sep = " ")
      stop(c)}
  } else {
    stop("The 'method' must be either 'MAX' or 'MEAN'.")}
}
