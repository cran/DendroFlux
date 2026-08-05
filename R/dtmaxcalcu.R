#' @title Determine the zero-flow conditions
#' @description This function determines zero-flow by environmental conditions.  \code{method} = "MN" means the dT of the zero-flow condition is the dT of midnight. \code{method} = "SP" means the dT of the zero-flow condition is the maximum dT within a 24-hour period that begins at daybreak. \code{method} = "PD" means the dT of the zero-flow condition is the maximum dT between midnight and morning.
#' 
#' @details The midnight method (MN) defines the dTmax is the dT of midnight. 
#' 
#' The successive predawn method (SP) defines dTmax as the maximum dT of the day that starts at predawn within a 24-hour period.This method has the advantage of being able to calculate dTmax quickly while minimizing the effect of nocturnal transpiration on dTmax estimation. See more in details in Peters et al.(2018). 
#' 
#' The daily predawn method (PD) defines dTmax as the maximum dT between midnight and the morning. See more in details in Peters et al.(2018). 
#' 
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with sap flux data for the same temporal resolution and time period.
#' @param method string, "MN" for the dTmax is the dT of midnight. "SP" for dTmax as the maximum dT of the day that starts at predawn within a 24-hour period. "PD" for dTmax as the maximum dT between midnight and the morning. Default is "MN".
#' @param predawn filter the time boundary for the dTmax.
#' 
#' @return A dataframe containing time series , dT series and corresponding dTmax series.
#' 
#' @references Peters et al.(2018). Quantification of uncertainties in conifer sap flow measured with the thermal dissipation method. New Phytologist, 219(4),1283-1299.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(sapflux)
#' 
#' #The midnight method (MN)
#' sapflux_baseline <- dtmaxcalcu(df=sapflux)
#' head(sapflux_baseline)
#' 
#' #The successive predawn method (SP)
#' sapflux_baseline <- dtmaxcalcu(df=sapflux, method = "SP")
#' head(sapflux_baseline)
#'
#' #The daily predawn method (PD)
#' sapflux_baseline <- dtmaxcalcu(df=sapflux, method = "PD")
#' head(sapflux_baseline)
#' 
#' @export
dtmaxcalcu <- function(df,method = "MN", predawn = "06:00:00"){
  if (method %in% c("MN","SP","PD")){
    #通用的处理
    a <- length(colnames(df))
    df$D <-  format(df[[1]], format="%Y-%m-%d")
    df$H <-  format(df[[1]], format="%H:%M:%S")
    df2 <- df[1]
    e <- colnames(df)[1]
    if (method == "MN"){# dTmax is midnight 12：00
      for (b in c(2:a)){
        df1 <- df[,c(1,b,a+1,a+2)]
        tmax <- subset(df1, df1$H == "00:00:00")
        c <- colnames(df)[b]
        d <- paste(c,"dtmax",sep = "")
        tmax <- tmax[c(-1,-4)]
        colnames(tmax) <- c(d,"D") #naming dTmax
        df1 <- df1[-4]
        df1 <- merge(df1,tmax, by = "D",no.dups = FALSE)
        df1 <- df1[-1]
        df2 <- merge(df2,df1,by = e)
      }
    } else{
      df$dn[df$H <= predawn] <- "n"
      df$dn[df$H > predawn] <- "d"
      D <- unique(df$D)
      if (method == "SP"){
        dd <- length(D)
        df0 <- data.frame() 
        for (f in c(1:dd)){
          g <- D[f]
          tf <- subset(df,df$D == g)
          tf$tf[tf$H < predawn] <- f-1 
          tf$tf[tf$H >= predawn] <- f
          df0 <- rbind(df0,tf)
        }
        for (b in c(2:a)){
          df1 <- df0[,c(1,b,a+1,a+2,a+3,a+4)]
          tmax1 <- data.frame()
          for (f in c(1:dd)){
            tmax0 <- subset(df1, df1$tf == f)
            tmax0 <- subset(tmax0, tmax0[[2]] == max(tmax0[[2]]))
            tmax0 <- unique(tmax0[,c(2,6)])
            tmax1 <- rbind(tmax1,tmax0)
          }
          i0 <- c(tmax1[1,1],0)
          tmax1 <- rbind(i0,tmax1)
          c <- colnames(df1)[2]
          d <- paste(c,"dtmax",sep = "")
          colnames(tmax1) <- c(d,"tf") #naming dTmax
          df1 <- df1[,-c(3:5)]
          df1 <- merge(df1,tmax1, by = "tf",no.dups = FALSE)
          df1 <- df1[-1]
          df2 <- merge(df2,df1,by = e)
        }
      } else{#PD#
        
        for (b in c(2:a)){
          df1 <- df[,c(1,b,a+1,a+2,a+3)]
          tmax0 <- subset(df1, df1$dn == "n")
          tmax2 <- data.frame()
          for (f in D){
            tmax1 <- subset(tmax0, tmax0$D == f)
            tmax1 <- subset(tmax1, tmax1[[2]] == max(tmax1[[2]]))
            tmax1 <- unique(tmax1[,c(2,3)])
            tmax2 <- rbind(tmax2,tmax1)
          }
          c <- colnames(df)[b]
          d <- paste(c,"dtmax",sep = "")
          colnames(tmax2) <- c(d,"D") #naming dTmax
          df1 <- df1[,-c(4,5)]
          df1 <- merge(df1,tmax2, by = "D",no.dups = FALSE)
          df1 <- df1[-1]
          df2 <- merge(df2,df1,by = e)
        } 
      }
    }
    return(df2)
  } else{
    stop("The 'method' must be either 'MN', 'SP' or 'PD'.")}
}
