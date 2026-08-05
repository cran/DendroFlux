#' @title Resampling temporal resolution of dendrometer/sap flux data
#'
#' @description This function is designed to change the temporal resolution of data. Depending on the objective, the user can define either maximum, minimum, or mean values to resample data in hourly, daily, weekly , monthly or yearly frequency.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS}.
#'
#' @param by either \emph{H, D, W,M} or \emph{Y} to resample data into hourly, daily, weekly, monthly or yearly resolution.
#'
#' @param value either \emph{max, min}, \emph{mean} or \emph{sum} for the resampling value.
#'
#' @return Dataframe with resampled data.
#'
#' @examples
#' ##Load data
#' data(sapflux)
#' # To resample hourly with mean value
#' resample_h_m <- resample(df = sapflux, by='H', value='mean')
#' head(resample_h_m)
#'
#' @export
resample <- function(df, by, value){
  z <- length(colnames(df))
  if (by %in% c("H","D","W","M","Y")){#A
    if (by %in% c("H","D")){ 
      if (by == "H"){###################################################################################--0
        df$D <- format(df[[1]], format="%Y-%m-%d")
        df$H <- format(df[[1]], format="%H")#---------------------------hourly
        a <- unique(df$D)
        b <- unique(df$H)
        i <- unique(df[c((z+1),(z+2))])
        for (y in c(2:z)){
          df0 <- df[c(1,y,(z+1),(z+2))]
          h <- data.frame()  
          for (c in a){###----------------------------------------------------------------------------------1
            for (d in b){ #hour
              e <- subset(df0, df0$D == c & df0$H == d)
              #########------------------------------------------------------------------------------2
              if (value %in% c("max", "min", "mean", "sum")){
                if (value %in% c("max", "min", "mean", "sum")){
                  if (value == "max"){
                    f <- max(e[2])
                  } else { #"min"
                    f <- min(e[2])
                  }
                } else {
                  if (value == "mean"){
                    f <- mean(e[2])
                  } else { #"sum"
                    f <- sum(e[2])
                  }
                }
              } else {stop("The 'value' must be either 'max','min','mean' or 'sum'.")}
              #########----------------------------------------------------------------------------------2
            g <- c(c,d,f)
            h <- rbind(h,g)
            }
          }###----------------------------------------------------------------------------------------------1
        colnames(h) <- c("D","H",colnames(e)[2])
        i <- merge(i,h, by =c("D","H"))
        }
        a <- paste(i[[1]],i[[2]],sep = " ")
        i[1] <- as.POSIXct(a,tryFormats = "%Y-%m-%d %H", tz = "UTC")
        i <- i[-2]
        return(i)
        } else {######################################################################################----0
          df$D <- format(df[[1]], format="%Y-%m-%d")#---------------daily
          i <- unique(df[(z+1)])
          for (y in c(2:z)){
            df0 <- df[c(1,y,(z+1))]
            f <- data.frame()
            a <- unique(df$D)
            for (b in a){
              c <- subset(df0, df0$D == b)
              #########------------------------------------------------------------------------------2
              if (value %in% c("max", "min", "mean", "sum")){
                if (value %in% c("max", "min", "mean", "sum")){
                  if (value == "max"){
                    d <- max(c[2])
                  } else { #"min"
                    d <- min(c[2])
                  }
                } else {
                  if (value == "mean"){
                    d <- mean(c[2])
                  } else { #"sum"
                    d <- sum(c[2])
                  }
                }
              } else {stop("The 'value' must be either 'max','min','mean' or 'sum'.")}
              #########----------------------------------------------------------------------------------2
              e <- c(b,d)
              f <- rbind(f,e)}
            colnames(f) <- c("D",colnames(c)[2])
            i <- merge(i,f, by ="D")}
          i[1] <- as.Date(i[[1]])
          return(i)}
      } else {#------------------------------------------------------------abcd
        if (by == "W"){
          df$W <- format(df[[1]], format="%W")#weekly
          i <- unique(df[(z+1)])
          for (y in c(2:z)){
            df0 <- df[c(1,y,(z+1))]
            f <- data.frame()
            a <- unique(df$W)
            for (b in a){
              c <- subset(df0, df0$W == b)
              #########------------------------------------------------------------------------------2
              if (value %in% c("max", "min", "mean", "sum")){
                if (value %in% c("max", "min", "mean", "sum")){
                  if (value == "max"){
                    d <- max(c[2])
                  } else { #"min"
                    d <- min(c[2])
                  }
                } else {
                  if (value == "mean"){
                    d <- mean(c[2])
                  } else { #"sum"
                    d <- sum(c[2])
                  }
                }
              } else {stop("The 'value' must be either 'max','min','mean' or 'sum'.")}
              #########----------------------------------------------------------------------------------2
              e <- c(b,d)
              f <- rbind(f,e)}
            colnames(f) <- c("W",colnames(c)[2])
            i <- merge(i,f, by ="W")}
          return(i)
        } else {
          if (by == "M"){
            df$M <- format(df[[1]], format="%Y-%m")#monthly
            i <- unique(df[(z+1)])
            for (y in c(2:z)){
              df0 <- df[c(1,y,(z+1))]
              f <- data.frame()
              a <- unique(df$M)
              for (b in a){
                c <- subset(df0, df0$M == b)
                #########------------------------------------------------------------------------------2
                if (value %in% c("max", "min", "mean", "sum")){
                  if (value %in% c("max", "min", "mean", "sum")){
                    if (value == "max"){
                      d <- max(c[2])
                    } else { #"min"
                      d <- min(c[2])
                    }
                  } else {
                    if (value == "mean"){
                      d <- mean(c[2])
                    } else { #"sum"
                      d <- sum(c[2])
                    }
                  }
                } else {stop("The 'value' must be either 'max','min','mean' or 'sum'.")}
                #########----------------------------------------------------------------------------------2
                e <- c(b,d)
                f <- rbind(f,e)}
              colnames(f) <- c("M",colnames(c)[2])
              i <- merge(i,f, by ="M")}
            return(i)
          } else {
            df$Y <- format(df[[1]], format="%Y")#yearly 
            i <- unique(df[(z+1)])
            for (y in c(2:z)){
              df0 <- df[c(1,y,(z+1))]
              f <- data.frame()
              a <- unique(df$Y)
              for (b in a){
                c <- subset(df0, df0$Y == b)
                #########------------------------------------------------------------------------------2
                if (value %in% c("max", "min", "mean", "sum")){
                  if (value %in% c("max", "min", "mean", "sum")){
                    if (value == "max"){
                      d <- max(c[2])
                    } else { #"min"
                      d <- min(c[2])
                    }
                  } else {
                    if (value == "mean"){
                      d <- mean(c[2])
                    } else { #"sum"
                      d <- sum(c[2])
                    }
                  }
                } else {stop("The 'value' must be either 'max','min','mean' or 'sum'.")}
                #########----------------------------------------------------------------------------------2
                e <- c(b,d)
                f <- rbind(f,e)}
              colnames(f) <- c("Y",colnames(c)[2])
              i <- merge(i,f, by ="Y")}
            return(i)
          }
        }
        }} else {stop("The 'by' must be either 'H','D','W','M' or 'Y'.")}#A
}
