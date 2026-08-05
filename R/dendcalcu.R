#' @title Calculate the cumulative growth or growth variation of dendrometer data.
#'
#' @description This function calculates the cumulative growth increment of all period(i.e., method = "cumul_p"), daily cumulative growth increment(i.e., method = "cumul_d") or growth variation between adjacent time periods(i.e., method = "increase").
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with dendrometer data for the same temporal resolution and time period.
#'
#' @param method string, "cumul_p" for annual cumulative growth increment, "cumul_d" for daily cumulative growth increment or 'increase' for growth changes between adjacent time periods. Default is "cumul_p".
#'
#' @return A dataframe containing time series and cumulative growth (or growth variation).
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(dendro)
#' 
#' #The default method is annual cumulative growth increment
#' dendro_cp <- dendcalcu(df=dendro)
#' head(dendro_cp)
#' 
#' #Daily cumulative growth increment
#' dendro_cd <- dendcalcu(df=dendro, method = "cumul_d")
#' head(dendro_cd)
#' 
#' #Growth variation between adjacent time periods
#' dendro_gv <- dendcalcu(df=dendro, method = "increase")
#' head(dendro_gv)
#' 
#' @export
dendcalcu <- function(df, method = "cumul_p"){
  if (method%in%c("cumul_p","cumul_d","increase")){
    m <- length(colnames(df))
    n <- length(rownames(df))
    g <- c()
    if (method == "cumul_p"){##Accumulated growth throughout the entire cycle
      for (a in c(2:m)){
        d <- df[1,a]
        f <- c()
        for (b in c(1:n)){
          c <- df[b,a]
          e <- c - d
          f <- c(f,e)
        }
        g <- cbind(g,f)
      }
    } else {
     if (method == "cumul_d"){##------Daily cumulative growth rate with a daily first zero
       df$D <-  format(df[[1]], format="%Y-%m-%d")
       df$H <-  format(df[[1]], format="%H:%M:%S")
       df0 <- df[1]
       for (a in c(2:m)){
         b <- df[c(1,a,(m+1),(m+2))]
         c <- unique(df$D)
         j <- c()
         for (d in c){
           e <- subset(b, b$D == d)
           e <- e[order(e[[1]]),]
           f <- length(rownames(e))
           k <- c(0)
           for (h in c(2:f)){
             i <- e[h,2]-e[1,2]
             k <- c(k,i)
           }
           k <- cbind(e[1],k)
           colnames(k) <- colnames(e)[c(1,2)]
           j <- rbind(j,k)
         }
         df0 <- merge(df0,j, by = colnames(df0)[1],all=T)
       }
       return(df0)
     } else {####----------The amount of change within two time intervals
       for (a in c(2:m)){
         f <- 0
         for (b in c(2:n)){
           c <- df[b,a]
           d <- df[b-1,a]
           e <- c - d
           f <- c(f,e)
         }
         g <- cbind(g,f)
       }
     }
    }
    g <- cbind(df[1],g)
    colnames(g) <- colnames(df)
    return(g)
  } else {
    stop("The 'method' must be either 'cumul_p','cumul_d' or 'increase'.")}
}
