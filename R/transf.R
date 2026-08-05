#' @title Conversion between Date and days of year
#'
#' @description This function is to convert Date and days of year into each other.
#'
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with dendrometer data for the same temporal resolution and time period.
#'
#' @param method string, "days" for Convert Date to days of year, "date" for Convert days of year to Date.
#' 
#' @param year To convert days of year to Date, the corresponding year needs to be input.
#' 
#' @return A dataframe containing the data including Date and days of year.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(dendro)
#' 
#' #The default method is Convert Date to days of year
#' dendro_days <- transf(df = dendro, method = "days")
#' head(dendro_days)
#' 
#' #Convert days of year to Date
#' dendro_days_date <- transf(df = dendro_days, method = "date", year =2025)
#' head(dendro_days_date)
#' 
#' @export
transf <- function(df,method = "days", year = NULL){
  if (method%in%c("days","date")){
    m <- length(colnames(df))
    n <- length(rownames(df))
    g <- c()
    if (method == "days"){ #Convert date to annual date
      a <- unique(format(df[[1]], format="%Y"))
      if (length(a) != 1){
        stop("Date conversion can only be done annually (1 year). Check it.")
      } else {
        a <- as.numeric(a)
        g <- c()
        if (is.integer(a / 4 )){
          #leap years
          b <- c(31,29,31,30,31,30,31,31,30,31,30,31)
          for (c in c(1:12)){ #month
            d <- b[c]
            for (e in c(1:d)){ #day
              f <- paste(a,c,e, sep = "-")
              g <- c(g,f)
            }
          }
          h <- c(1:366)
        } else {
          b <- c(31,28,31,30,31,30,31,31,30,31,30,31)
          for (c in c(1:12)){ #month
            d <- b[c]
            for (e in c(1:d)){ #day
              f <- paste(a,c,e, sep = "-")
              g <- c(g,f)
            }
          }
          h <- c(1:365)
        }
        g <- data.frame(cbind(g,h))
        colnames(g) <- c("i","Days")
        g$i <- as.Date(g$i)
        df$i <- as.Date(df[[1]])
        df <- merge(g,df,by= "i")
        df <- df[-1]
        return(df)
      }
    } else {#Convert days of year to dates
      year <- as.numeric(year)
      year <- round(year)
        if (year > 0){
          if (is.integer(year / 4 )){
            #leap years
            b <- c(31,29,31,30,31,30,31,31,30,31,30,31)
            for (c in c(1:12)){ #month
              d <- b[c]
              for (e in c(1:d)){ #day
                f <- paste(a,c,e, sep = "-")
                g <- c(g,f)
              }
            }
            h <- c(1:366)
          } else {
            b <- c(31,28,31,30,31,30,31,31,30,31,30,31)
            for (c in c(1:12)){ #month
              d <- b[c]
              for (e in c(1:d)){ #day
                f <- paste(year,c,e, sep = "-")
                g <- c(g,f)
              }
            }
            h <- c(1:365)
          } 
          g <- data.frame(cbind(g,h))
          colnames(g) <- c("Date","i")
          g$Date <- as.Date(g$Date)
          df$i <- df[[1]]
          df <- merge(g,df,by= "i")
          df <- df[-1]
          return(df)
        } else {
          stop("Please input the correct year.")}
    }
  } else {
    stop("The method must be either 'days' or 'date'.")}
}
