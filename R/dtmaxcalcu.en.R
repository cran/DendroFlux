#' @title Determine the zero-flow conditions by environmental conditions
#'
#' @description This function determines zero-flow by environmental conditions.  \code{method} = "CV" means the dT of the zero-flow condition is stable dT, with a low coefficient of variation. 
#' \code{method} = "S" means the dT 0f the zero-flow condition is stable dT, with nighttime stability.
#'  
#' @details The environmental dependent method (ED) filter the dTmax by the daily predawn method (PD), using the environmental conditions(T was < 1 celsius degree or D was < 0.1–0.05 kPa for a period, default is 2 h) when plants let their sap flux nearly zero. 
#' A stable dT, with a low coefficient of variation (CV)(default CV < 0.005) within the same time range for stable nighttime environmental conditions(Peters et al., 2018) or with a low nighttime stability (S)(default S < 0.005) within the same time range for stable nighttime environmental conditions(Oish et al., 2008,2016). 
#' 
#' Nighttime stability is determined using the following function: 
#' 
#' S = N / V 
#' 
#' where S is stability, N is the standard deviation of nighttime delta-T values within the user-defined time range (this is the same time range for stable nighttime VPD conditions, default is 2-hours), and V is diurnal variability of delta-T for a 7-day moving window centered on a given night where 
#' 
#' V = mean (P(i) - L(i)) 
#' 
#' where P(i) is the peak value of delta-T for day i, defined by the upper quartile of nightime values L(i) is the low value of delta-T for day i, defined by the lower quartile of daytime values.
#'
#' @param df dataframe with sap flux data.
#' @param dc dataframe with Climate data.
#' @param method string, "CV" for the stable dT, with a low coefficient of variation (CV), "S" for A stable dT, with a low nighttime stability (S). Default is "CV".
#' @param predawn filter the time boundary for the dTmax.
#' @param hour time range for stable environmental conditions, default is 2h.
#' @param day filter the duration of the sliding time window for nighttime stability (S), default is 7 d.
#' @param threshold_cli the threshold of low environmental variables, default is 0.05.
#' @param threshold_sta the threshold of stable dT, default is 0.005.
#' @param fill logical, if TRUE it fills the NA values using spline interpolation. Default is FALSE.
#'  
#' @return A dataframe containing time series , dT series and corresponding dTmax series.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @importFrom stats quantile
#' @importFrom zoo na.spline
#' 
#' @references Oish et al.(2008). Estimating components of forest evapotranspiration: A footprint approach for scaling sap flux measurements. Agricultural and Forest Meteorology, 148, 1719-1732. 
#' 
#' Oish et al.(2016). Baseliner: An open-source, interactive tool for processing sap flux data from thermal dissipation probes. SoftwareX, 2016,5, 139-143.
#' 
#' Peters et al.(2018). Quantification of uncertainties in conifer sap flow measured with the thermal dissipation method. New Phytologist, 219(4),1283-1299.
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(sapflux)
#' data(climate)
#' #A stable dT, with a low coefficient of variation (CV)
#' sapflux_baseline <- dtmaxcalcu.en(df=sapflux,dc =climate)
#' head(sapflux_baseline)
#' 
#' #A stable dT, with a low nighttime stability (S)
#' sapflux_baseline <- dtmaxcalcu.en(df=sapflux,dc =climate, method = "S")
#' head(sapflux_baseline)
#'
#' @export
dtmaxcalcu.en <- function(df,dc = NULL,method = "CV", predawn = "06:00:00",hour=2,day =7,threshold_cli = 0.05, threshold_sta = 0.005, fill = FALSE){
  if (method %in% c("CV","S")) {
    if (is.null(dc)){
      dc <- df[1]
      dc[2] <- "Y"
    } else {
      a <- length(rownames(dc))
      reso <- as.numeric(difftime(dc[2,1],dc[1,1],units="mins"))
      n <- 60/reso
      n <- round((hour * n/2),2)
      ###---------------------------------------------------------------###dc
      for (b in c((n+1):(a-n))){
        c <- dc[c((b-n):(b+n)),2]
        ifelse(all(c < threshold_cli),dc[b,3]<-"Y",dc[b,3]<-"N")
      }
      dc[c(1:n,(a-n+1):a),3] <- "N"
      dc <- dc[-2]
      ###--------------------------------------------------------------###dc
    }
    ###----------------------------------------------------------------###Processing DF data
    a <- length(rownames(df))
    b <- length(colnames(df))
    reso <- as.numeric(difftime(df[2,1],df[1,1],units="mins"))
    n <- 60/reso
    df1 <- df[1]
   if (method == "CV"){####------------------------------cv computing method
     for (c in c(2:b)){###The data of a certain tree
       df0 <- df[,c(1,c)]
       cv <- c()
       for (d in c((n+1):(a-n))){
         e <- df0[c((d-n):(d+n)),2]
         cv0 <- sd(e)/mean(e)   #calculate CV
         cv <- c(cv,cv0)
         }
       cv <- c(rep(NA,n),cv,rep(NA,n))
       f <- paste(colnames(df0)[2],"_cv",sep = "")
       df0 <- cbind(df0,cv)
       colnames(df0) <- c(colnames(df0)[c(1,2)],f)
       df1 <- merge(df1,df0,by=colnames(df1)[1])
     }
   } else {###--------------------------------------------------------------------------Stable computing method
     day <- floor(day/2)
     for (c in c(2:b)){###The data of a certain tree
       df0 <- df[,c(1,c)]
       sd <- c()
       for (d in c((n+1):(a-n))){
         e <- df0[c((d-n):(d+n)),2]
         sd0 <- sd(e) #calculate sd
         sd <- c(sd,sd0)
       }
       sd <- c(rep(NA,n),sd,rep(NA,n))
       f <- paste(colnames(df0)[2],"_sd",sep = "")
       df0 <- cbind(df0,sd)
       colnames(df0) <- c(colnames(df0)[c(1,2)],f)
       ###--------------------------------Calculate the upper and lower quartiles of the 7-day sliding motion
       df0$D <-  format(df0[[1]], format="%Y-%m-%d")
       d <- length(unique(df0$D))
       v <- c()
       for (e in c((day+1):(d-day))){
         f <- unique(df0$D)[c((e-day):(e +day))]
         g <- subset(df0, df0$D == f)
         v0 <- c()
         for (h in f){
           g1 <- subset(g, g$D == h)
           pi <- as.numeric(quantile(g1[[2]],probs = 0.75))
           li <- as.numeric(quantile(g1[[2]],probs = 0.25))
           v1 <- pi - li
           v0 <- c(v0,v1)
         }
         v0 <- mean(v0) #The mean of the upper and lower quartiles of the sliding window
         v <- c(v,v0)
       }
       v <- c(rep(NA,day),v,rep(NA,day))
       g <- unique(df0$D)
       g <- cbind(g,v)
       f <- paste(colnames(df0)[2],"_v",sep = "")
       colnames(g) <- c("D",f)
       df0 <- merge(df0,g,by="D")
       df0[[5]] <- as.numeric(df0[[5]])
       df0$s <- df0[[4]]/df0[[5]]
       df0 <- df0[c(2,3,6)]
       f <- paste(colnames(df0)[2],"_s",sep = "")
       colnames(df0) <- c(colnames(df0)[c(1,2)],f)
       #########------stable is done
       df1 <- merge(df1,df0,by=colnames(df1)[1])
     }
   }
     ############--------------------merge dc and df1  
      df1 <- merge(dc,df1,by=colnames(df1)[1])
      a <- length(rownames(df1))
      b <- length(colnames(df1))
      df1$D <-  format(df1[[1]], format="%Y-%m-%d")
      df1$H <-  format(df1[[1]], format="%H:%M:%S")
      df1$dn[df1$H <= predawn] <- "n"
      df1$dn[df1$H > predawn] <- "d"
      c <- unique(df1$D)
      dfe <- df1[1]
      dfe_o <- df1[1]
      for (d in seq(3,b,2)){ #The data of a certain tree
        df0 <- df1[c(1,2,d,(d+1),(b+1),(b+3))]
        df02 <- c()
        for (e in c){
          df01 <- subset(df0,(df0[2] == "Y" &  df0[4] < threshold_sta & df0[5] == e& df0[6] =="n"))
          df01 <- df01[c(3,5)]
          df01 <-unique(subset(df01,df01[1] == max(df01[1])))
          df02 <- rbind(df02,df01)
        }
        f <- paste(colnames(df02)[1],"_max",sep = "")
        colnames(df02) <- c(f,"D")
        df01 <- unique(df0[5])
        df01 <- merge(df01,df02,by="D",all=T)
        df01_o <- df01
        if (length(rownames(df02)) >= 2){
          df01[2]<- zoo::na.spline(df01[[2]])
        }
        df02 <- df0[c(1,3,5)]
        df01<- merge(df02,df01,by="D",all=T)
        df01 <- df01[-1]
        df01_o <- merge(df02,df01_o,by="D",all=T)
        df01_o <- df01_o[-1]
        dfe <- merge(dfe,df01,by=colnames(dfe)[1],all=T)
        dfe_o <- merge(dfe_o,df01_o,by=colnames(dfe_o)[1],all=T)
      } 
      original <- dfe_o
      interpolated <- dfe
      ifelse(fill == FALSE, return(original), return(interpolated))
      
  } else {
    stop("The 'method' must be either 'CV' or 'S'.")}
}
