#' @title Simple plotting about dendrometer or sap flux data
#'
#' @description This function draws a simple graph about dendrometer or sap flux data.
#' 
#' @param df dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with data for the same temporal resolution and time period.
#'
#' @param facet logical, if \code{TRUE} draws independent subgraphs, if \code{FALSE} draws an integrated graph. Default is \code{FALSE}.
#' 
#' @return Simple graph(s).
#'  
#' @importFrom ggplot2 ggplot aes geom_point labs scale_x_datetime facet_wrap scale_x_continuous scale_y_continuous
#' @importFrom rlang .data
#' @author Shuo Wen <shuowen@caf.ac.cn>
#' @examples
#' ##Load data
#' data(sapflux)
#' 
#' #Default is an integrated graph.
#' plot_simple(df=sapflux)
#' 
#' #Drawing independent subgraphs.
#' plot_simple(df=sapflux,facet = TRUE )
#' 
#' @export
  
##################-----------------------------------------  
plot_simple <- function(df,facet =FALSE){
  a <- colnames(df)
  b <- length(df)
  e <- c()
  f <- length(rownames(df))
  for (c in c(2:b)){
    d <- df[c(1,c)]
    d$class <-  a[c]
    colnames(d) <- c("Time","Y","Class")
    e <- rbind(e,d)  }
  if (facet == TRUE){ ##---Separate subgraphs
    g_plot <- ggplot(e,aes(x = .data$Time,y = .data$Y,colour = .data$Class)) + 
      geom_point() + facet_wrap(.~Class, nrow=(b-1)) + 
      scale_x_datetime(
        date_breaks = "1 week",
        date_labels = "%m/%d"
      ) +
      scale_y_continuous(breaks = pretty(e$Y, n = 6))
    } else { ##---one graph
      g_plot <-  ggplot(e,aes(x = .data$Time,y = .data$Y,colour = .data$Class)) + 
        scale_x_datetime(
          date_breaks = "1 week",
          date_labels = "%m/%d"
        ) +
        geom_point() + scale_y_continuous(breaks = pretty(e$Y, n = 6))}
  return(g_plot)
}
