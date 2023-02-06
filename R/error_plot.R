#' Error Plot for Univariate Comparisons 
#'
#' @description display univariate mean/SEM comparisons across groups
#' @param x string or factor variable of your experimental groups by sample
#' @param y numeric variable for y axis
#' @param color defaults to x; string or factor variable to color your data points
#' @param color.str default ""; a string to label the color legend
#' @param xlab,ylab default ""; axis labels 
#' @param title default ylab; sets title of the scores plot
#' @return A ggplot2 object of the error plot
#' @export
error_plot <- function(x,y,color=x,xlab="",ylab="",color.str="",title=ylab,
                       axis.text.x.size=20,axis.text.y.size=20,axis.text.x.angle=90,title.size=20,text.size=20){
  dataPlot <- data.frame(factor(x),y,factor(color))
  colnames(dataPlot)=c("x_dot","y_dot","colors_dot")
  plotOut <- ggplot(dataPlot,aes(x=x_dot,y=y_dot))+
    geom_jitter(size=4,aes(color=color))+
    stat_summary(geom="errorbar",color="black",size=1.5,aes(width=0.3),)+
    stat_summary(fun = "mean", geom="point", size=4,show.legend = F, color="black") +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid.major=element_blank(),
          axis.line=element_line(color="black"),
          text = element_text(size=text.size),
          legend.key = element_blank(),
          axis.text.x = element_text(color = "black",
                                     size = axis.text.x.size,angle= axis.text.x.angle),
          axis.text.y = element_text( color = "black",
                                      size = axis.text.y.size),
          plot.title=element_text(hjust=0.5,face = "bold",size=title.size))+
    xlab(xlab)+
    ylab(ylab)+
    labs(color = color.str) +
    ggtitle(paste(title))
  return(plotOut)
}



