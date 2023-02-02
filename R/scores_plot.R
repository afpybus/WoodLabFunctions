#' Make a Scores Plot
#'
#' @description Used to create scores plot colored by your categorical variable of interest.
#' @param T1,T2 Sample scores in your two components (e.g.: PC1 and PC2)
#' @param color A character or factor array of your experimental groups by sample
#' @param color.str Text to label the color legend
#' @param analysis Set to "PCA" to change axis labels to PC instead of LV; defaults to "PLSDA" with LV axes; sets default plot title
#' @param plot.title Set title of the scores plot; if not given, defaults to analysis
#' @return A ggplot2 object of the scores plot
#' @export
scores_plot <- function(T1,T2,color,color.str="",analysis="PLSDA",plot.title=NULL){
  if(is.null(plot.title)){plot.title=analysis}
  if(analysis=="PCA"){axis.label="PC"}else{axis.label="LV"}
  ScoresPlot=data.frame(T1,T2,as.factor(color))
  colnames(ScoresPlot)=c("T1","T2","color")
  plotOut <- ggplot2::ggplot(ScoresPlot, aes(x=T1, y=T2, color=as.factor(color))) + 
    ggplot2::geom_vline(xintercept=0)+
    ggplot2::geom_hline(yintercept=0)+
    ggplot2::geom_point(size=6)+
    ggplot2::xlab(paste0("Scores on ",axis.label,"1"))+
    ggplot2::ylab(paste0("Scores on ",axis.label,"2"))+
    ggplot2::ggtitle(plot.title)+
    ggplot2::xlim(-1.1*max(abs(T1)),1.1*max(abs(T1)))+
    ggplot2::ylim(-1.1*max(abs(T2)),1.1*max(abs(T2)))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white',colour='black'), 
          text = ggplot2::element_text(size=20),
          panel.border = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text = ggplot2::element_text(color = "black"),
          axis.ticks = ggplot2::element_line(color="black")
    )+
    ggplot2::labs(color = color.str)
  return(plotOut)
}

#' Make a Scores Plot with Gradient Colored Points
#' 
#' @description Used to create scores plot colored by quantitative variable of interest.
#' @param T1,T2 Sample scores in your two components (e.g.: PC1 and PC2)
#' @param color A character or factor array of your experimental groups by sample
#' @param color.str Text to label the color legend
#' @param analysis Set to "PCA" to change axis labels to PC instead of LV; defaults to "PLSDA" with LV axes; sets default plot title
#' @param plot.title Set title of the scores plot; if not given, defaults to analysis
#' @return A ggplot2 object of the scores plot
#' @export
scores_plot_gradient <- function(T1,T2,color,color.str,analysis="PLSDA",plot.title=NULL){
  if(is.null(plot.title)){plot.title=analysis}
  if(analysis=="PCA"){axis.label="PC"}else{axis.label="LV"}
  breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
  barColors = gplots::colorpanel(length(breakBarColors)-1, "blue", "white", "red2")
  sc = ggplot2::scale_colour_gradientn(colours = barColors, limits=c(min(color), max(color)))
  ScoresPlot=data.frame(T1,T2,color)
  colnames(ScoresPlot)=c("T1","T2","color")
  plotOut <- ggplot2::ggplot(ScoresPlot, aes(x=T1, y=T2, color=color)) + 
    ggplot2::geom_vline(xintercept=0)+
    ggplot2::geom_hline(yintercept=0)+
    ggplot2::geom_point(size=6)+
    sc+
    ggplot2::xlab(paste0("Scores on ",axis.label,"1"))+
    ggplot2::ylab(paste0("Scores on ",axis.label,"2"))+
    ggtitle(plot.title)+
    ggplot2::xlim(-1.1*max(abs(T1)),1.1*max(abs(T1)))+
    ggplot2::ylim(-1.1*max(abs(T2)),1.1*max(abs(T2)))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white',colour='black'), 
          text = ggplot2::element_text(size=20),
          panel.border= ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text = ggplot2::element_text(color = "black"),
          axis.ticks = ggplot2::element_line(color="black")
    )+
    ggplot2::labs(color = color.str)
  return(plotOut)
}

#' Make a Scores Plot with Confidence Ellipses
#'
#' @description Create scores plot colored by categorical variable of interest with ellipses showing confidence level of each group.
#' @param T1,T2 Sample scores in your two components (e.g.: PC1 and PC2)
#' @param color A character or factor array of your experimental groups by sample
#' @param color.str Text to label the color legend
#' @param analysis Set to "PCA" to change axis labels to PC instead of LV; defaults to "PLSDA" with LV axes; sets default plot title
#' @param plot.title Set title of the scores plot; if not given, defaults to analysis
#' @param level Confidence level of ellipse, default set to 0.95
#' @return A ggplot2 object of the scores plot
#' @export
scores_plot_ellipse <- function(T1,T2,color,color.str="",analysis="PLSDA",plot.title=NULL,level=0.95){
  if(is.null(plot.title)){plot.title=analysis}
  if(analysis=="PCA"){axis.label="PC"}else{axis.label="LV"}
  ScoresPlot=data.frame(T1,T2,as.factor(color))
  colnames(ScoresPlot)=c("T1","T2","color")
  plotOut <- ggplot2::ggplot(ScoresPlot, aes(x=T1, y=T2, color=as.factor(color))) + 
    ggplot2::geom_vline(xintercept=0)+
    ggplot2::geom_hline(yintercept=0)+
    ggplot2::geom_point(size=6)+
    ggplot2::xlab(paste0("Scores on ",axis.label,"1"))+
    ggplot2::ylab(paste0("Scores on ",axis.label,"2"))+
    ggplot2::stat_ellipse(level=level)+
    ggplot2::ggtitle(plot.title)+
    ggplot2::xlim(-1.1*max(abs(T1)),1.1*max(abs(T1)))+
    ggplot2::ylim(-1.1*max(abs(T2)),1.1*max(abs(T2)))+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white',colour='black'), 
          text = ggplot2::element_text(size=20),
          panel.border= ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text = ggplot2::element_text(color = "black"),
          axis.ticks = ggplot2::element_line(color="black")
    )+
    ggplot2::labs(color = color.str)
  return(plotOut)
}