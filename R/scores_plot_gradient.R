#Used to create scores plot colored by quantitative variable of interest.
scores_plot_gradient <- function(T1,T2,color,color.str,analysis="PLSDA",plot.title=NULL){
  if (!require("tidyverse")) install.packages("tidyverse")
  if(is.null(plot.title)){plot.title=analysis}
  if(analysis=="PCA"){axis.label="PC"}else{axis.label="LV"}
  breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
  barColors = colorpanel(length(breakBarColors)-1, "blue", "white", "red2")
  sc = scale_colour_gradientn(colours = barColors, limits=c(min(color), max(color)))
  ScoresPlot=data.frame(T1,T2,color)
  colnames(ScoresPlot)=c("T1","T2","color")
  plotOut <-    ggplot(ScoresPlot, aes(x=T1, y=T2, color=color)) + 
    geom_vline(xintercept=0)+
    geom_hline(yintercept=0)+
    geom_point(size=6)+
    sc+
    xlab(paste0("Scores on ",axis.label,"1"))+
    ylab(paste0("Scores on ",axis.label,"2"))+
    ggtitle(plot.title)+
    xlim(-1.1*max(abs(T1)),1.1*max(abs(T1)))+
    ylim(-1.1*max(abs(T2)),1.1*max(abs(T2)))+
    theme(panel.background = element_rect(fill = 'white',colour='black'), 
          text = element_text(size=20),
          panel.border=element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(color = "black"),
          axis.ticks=element_line(color="black")
    )+
    labs(color = color.str)
  return(plotOut)
}