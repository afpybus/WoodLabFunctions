#Used to create scores plot colored by categorical variable of interest, saved to working directory. No output.
scores_plot_ellipse <- function(T1,T2,color,color.str="",analysis="PLSDA",plot.title=NULL,level=0.95){
  
  if (!require("tidyverse")) install.packages("tidyverse")
  if(is.null(plot.title)){plot.title=analysis}
  if(analysis=="PCA"){axis.label="PC"}else{axis.label="LV"}
  ScoresPlot=data.frame(T1,T2,as.factor(color))
  colnames(ScoresPlot)=c("T1","T2","color")
  plotOut <- ggplot(ScoresPlot, aes(x=T1, y=T2, color=as.factor(color))) + 
    geom_vline(xintercept=0)+
    geom_hline(yintercept=0)+
    geom_point(size=6)+
    xlab(paste0("Scores on ",axis.label,"1"))+
    ylab(paste0("Scores on ",axis.label,"2"))+
    stat_ellipse(level=level)+
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