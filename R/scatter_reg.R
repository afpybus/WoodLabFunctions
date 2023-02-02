# UNDER CONSTRUCTION #############






# scatter_reg by Brae, edited AFP 1/24/21 ########



scatter_reg <- function(x,y,x.str="",y.str="",title="",point_color="black",line_color="red", 
                        showRSQ = TRUE, showPVAL = TRUE, 
                        line_size = 1.5, point_size = 4, line_format = "longdash", axisfontsize = 14, 
                        titlefontsize = 20,
                        text_position = "top right") {
  dataReg=data.frame(x,y)
  colnames(dataReg) <- c("x","y")
  linmodel=lm(y~x,dataReg)
  RSQ=summary(linmodel)$r.squared
  f <- summary(linmodel)$fstatistic
  pval <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(pval) <- NULL
  
  if (text_position == "top right"){p_x=0.8; p_y=1;r_x=0.8; r_y=0.9}
  if (text_position == "top left"){p_x=0.2; p_y=1;r_x=0.2; r_y=0.9}
  if (text_position == "bottom right"){p_x=0.8; p_y=0.15;r_x=0.8; r_y=0.05}
  if (text_position == "bottom left"){p_x=0.2; p_y=0.15;r_x=0.2; r_y=0.05}
  
  
  if(showRSQ == FALSE){if(r_y==0.05){p_y=r_y}; r_x = NaN}
  if(showPVAL== FALSE){if(p_y==1){r_y=p_y};p_x = NaN}
  
  output_plot <- ggplot(dataReg, aes(x=x, y=y)) +
    geom_smooth(method='lm',se=FALSE,color=line_color,linetype=line_format, size = line_size)+
    geom_point(size=point_size,color=point_color)+
    theme(panel.background = element_rect(fill = 'transparent'),
          text = element_text(size=20),
          axis.text.x = element_text(color = "black",
                                     size = axisfontsize),
          axis.text.y = element_text(color = "black",
                                     size = axisfontsize),
          plot.title = element_text(hjust = 0.5,face='bold',size=titlefontsize),
          panel.border=element_blank(),
          axis.line=element_line(color='black'),
          plot.margin=margin(10,20,10,10)
    )+
    xlab(x.str)+
    ylab(y.str)+
    annotate("text",size=6, x={r_x*max(x,na.rm=T)+(1-r_x)*min(x,na.rm=T)}, y={r_y*max(y,na.rm=T)+(1-r_y)*min(y,na.rm=T)}, label=bquote(R^2==.(formatC(RSQ, digits=2))))+
    annotate("text",size=6, x=p_x*max(x,na.rm=T)+(1-p_x)*min(x,na.rm=T), y=p_y*max(y,na.rm=T)+(1-p_y)*min(y,na.rm=T), label= bquote(p==.(formatC(pval, digits=2))))+
    ggtitle(title)
  
  return(output_plot)
}


# scatter_reg_group

scatter_reg_group <- function(x,y,grouping,x.str="",y.str="",title="",
                              colors=c("red","black"), 
                              showRSQ = TRUE, showPVAL = TRUE, 
                              line_size = 1.5, point_size = 4, 
                              line_format = "longdash", axisfontsize = 14, 
                              text_position = "top right") {
  dataReg=data.frame(x,y,factor(grouping))
  colnames(dataReg) <- c("x","y","grouping")
  
  RSQ=matrix(ncol=length(unique(dataReg$grouping)))
  pval=matrix(ncol=length(unique(dataReg$grouping)))
  for(i in 1:length(unique(dataReg$grouping))){
    linmodel=lm(y~x,dataReg,which(grouping==unique(grouping)[i]))
    RSQ[i]=summary(linmodel)$r.squared
    f <- summary(linmodel)$fstatistic
    pval[i] <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(pval) <- NULL
  }
  
  
  if (text_position == "top right"){p_x=0.8; p_y=1;r_x=0.8; r_y=0.9; p_x2=p_x; r_x2=r_x; p_y2=0.75; r_y2=0.65}
  if (text_position == "top left"){p_x=0.2; p_y=1;r_x=0.2; r_y=0.9}
  if (text_position == "bottom right"){p_x=0.8; p_y=0.15;r_x=0.8; r_y=0.05; p_x2=p_x; r_x2=r_x; p_y2=0.35; r_y2=0.25}
  if (text_position == "bottom left"){p_x=0.2; p_y=0.15;r_x=0.2; r_y=0.05}
  
  if(showRSQ == FALSE){if(r_y==0.05){p_y=r_y}; r_x = NaN}
  if(showPVAL== FALSE){if(p_y==1){r_y=p_y};p_x = NaN}
  
  output_plot <- ggplot(dataReg, aes(x=x, y=y, group=grouping, color=grouping)) +
    geom_smooth(method='lm',se=FALSE,size = line_size,linetype=line_format)+
    geom_point(size=point_size)+
    scale_color_manual(values=colors)+
    theme(panel.background = element_rect(fill = 'transparent'),
          text = element_text(size=20),
          axis.text.x = element_text(color = "black",
                                     size = axisfontsize),
          axis.text.y = element_text(color = "black",
                                     size = axisfontsize),
          plot.title = element_text(hjust = 0.5,face='bold'),
          panel.border=element_blank(),
          axis.line=element_line(color='black'),
          plot.margin=margin(10,20,10,10)
    )+
    xlab(x.str)+
    ylab(y.str)+
    annotate("text",size=6, x={r_x2*max(x,na.rm=T)+(1-r_x2)*min(x,na.rm=T)}, y={r_y2*max(y,na.rm=T)+(1-r_y2)*min(y,na.rm=T)}, label=bquote(R^2==.(formatC(RSQ[2], digits=2))),colour="red")+
    annotate("text",size=6, x=p_x2*max(x,na.rm=T)+(1-p_x2)*min(x,na.rm=T), y=p_y2*max(y,na.rm=T)+(1-p_y2)*min(y,na.rm=T), label= bquote(p==.(formatC(pval[2], digits=2))),colour="red")+
    annotate("text",size=6, x={r_x*max(x,na.rm=T)+(1-r_x)*min(x,na.rm=T)}, y={r_y*max(y,na.rm=T)+(1-r_y)*min(y,na.rm=T)}, label=bquote(R^2==.(formatC(RSQ[1], digits=2))),colour="black")+
    annotate("text",size=6, x=p_x*max(x,na.rm=T)+(1-p_x)*min(x,na.rm=T), y=p_y*max(y,na.rm=T)+(1-p_y)*min(y,na.rm=T), label= bquote(p==.(formatC(pval[1], digits=2))),colour="black")+
    ggtitle(title)
  
  return(output_plot)
}

