#' Scatter plot with regression
#'
#' @description Make scatter plot with a regression line and display statistics
#' Please note: R^2 and p-value do NOT change with method input - always OLS "lm" method
#' @param x numeric x variable
#' @param y numeric y variable
#' @param title default ""; set a title
#' @param xlab,ylab default ""; set axis labels
#' @param point_size default 4; numeric size of points
#' @param line_color default "red"; regression line color
#' @param line_size default 1.5; regression line size
#' @param linetype default "longdash"; regression line type
#' @param axisfontsize default 14; axis font size
#' @param method default is "lm"; set regression method
#' @param showRSQ,showP default is TRUE; set to FALSE to hide regression statistics
#' @param text_position default is "lower right"
#' Allowable values are "lower left", "upper right", "upper left"
#' @param xintercept default is NA; change to numeric to show vertical line at ideal loading
#' @return A y vs x scatter plot with regression line and OLS regression statistics
#' @export
scatter_reg <- function(x,y,
                        xlab="",ylab="",title="",
                        point_color="black", point_size = 4,
                        line_color="red",line_size = 1.5,  linetype = "longdash", 
                        axisfontsize = 14,
                        method="lm",
                        showRSQ = TRUE,showP = TRUE,
                        text_position = "lower right",
                        xintercept = NA){
  dataReg=data.frame(y,x)
  linmodel=lm(y~x,dataReg)
  RSQ=summary(linmodel)$r.squared
  f <- summary(linmodel)$fstatistic
  pval <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(pval) <- NULL
  if (text_position == "upper right"){p_x=0.8; p_y=1;r_x=0.8; r_y=0.9}
  if (text_position == "upper left"){p_x=0.2; p_y=1;r_x=0.2; r_y=0.9}
  if (text_position == "lower right"){p_x=0.8; p_y=0.15;r_x=0.8; r_y=0.05}
  if (text_position == "lower left"){p_x=0.2; p_y=0.15;r_x=0.2; r_y=0.05}
  if(showRSQ == FALSE){if(r_y==0.05){p_y=r_y}; r_x = NaN}
  if(showP== FALSE){if(p_y==1){r_y=p_y};p_x = NaN}
  
  p <- ggplot(dataReg, aes(x=x, y=y)) +
    geom_smooth(method=method,se=FALSE,color=line_color,linetype=linetype, size = line_size)+
    geom_point(size=point_size,color=point_color)+
    theme(panel.background = element_rect(fill = 'white'),
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
    xlab(xlab)+ylab(ylab)+ggtitle(title)+
    annotate("text",size=6, x={r_x*max(x,na.rm=T)+(1-r_x)*min(x,na.rm=T)}, y={r_y*max(y,na.rm=T)+(1-r_y)*min(y,na.rm=T)}, label=bquote(R^2==.(formatC(RSQ, digits=2))))+
    annotate("text",size=6, x=p_x*max(x,na.rm=T)+(1-p_x)*min(x,na.rm=T), y=p_y*max(y,na.rm=T)+(1-p_y)*min(y,na.rm=T), label= bquote(p==.(formatC(pval, digits=2))))
  if(!is.na(xintercept)){p=p+geom_vline(xintercept = xintercept,linetype="dotted",color="darkgray",size=line_size)}
  return(p)
}

#' Linear Range Panel
#'
#' @description Create a panel of linear range graphs from a single data frame
#' @param analytes data frame or matrix of analytes measured
#' @param loadings numeric variable of loadings in same order as analytes matrix
#' @param xlab default "Loading"; character to change x axis label
#' @param xintercept default is NA; change to numeric to show vertical line at ideal loading
#' @return A panel of linear range scatter graphs
#' @export
LinearRangePanel <- function(analytes,loadings,xlab="Loading",xintercept=NA){
  grid.plots=list()
  for(i in 1:ncol(analytes)){
    p = scatter_reg(x=loadings,y=as.matrix(analytes[,i]),
                    xlab=xlab,ylab=paste(colnames(analytes)[i],"[a.u.]"),
                    showRSQ=FALSE,showP=FALSE,
                    method="loess",xintercept = xintercept)
    grid.plots$new = p
    names(grid.plots)[which(names(grid.plots)=="new")] = paste0("analyte",i)
  }
  nrow=floor(sqrt(ncol(analytes)))
  ncol=ceiling(ncol(analytes)/nrow)
  out = gridExtra::grid.arrange(grobs=grid.plots,nrow=nrow,top=ggpubr::text_grob(paste0("Linear Range"),size = 24,face = "bold"))
  return(out)
}


# 
# # scatter_reg_group
# 
# scatter_reg_group <- function(x,y,grouping,x.str="",y.str="",title="",
#                               colors=c("red","black"), 
#                               showRSQ = TRUE, showPVAL = TRUE, 
#                               line_size = 1.5, point_size = 4, 
#                               line_format = "longdash", axisfontsize = 14, 
#                               text_position = "top right") {
#   dataReg=data.frame(x,y,factor(grouping))
#   colnames(dataReg) <- c("x","y","grouping")
#   
#   RSQ=matrix(ncol=length(unique(dataReg$grouping)))
#   pval=matrix(ncol=length(unique(dataReg$grouping)))
#   for(i in 1:length(unique(dataReg$grouping))){
#     linmodel=lm(y~x,dataReg,which(grouping==unique(grouping)[i]))
#     RSQ[i]=summary(linmodel)$r.squared
#     f <- summary(linmodel)$fstatistic
#     pval[i] <- pf(f[1],f[2],f[3],lower.tail=F)
#     attributes(pval) <- NULL
#   }
#   
#   
#   if (text_position == "top right"){p_x=0.8; p_y=1;r_x=0.8; r_y=0.9; p_x2=p_x; r_x2=r_x; p_y2=0.75; r_y2=0.65}
#   if (text_position == "top left"){p_x=0.2; p_y=1;r_x=0.2; r_y=0.9}
#   if (text_position == "bottom right"){p_x=0.8; p_y=0.15;r_x=0.8; r_y=0.05; p_x2=p_x; r_x2=r_x; p_y2=0.35; r_y2=0.25}
#   if (text_position == "bottom left"){p_x=0.2; p_y=0.15;r_x=0.2; r_y=0.05}
#   
#   if(showRSQ == FALSE){if(r_y==0.05){p_y=r_y}; r_x = NaN}
#   if(showPVAL== FALSE){if(p_y==1){r_y=p_y};p_x = NaN}
#   
#   p <- ggplot(dataReg, aes(x=x, y=y, group=grouping, color=grouping)) +
#     geom_smooth(method='lm',se=FALSE,size = line_size,linetype=line_format)+
#     geom_point(size=point_size)+
#     scale_color_manual(values=colors)+
#     theme(panel.background = element_rect(fill = 'transparent'),
#           text = element_text(size=20),
#           axis.text.x = element_text(color = "black",
#                                      size = axisfontsize),
#           axis.text.y = element_text(color = "black",
#                                      size = axisfontsize),
#           plot.title = element_text(hjust = 0.5,face='bold'),
#           panel.border=element_blank(),
#           axis.line=element_line(color='black'),
#           plot.margin=margin(10,20,10,10)
#     )+
#     xlab(x.str)+
#     ylab(y.str)+
#     annotate("text",size=6, x={r_x2*max(x,na.rm=T)+(1-r_x2)*min(x,na.rm=T)}, y={r_y2*max(y,na.rm=T)+(1-r_y2)*min(y,na.rm=T)}, label=bquote(R^2==.(formatC(RSQ[2], digits=2))),colour="red")+
#     annotate("text",size=6, x=p_x2*max(x,na.rm=T)+(1-p_x2)*min(x,na.rm=T), y=p_y2*max(y,na.rm=T)+(1-p_y2)*min(y,na.rm=T), label= bquote(p==.(formatC(pval[2], digits=2))),colour="red")+
#     annotate("text",size=6, x={r_x*max(x,na.rm=T)+(1-r_x)*min(x,na.rm=T)}, y={r_y*max(y,na.rm=T)+(1-r_y)*min(y,na.rm=T)}, label=bquote(R^2==.(formatC(RSQ[1], digits=2))),colour="black")+
#     annotate("text",size=6, x=p_x*max(x,na.rm=T)+(1-p_x)*min(x,na.rm=T), y=p_y*max(y,na.rm=T)+(1-p_y)*min(y,na.rm=T), label= bquote(p==.(formatC(pval[1], digits=2))),colour="black")+
#     ggtitle(title)
#   
#   return(p)
# }

