#' Heatmap3 made simple(r)
#'
#' @description Create a heatmap with minimal input
#' @param data data frame or matrix for heatmap; will be scaled across columns
#' @param mar default c(5,2); margin for column and row names
#' @param title default ""; set a title
#' @param xlab,ylab default ""; set axis labels
#' @param color1,color2,color3 default blue to red color scale; set to change heatmap colors
#' @param cex_r,cex_c default 1; numeric scale factor for axis text
#' @param z_lim default 1.5; numeric to set max/min colors in z-score scale
#' @param clust_r set to TRUE to cluster rows
#' @param clust_c set to TRUE to cluster columns
#' @param labRow default is rownames(data); sets row labels
#' @param labCol default is colnames(data); sets col labels
#' @return A heatmap of your data
#' @export
heatmap_wl <- function(data, mar=c(5,2),title="", xlab = "", ylab = "", 
                       color1 = "blue", color2 = "white", color3 = "red2",
                       cex_r=1,cex_c=1,z_lim=1.5,
                       clust_r=FALSE,clust_c=FALSE,
                       labRow=rownames(data),labCol=colnames(data)) {
  dataZ = apply(data,2,"scale")
  rownames(dataZ) = rownames(data)
  breakBarColors=c(-200,seq(-z_lim, z_lim, 0.01),200) #Outside numbers clip outliers, used for z-scored data
  barColors = gplots::colorpanel(length(breakBarColors)-1, color1, color2, color3)
  if(clust_c){hc=hclust(dist(t(dataZ), method = "euclidean"), method = "average")
  Colv = as.dendrogram(hc)}else{Colv=NA}
  if(clust_r){hr=hclust(dist(dataZ, method = "euclidean"), method = "average")
  Rowv = as.dendrogram(hr)}else{Rowv=NA}
  hm <- heatmap3::heatmap3(dataZ, 
                           col=barColors, breaks=breakBarColors,legendfun=function()heatmap3::showLegend(legend=c(NA),col=c(NA),cex=2.5),
                           Rowv=Rowv,Colv=Colv,
                           labRow = labRow, labCol = labCol,
                           scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,
                           highlightCell=data.frame(rep(1:dim(dataZ)[1],each=(dim(dataZ)[2]+1)),rep(1:(dim(dataZ)[2]+1),times=dim(dataZ)[1]),'black',1),
                           main = title, xlab = xlab, ylab = ylab)
  out = list()
  out$bbc = breakBarColors
  out$bc = barColors
  return(out)
}


#' Heatmap3 made simple(r), with Side Bar of specified colors
#'
#' @description Create a heatmap with minimal input
#' @param data data frame or matrix for heatmap; will be scaled across columns
#' @param RowSideColors variable of colors to be labeled in column next to heatmap
#' To label the color column, set RowSideColors=cbind(label=c(RowSideColors))
#' @param mar default c(5,2); margin for column and row names
#' @param title default ""; set a title
#' @param xlab,ylab default ""; set axis labels
#' @param color1,color2,color3 default blue to red color scale; set to change heatmap colors
#' @param cex_r,cex_c default 1; numeric scale factor for axis text
#' @param z_lim default 1.5; numeric to set max/min colors in z-score scale
#' @param clust_r set to TRUE to cluster rows
#' @param clust_c set to TRUE to cluster columns
#' @param labRow default is rownames(data); sets row labels
#' @param labCol default is colnames(data); sets col labels
#' @return A heatmap of your data
#' @export
heatmap_wl_sidebar <- function(data, RowSideColors, 
                               mar=c(5,2),title="", xlab = "", ylab = "", 
                               color1 = "blue", color2 = "white", color3 = "red2",
                               cex_r=1,cex_c=1,z_lim=1.5,
                               clust_r=FALSE,clust_c=FALSE,
                               labRow=rownames(data),labCol=colnames(data)) {
  dataZ = apply(data,2,"scale")
  rownames(dataZ) = rownames(data)
  breakBarColors=c(-200,seq(-z_lim, z_lim, 0.01),200) #Outside numbers clip outliers, used for z-scored data
  barColors = gplots::colorpanel(length(breakBarColors)-1, color1, color2, color3)
  if(clust_c){hc=hclust(dist(t(dataZ), method = "euclidean"), method = "average")
  Colv = as.dendrogram(hc)}else{Colv=NA}
  if(clust_r){hr=hclust(dist(dataZ, method = "euclidean"), method = "average")
  Rowv = as.dendrogram(hr)}else{Rowv=NA}
  hm <- heatmap3::heatmap3(dataZ, 
                           col=barColors, breaks=breakBarColors,legendfun=function()heatmap3::showLegend(legend=c(NA),col=c(NA),cex=2.5),
                           Rowv=Rowv,Colv=Colv,
                           labRow = labRow, labCol = labCol,
                           scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,
                           highlightCell=data.frame(rep(1:dim(dataZ)[1],each=(dim(dataZ)[2]+1)),rep(1:(dim(dataZ)[2]+1),times=dim(dataZ)[1]),'black',1),
                           main = title, xlab = xlab, ylab = ylab,RowSideColors = RowSideColors)
  out = list()
  out$bbc = breakBarColors
  out$bc = barColors
  return(out)
}

#' Make a color spectrum graphic
#'
#' @description Create color bars (taken from http://www.colbyimaging.com/wiki/statistics/color-bars)
#' @param bc barColors, the character variable of colors across the spectrum
#' @param min numeric; the value corresponding to the first color in bc (usually -1.5)
#' @param max numeric; the value corresponding to the last color in bc (usually 1.5)
#' @param nticks default 5; number of ticks
#' @param ticks variable of where you want the ticks to show; default is evenly spaced
#' @param title character to set title
#' @return A color bar for use in making graphics
#' @export
color.bar = function(bc, min, max=-min, nticks=5, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(bc)-1)/(max-min)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(bc)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=bc[i], border=NA)
  }
}



# 
# 
# Heatmap_SideCol <- function(hm_mat,SideCol,title="",x.str="",y.str="",color1="blue",color2="white",color3="red2",cex_r=1,cex_c=1,mar=c(5,2)){
#   if (!require("gplots")) install.packages("gplots")
#   if (!require("heatmap3")) install.packages("heatmap3")
#   breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
#   barColors = colorpanel(length(breakBarColors)-1, color1, color2, color3)
#   hc = hclust(dist(t(hm_mat), method = "euclidean"), method = "average")
#   ColLabels = colnames(hm_mat)
#   RowLabels = NA #rownames(hm_mat)
#   
#   output_hm <- heatmap3(hm_mat,
#                         col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
#                         Rowv=NA,
#                         Colv=as.dendrogram(hc),
#                         scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
#                         highlightCell=dataZ.frame(rep(1:dim(hm_mat)[1],each=(dim(hm_mat)[2]+1)),rep(1:(dim(hm_mat)[2]+1),times=dim(hm_mat)[1]),'black',1),
#                         labCol=ColLabels, labRow=RowLabels, main = title, xlab = x.str, ylab = y.str, RowSideColors = SideCol
#   )
# }
# 
# 
# # Correlation Heatmap ######################
# Heatmap_Cor <- function(hmdataZ, title="", x.str = "", y.str = "", 
#                         color1 = "black", color2 = "white", color3 = "orange", 
#                         cex_r=1,cex_c=0.6,mar=c(12,13)){
#   if (!require("gplots")) install.packages("gplots")
#   if (!require("heatmap3")) install.packages("heatmap3")
#   breakBarColors=c(-200,seq(-1, 1, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
#   barColors = colorpanel(length(breakBarColors)-1, color1, color2, color3)
#   
#   corOut=cor(hmdataZ)
#   corOut[is.na(corOut)] = 0
#   hCor=hclust(as.dist((1-corOut)/2))
#   
#   heatmap3(corOut,
#            col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA)), 
#            Rowv=as.dendrogram(hCor), Colv=as.dendrogram(hCor),  scale="none",margins=mar,
#            cexCol=cex_c,cexRow=cex_r,
#            main = title, xlab = x.str, ylab = y.str)
# }
# 
# Heatmap_p <- function(hmdataZ, title="", x.str = "", y.str = "", 
#                       color1 = "red",color2= "green", color3 = "black", 
#                       cex_r=1,cex_c=0.6,mar=c(12,13),
#                       loglimit=-5){
#   if (!require("gplots")) install.packages("gplots")
#   if (!require("heatmap3")) install.packages("heatmap3")
#   breakBarColors=c(0,10^seq(loglimit, 0, 0.05)) 
#   barColors = colorpanel(length(breakBarColors)-1, color1, color2)#, color3)
#   
#   corOut_FDR= rcorr.adjustFDR(hmdataZ)
#   
#   corOut=cor(hmdataZ)
#   corOut[is.na(corOut)] = 0
#   hCor=hclust(as.dist((1-corOut)/2))
#   
#   heatmap3(corOut_FDR$P,
#            col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA)), 
#            Rowv=as.dendrogram(hCor), Colv=as.dendrogram(hCor),  scale="none",margins=mar,
#            cexCol=cex_c,cexRow=cex_r,
#            main = title, xlab = x.str, ylab = y.str)
# }

