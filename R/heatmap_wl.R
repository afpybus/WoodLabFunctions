# UNDER CONSTRUCTION #############



# #Set heatmap3 color bar parameters
# breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
# barColors = colorpanel(length(breakBarColors)-1, "blue", "white", "red")
# 
# breakBarColorsCor=c(-200,seq(-1, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
# barColorsCor = colorpanel(length(breakBarColorsCor)-1, "black", "white", "orange")


# Heatmap_Func by Brae ###################

Heatmap_Func <- function(heatMapdata, title, x_str = "", y_str = "", color1 = "blue", color2 = "white", color3 = "red2", verticalcluster = TRUE, horizontalcluster = TRUE,cex_r=1,cex_c=0.6,mar=c(5,2)) {
  if (!require("gplots")) install.packages("gplots")
  if (!require("heatmap3")) install.packages("heatmap3")
  breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
  barColors = colorpanel(length(breakBarColors)-1, color1, color2, color3)
  if (horizontalcluster & verticalcluster) {
    hc = hclust(dist(t(heatMapdata), method = "euclidean"), method = "average")
    hcv = hclust(dist(heatMapdata, method = "euclidean"), method = "average")
    dataHeatMap = data.frame(heatMapdata)
    ColLabels = colnames(heatMapdata)
    RowLabels = rownames(heatMapdata)
    
    output_hm <- heatmap3(heatMapdata,
                          col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
                          Rowv=as.dendrogram(hcv),
                          Colv=as.dendrogram(hc),
                          scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
                          highlightCell=data.frame(rep(1:dim(heatMapdata)[1],each=(dim(heatMapdata)[2]+1)),rep(1:(dim(heatMapdata)[2]+1),times=dim(heatMapdata)[1]),'black',1),
                          labCol=ColLabels, labRow=RowLabels, main = title, xlab = x_str, ylab = y_str
    )
  } else if (horizontalcluster) {
    hc = hclust(dist(t(heatMapdata), method = "euclidean"), method = "average")
    dataHeatMap = data.frame(heatMapdata)
    ColLabels = colnames(heatMapdata)
    RowLabels = rownames(heatMapdata)
    
    output_hm <- heatmap3(heatMapdata,
                          col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
                          Rowv=NA,
                          Colv=as.dendrogram(hc),
                          scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
                          highlightCell=data.frame(rep(1:dim(heatMapdata)[1],each=(dim(heatMapdata)[2]+1)),rep(1:(dim(heatMapdata)[2]+1),times=dim(heatMapdata)[1]),'black',1),
                          labCol=ColLabels, labRow=RowLabels, main = title, xlab = x_str, ylab = y_str
    )
  } else if (verticalcluster) {
    hcv = hclust(dist(heatMapdata, method = "euclidean"), method = "average")
    dataHeatMap = data.frame(heatMapdata)
    ColLabels = colnames(heatMapdata)
    RowLabels = rownames(heatMapdata)
    
    output_hm <- heatmap3(heatMapdata,
                          col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
                          Rowv=as.dendrogram(hcv),
                          Colv=NA,
                          scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
                          highlightCell=data.frame(rep(1:dim(heatMapdata)[1],each=(dim(heatMapdata)[2]+1)),rep(1:(dim(heatMapdata)[2]+1),times=dim(heatMapdata)[1]),'black',1),
                          labCol=ColLabels, labRow=RowLabels, main = title, xlab = x_str, ylab = y_str
    )
  } else {
    dataHeatMap = data.frame(heatMapdata)
    ColLabels = colnames(heatMapdata)
    RowLabels = rownames(heatMapdata)
    
    output_hm <- heatmap3(heatMapdata,
                          col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
                          Rowv=NA,
                          Colv=NA,
                          scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
                          highlightCell=data.frame(rep(1:dim(heatMapdata)[1],each=(dim(heatMapdata)[2]+1)),rep(1:(dim(heatMapdata)[2]+1),times=dim(heatMapdata)[1]),'black',1),
                          labCol=ColLabels, labRow=RowLabels, main = title, xlab = x_str, ylab = y_str
    )
  }
  return(output_hm)
}

Heatmap_SideCol_noclust <- function(hm_mat,SideCol,title="",x.str="",y.str="",color1="blue",color2="white",color3="red2",cex_r=1,cex_c=1,mar=c(5,2)){
  if (!require("gplots")) install.packages("gplots")
  if (!require("heatmap3")) install.packages("heatmap3")
  breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
  barColors = colorpanel(length(breakBarColors)-1, color1, color2, color3)
  #hc = hclust(dist(t(hm_mat), method = "euclidean"), method = "average")
  ColLabels = colnames(hm_mat)
  RowLabels = NA #rownames(hm_mat)
  
  output_hm <- heatmap3(hm_mat,
                        col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
                        Rowv=NA,
                        Colv=NA,
                        scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
                        highlightCell=data.frame(rep(1:dim(hm_mat)[1],each=(dim(hm_mat)[2]+1)),rep(1:(dim(hm_mat)[2]+1),times=dim(hm_mat)[1]),'black',1),
                        labCol=ColLabels, labRow=RowLabels, main = title, xlab = x.str, ylab = y.str, RowSideColors = SideCol
  )
}

Heatmap_SideCol <- function(hm_mat,SideCol,title="",x.str="",y.str="",color1="blue",color2="white",color3="red2",cex_r=1,cex_c=1,mar=c(5,2)){
  if (!require("gplots")) install.packages("gplots")
  if (!require("heatmap3")) install.packages("heatmap3")
  breakBarColors=c(-200,seq(-1.5, 1.5, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
  barColors = colorpanel(length(breakBarColors)-1, color1, color2, color3)
  hc = hclust(dist(t(hm_mat), method = "euclidean"), method = "average")
  ColLabels = colnames(hm_mat)
  RowLabels = NA #rownames(hm_mat)
  
  output_hm <- heatmap3(hm_mat,
                        col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA),cex=2.5),
                        Rowv=NA,
                        Colv=as.dendrogram(hc),
                        scale="none", cexCol=cex_c,cexRow=cex_r, margins=mar,   #5,2
                        highlightCell=data.frame(rep(1:dim(hm_mat)[1],each=(dim(hm_mat)[2]+1)),rep(1:(dim(hm_mat)[2]+1),times=dim(hm_mat)[1]),'black',1),
                        labCol=ColLabels, labRow=RowLabels, main = title, xlab = x.str, ylab = y.str, RowSideColors = SideCol
  )
}


# Correlation Heatmap ######################
Heatmap_Cor <- function(hmData, title="", x.str = "", y.str = "", 
                        color1 = "black", color2 = "white", color3 = "orange", 
                        cex_r=1,cex_c=0.6,mar=c(12,13)){
  if (!require("gplots")) install.packages("gplots")
  if (!require("heatmap3")) install.packages("heatmap3")
  breakBarColors=c(-200,seq(-1, 1, 0.01),200) #Outside numbers clip outliers. This is for zscoring.
  barColors = colorpanel(length(breakBarColors)-1, color1, color2, color3)
  
  corOut=cor(hmData)
  corOut[is.na(corOut)] = 0
  hCor=hclust(as.dist((1-corOut)/2))
  
  heatmap3(corOut,
           col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA)), 
           Rowv=as.dendrogram(hCor), Colv=as.dendrogram(hCor),  scale="none",margins=mar,
           cexCol=cex_c,cexRow=cex_r,
           main = title, xlab = x.str, ylab = y.str)
}

Heatmap_p <- function(hmData, title="", x.str = "", y.str = "", 
                      color1 = "red",color2= "green", color3 = "black", 
                      cex_r=1,cex_c=0.6,mar=c(12,13),
                      loglimit=-5){
  if (!require("gplots")) install.packages("gplots")
  if (!require("heatmap3")) install.packages("heatmap3")
  breakBarColors=c(0,10^seq(loglimit, 0, 0.05)) 
  barColors = colorpanel(length(breakBarColors)-1, color1, color2)#, color3)
  
  corOut_FDR= rcorr.adjustFDR(hmData)
  
  corOut=cor(hmData)
  corOut[is.na(corOut)] = 0
  hCor=hclust(as.dist((1-corOut)/2))
  
  heatmap3(corOut_FDR$P,
           col=barColors, breaks=breakBarColors,legendfun=function()showLegend(legend=c(NA),col=c(NA)), 
           Rowv=as.dendrogram(hCor), Colv=as.dendrogram(hCor),  scale="none",margins=mar,
           cexCol=cex_c,cexRow=cex_r,
           main = title, xlab = x.str, ylab = y.str)
}

