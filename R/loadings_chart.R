#' Loadings chart for multivariate analyses (PCA,PLS)
#'
#' @description Used to create loadings bar plots sorted by value and scaled by diving by max value.
#' @param loadings loadings variable (e.g. P1 or P2 output from rotate_opls)
#' Set rownames() for analyte labels
#' @param component_str string with name of component (e.g. "LV1" or "LV2")
#' @param lim default 1; axis limit
#' @param cex numeric scaling factor for labels
#' @param col default "lightblue"; set for bar colors in barplot()
#' @return A ggplot object of the loadings chart in ascending order
#' @export
loadings_chart <- function(loadings,component_str,lim=1,cex=1,col="lightblue"){
  P1Sort=base::sort(t(loadings))/max(abs(loadings))
  indP1=base::sort(loadings, index.return=TRUE)$ix
  if(length(col)>1){col=col[indP1]}
  plotOut <- barplot(P1Sort,
                     main = paste0("Signals in ",component_str),
                     xlab = "",
                     ylab = "",
                     cex.names = cex,
                     names.arg = rownames(loadings)[indP1],
                     col = col,
                     horiz = FALSE,
                     las=2,
                     ylim=c(-lim,lim))
  return(plotOut)
}


#' Alternate loadings chart for multivariate analyses (PCA,PLS) using ggpubr
#'
#' @description Used to create loadings bar plots sorted by value and scaled by diving by max value.
#' @param loadings loadings variable (e.g. P1 or P2 output from rotate_opls)
#' Set rownames() for analyte labels
#' @param component_str string with name of component (e.g. "LV1" or "LV2")
#' @param x_size default 10; text size for x axis text
#' @return A ggplot object of the loadings chart in ascending order
#' @export
loadings_chart2 <- function(loadings,component_str,x_size=10){
  library("ggpubr")
  P1Sort=base::sort(t(loadings))/max(abs(loadings))
  indP1=base::sort(loadings, index.return=TRUE)$ix
  
  df <- data.frame(P1Sort,rownames(loadings)[indP1])
  colnames(df) <- c("Loadings","Analytes")
  plotOut <- ggbarplot(df,x="Analytes",y="Loadings",fill="lightblue",xlab = "",ylab="",width = 0.8) +
    rotate_x_text() +
    ggtitle(paste0("Signals in ",component_str)) +
    theme(plot.title = element_text(hjust=0.5,face = "bold"),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = x_size))
  return(plotOut)
}


#' Loadings chart with sd error bars for multivariate analyses (PCA,PLS)
#'
#' @description Used to create loadings bar plots with sd error bars sorted by value and scaled by diving by max value.
#' @param loadings loadings variable (e.g. P1 or P2 output from rotate_opls)
#' Set rownames() for analyte labels
#' @param sd standard deviations variable (e.g. sd_P1_LOOCV or sd_P2_LOOCV output from rotate_opls_LOOCV)
#' @param component_str string with name of component (e.g. "LV1" or "LV2")
#' @param lim default 1; axis limit
#' @param cex numeric scaling factor for labels
#' @param col default "lightblue"; set for bar colors in barplot()
#' @return A ggplot object of the loadings chart in ascending order with sd error bars 
#' @export
loadings_chart_sd <- function(loadings,sd,component_str,lim=1,cex=1,col="lightblue"){
  P1Sort=base::sort(t(loadings))/max(abs(loadings))
  sd=sd/max(abs(loadings))
  indP1=base::sort(loadings, index.return=TRUE)$ix
  if(length(col)>1){col=col[indP1]}
  barCenters <- barplot(P1Sort,
                        main = paste0("Signals in ",component_str),
                        xlab = "",
                        ylab = "",
                        cex.names = cex,
                        names.arg = rownames(loadings)[indP1],
                        col = col,
                        horiz = FALSE,
                        las=2,
                        ylim=c(-lim,lim))
  segments(barCenters,P1Sort-sd[indP1],barCenters,P1Sort+sd[indP1])
  arrows(barCenters,P1Sort-sd[indP1],barCenters,P1Sort+sd[indP1],code=3,angle=90,length=0.05)
  return(barCenters)
}
