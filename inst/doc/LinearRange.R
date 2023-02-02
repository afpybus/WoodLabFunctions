## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "tools/LinearRange-",
  warning = FALSE
)

## ----Installation, eval = FALSE----------------------------------------------------------------------------------------------------------
#  # Install
#  if(!require(devtools)) install.packages("devtools")
#  devtools::install_github("afpybus/WoodLabFunctions")

## ----Load Data---------------------------------------------------------------------------------------------------------------------------
library("WoodLabFunctions")
data(LinearRange)

LinearRange

## ----Scatter Graph, fig.width=4,fig.height=4---------------------------------------------------------------------------------------------
scatter_reg(x=LinearRange$Mass,y=LinearRange$pJnk,
            xlab="Protein Mass [ug]",ylab="pJnk [a.u.]",
            showRSQ=FALSE,showP=FALSE,
            method="loess")


## ----Linear Range Panel, fig.height=10,fig.width=12--------------------------------------------------------------------------------------
loadings = LinearRange$Mass
analytes = LinearRange[,2:10]

LinearRangePanel(analytes = analytes,loadings = loadings,xlab = "Protein Mass [ug]")


