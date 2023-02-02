#' Rotate an opls object
#'
#' @description Used to rotate scores and loadings on opls objects by specified degrees, then perform axis swaps and/or flips. Output is list of scores, loadings, and rotation matrix.
#' @param plsOut output object from opls() function, must contain only 2 components
#' Ex. plsOut = opls(data,predI=2) for Principal Component Analysis
#' plsOut = opls(data,y=pheno,predI=2) for PLS
#' @param degrees default 0; specified degrees of clockwise rotation
#' @param swap default FALSE; set to TRUE to swap x and y axes
#' @param flip_y default FALSE; set to TRUE to flip data across y axis
#' @param flip_x default FALSE; set to TRUE to flip data across x axis
#' @return A list with the following objects:
#' T1 = scores in rotated LV1
#' T2 = scores in rotated LV2
#' P1 = loadings in rotated LV1
#' P2 = loadings in rotated LV2
#' rotmat = matrix of rotation by specified degrees
#' @export
rotate_opls <- function(plsOut,degrees=0,swap=F,flip_y=F,flip_x=F){
  #rotate by specified degrees
  theta=degrees*pi/180
  rotmat=base::rbind(c(cos(theta),-sin(theta)),c(sin(theta), cos(theta)))
  scores = plsOut@scoreMN %*% rotmat
  T1=scores[,1]
  T2=scores[,2]
  loadings = plsOut@loadingMN %*% rotmat
  P1=loadings[,1]
  P2=loadings[,2]
  
  #swap axis 1 and 2 if specified
  if(swap==TRUE){
    P1temp=P1; P2temp=P2
    P1=P2temp; P2=P1temp
    T1temp=T1; T2temp=T2
    T1=T2temp; T2=T1temp
  }
  
  #Flip axes 1 and/or 2 if specified
  if(flip_y==TRUE){
    P1=-P1
    T1=-T1
  }
  if(flip_x==TRUE){
    P2=-P2
    T2=-T2
  }
  out <- list()
  out$T1 <- T1
  out$T2 <- T2
  out$P1 <- as.matrix(P1)
  out$P2 <- as.matrix(P2)
  out$rotmat <- rotmat
  return(out)
}