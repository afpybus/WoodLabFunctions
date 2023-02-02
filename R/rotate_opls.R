#Used to rotate scores and loadings on opls objects by specified degrees, then perform axis swaps and/or flips. Output is list of scores, loadings, and rotation matrix.
rotate_opls <- function(plsOut,degrees,swap=F,flip1=F,flip2=F){
  #rotate by specified degrees
  theta=degrees*pi/180
  rotmat=rbind(c(cos(theta),-sin(theta)),c(sin(theta), cos(theta)))
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
  if(flip1==TRUE){
    P1=-P1
    T1=-T1
  }
  if(flip2==TRUE){
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