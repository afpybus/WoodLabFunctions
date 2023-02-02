#' Iterative PLS / PLSR with opls() using leave-n out cross validation
#'
#' @description Conducts partial least squares (PLS) regression (PLSR) or discriminant analysis (PLS-DA) using ropls::opls()
#' Iteratively computes PLS scores and loadings runs=100 times, each time leaving out n=1 samples
#' Outputs all iterations of scores and loadings for computing standard deviation
#' 
#' note: the term LOOCV (leave one out cross validation) is used broadly below, but changing value of n in input will leave-n out for all output
#' @param x x input for opls(), the numeric matrix of samples (rows) and analytes (columns)
#' @param y y input for opls(), either an array of regression variable values (PLSR) or an array of factors for group membership (PLSDA)
#' @param n default 1; the number of randomized samples to be left out per iteration, must be an integer
#' Recommend 15% of samples
#' @param runs default 100; 100, the number of iterations to conduct leave-n out cross validation, must be an integer
#' @return A list with the following objects:
#' loadings = loadings in LV1 (first column) and LV2 (second column) from opls() of all samples (not LOOCV)
#' scores = scores in LV1 (first column) and LV2 (second column) from opls() of all samples (not LOOCV)
#' loadings_LOOCV = mean values of loadings in LV1 (first column) and LV2 (second column) from LOOCV iterations
#' LV1_mat = matrix of LV1 output of each LOOCV iteration (rows), for use in calculating standard deviation after rotation
#' LV2_mat = matrix of LV2 output of each LOOCV iteration (rows), for use in calculating standard deviation after rotation
#' @export
opls_LOOCV <- function(x,y,n=1,runs=100){
  oplsOut <- ropls::opls(x,y,predI=2)
  Load_LV1_LOOCV=matrix(0, nrow=runs, ncol=dim(x)[2])
  Load_LV2_LOOCV=matrix(0, nrow=runs, ncol=dim(x)[2])
  #iterations of opls
  for (i in 1:runs)
  {
    leftOut=base::sample(1:dim(x)[1],n,replace=FALSE) #creates randomized array of sample indices
    x_LOOCV=base::apply(x[-leftOut,],2,scale)
    indConstantColumns=matrixStats::colSds(x_LOOCV)<1e-10  #Remove constant columns and then run the PLS
    opls.out_LOOCV <- ropls::opls(x_LOOCV[,!indConstantColumns], y[-leftOut],predI=2,fig.pdfC="none",info.txtC="none")  
    P1_LOOCV=matrix(0,ncol = 1, nrow=dim(x)[2])
    P2_LOOCV=matrix(0,ncol = 1, nrow=dim(x)[2])
    P1_LOOCV[!indConstantColumns]=opls.out_LOOCV@loadingMN[,1]
    P2_LOOCV[!indConstantColumns]=opls.out_LOOCV@loadingMN[,2]
    rownames(P1_LOOCV)=colnames(x)
    rownames(P2_LOOCV)=colnames(x)
    
    #test for axis flip from original loadings vector, assign to loading matrix
    Load_LV1_LOOCV[i,]=P1_LOOCV * sign(sum(P1_LOOCV * oplsOut@loadingMN[,1]))
    Load_LV2_LOOCV[i,]=P2_LOOCV * sign(sum(P2_LOOCV * oplsOut@loadingMN[,2]))
  }   #end LOOCV opls iterations
  
  #compute mean and standard deviation for stored loadings over all iterations
  P1_LOOCV=base::colMeans(Load_LV1_LOOCV)
  P2_LOOCV=base::colMeans(Load_LV2_LOOCV)
  
  listOut=list()
  listOut$loadings <- oplsOut@loadingMN
  listOut$scores <- oplsOut@scoreMN
  listOut$loadings_LOOCV <- base::cbind(P1_LOOCV,P2_LOOCV)
  listOut$LV1_mat <- Load_LV1_LOOCV
  listOut$LV2_mat <- Load_LV2_LOOCV
  
  return(listOut)
}


#' Rotate opls_LOOCV ouput
#'
#' @description Rotates output from function opls.LOOCV by specified degrees and performs specified axis swap/flips
#' Computes and returns standard deviations of loadings across all LOOCV iterations
#' 
#' Ex. opls_LOOCV_out = opls_LOOCV(data,predI=2)
#' rotated_opls = rotate_opls_LOOCV(opls_LOOCV_out,30) # 30 degree rotation
#' @param opls_LOOCV_out object output from opls.LOOCV()
#' @param degrees default 0; specified degrees of clockwise rotation
#' @param swap default FALSE; set to TRUE to swap x and y axes
#' @param flip_y default FALSE; set to TRUE to flip data across y axis
#' @param flip_x default FALSE; set to TRUE to flip data across x axis
#' @return A list with the following objects:
#' T1 = scores in rotated LV1
#' T2 = scores in rotated LV2
#' P1 = loadings in rotated LV1
#' P2 = loadings in rotated LV2
#' P1_LOOCV = mean value of loadings in LV1, calculated across LOOCV iterations
#' P2_LOOCV = mean value of loadings in LV2, calculated across LOOCV iterations
#' sd_LV1_LOOCV = standard deviation of each loading in LV1, calculated across LOOCV iterations
#' sd_LV2_LOOCV = standard deviation of each loading in LV2, calculated across LOOCV iterations
#' rotmat = matrix of rotation by specified degrees
#' @export
rotate_opls_LOOCV <- function(opls_LOOCV_out,degrees=0,swap=F,flip_y=F,flip_x=F){
  #rotate by specified degrees
  theta=degrees*pi/180
  rotmat=rbind(c(cos(theta),-sin(theta)),c(sin(theta), cos(theta)))
  scores = opls_LOOCV_out$scores %*% rotmat
  T1=scores[,1]
  T2=scores[,2]
  loadings = opls_LOOCV_out$loadings %*% rotmat
  P1=loadings[,1]
  P2=loadings[,2]
  
  #rotate matrix of loadings from LOOCV
  P1_mat <- opls_LOOCV_out$LV1_mat
  P2_mat <- opls_LOOCV_out$LV2_mat
  for(i in 1:dim(opls_LOOCV_out$LV1_mat)[1]) {
    load_mat <- base::cbind(opls_LOOCV_out$LV1_mat[i,],opls_LOOCV_out$LV2_mat[i,])
    rot_load_mat <- load_mat %*% rotmat
    P1_mat[i,] <- rot_load_mat[,1]
    P2_mat[i,] <- rot_load_mat[,2]
  }
  
  #swap axis 1 and 2 if specified
  if(swap==TRUE){
    P1temp=P1; P2temp=P2
    P1_mat_temp = P1_mat; P2_mat_temp = P2_mat
    P1=P2temp; P2=P1temp
    P1_mat = P2_mat_temp; P2_mat = P1_mat_temp
    T1temp=T1; T2temp=T2
    T1=T2temp; T2=T1temp
  }
  
  #Flip axes 1 and/or 2 if specified
  if(flip_y==TRUE){
    P1=-P1
    P1_mat=-P1_mat
    T1=-T1
  }
  if(flip_x==TRUE){
    P2=-P2
    P2_mat=-P2_mat
    T2=-T2
  }
  
  P1_LOOCV=colMeans(P1_mat)
  names(P1_LOOCV)=names(P1)
  P2_LOOCV=colMeans(P2_mat)
  names(P2_LOOCV)=names(P2)
  stdevLV1=colSds(P1_mat) 
  stdevLV2=colSds(P2_mat)
  
  out <- list()
  out$T1 <- T1
  out$T2 <- T2
  out$P1 <- as.matrix(P1)
  out$P2 <- as.matrix(P2)
  out$P1_LOOCV <- as.matrix(P1_LOOCV)
  out$P2_LOOCV <- as.matrix(P2_LOOCV)
  out$sd_P1_LOOCV <- stdevLV1
  out$sd_P2_LOOCV <- stdevLV2
  out$rotmat <- rotmat
  return(out)
}

