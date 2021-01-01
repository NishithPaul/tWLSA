wlsMisImp <-
function(x,lamda=0.1,conRate=99,group=1,noGrpMem=c(ncol(x)),outlier=TRUE){

if (group!=length(noGrpMem))
stop("Please Enter the Individual Column Number of Each Group")

grwdat<-NULL
nCol<-0
for (i in 1:group){
ncol1<-nCol+1
nCol<-nCol+noGrpMem[i]
grwdat[[i]]<-x[,ncol1:nCol]
}

wlsImp<-function(x,lambda=lamda,contRate=conRate){

  origdat<-x

  if (!require(matrixStats, quietly = TRUE)) 
  stop("matrixStats package  is required")

###############################################

  iqR<-rowIQRs(x,na.rm=TRUE)
  quartile<-rowQuantiles(x,na.rm=TRUE)
  q1<-quartile[,2]
  q3<-quartile[,4]

dat<-x
for(i in 1:nrow(x)){
  for (j in 1:ncol(x)){
  if(!is.na(dat[i,j])&& (dat[i,j]>(q3[i]+iqR[i])||dat[i,j]<(q1[i]-iqR[i]))) dat[i,j]<-runif(1,(q1[i]-iqR[i]),(q3[i]+iqR[i]))
  }
}
x<-dat
###############################################
  wmean<-function(x){
    mu<-median(x,na.rm=TRUE)
    sig<-mad(x,na.rm=TRUE)
    n<-length(x)
    wt<-0
    summu <- 0
    swt<- 0
    for (i in 1:n)
      {
        if (is.na(x[i])==FALSE && is.finite(x[i])==TRUE){
          wt<-exp(-(lambda*(sig^-2)*(x[i]-mu)^2)/2)
          if (is.na(wt)==FALSE && is.finite(wt)==TRUE){
            swt<-swt+wt
            summu<- summu+wt*x[i] 
           }
          
        }
      }
  
    if (swt > 0){
    mu<-summu/swt
    }
    else{
      mu<-weightedMedian(x,na.rm=TRUE)
    }
    return(mu)
  }

################################################

  tune<-lambda
  wRegCoef <- function(x, a) {
        bt<- tune
        sig<- mad(x,na.rm=TRUE)
        p<- median(x,na.rm=TRUE)
        wt<-0
        sumwt<- 0
        tswt<- 0
        r<-0
        for (i in 1:length(x)){
          if (is.na(x[i])==FALSE & a[i]!=0)
            {
             wt<- exp(-(bt*sig^-2*(x[i]-p)^2)/2)*a[i]^2
             sumwt<- sumwt + wt
             tswt<- tswt+ wt*x[i]/a[i]
            }  
          }
         if (is.na(sumwt)==FALSE & (sumwt>0)){
           p<-tswt/sumwt
         }
         else {
           p<-wmean(x/a)
         }
        return(p)
    }


    svdu <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
    svdv <- matrix(NA, nrow = ncol(x), ncol = ncol(x))
    svdd <- rep(NA, ncol(x))

    for (k in 1:ncol(x)) {
        ak <- apply(x, 1, wmean)
        converged <- 1
        l<-0
        while (converged > 0.01) {
            akprev <- ak
            c <- apply(x, 2, wRegCoef, ak)
            bk <- c/sqrt(sum(c^2))
            d <- apply(x, 1, wRegCoef, c)
            ak<-d
            akf <- d/sqrt(sum(d^2))
            if (is.na(sum((ak - akprev)^2))==FALSE && is.finite(sum((ak - akprev)^2))==TRUE){
              converged <-sum((ak - akprev)^2)
            }
           #if (converged1>converged)
          l<-l+1
          if(l>3){converged =0} 
        }

        eigenk <- sqrt(sum(c^2))*sqrt(sum(d^2))
        approx<-eigenk * akf %*% t(bk)

        x <- x - eigenk * akf %*% t(bk)
        svdu[, k] <- akf
        svdv[, k] <- bk
        svdd[k] <- eigenk
    }
    
  d <- svdd
  u <- svdu
  v <- svdv

#############################
###                       ###
#############################
    d[is.na(d)]<-0
    u[is.na(u)]<-0
    v[is.na(v)]<-0

    sm<-0
    tsum<-sum(d*d)
    expVar<-NULL
    ik<-0
    targetVar<-FALSE
    while(!targetVar){
    ik=ik+1
    sm=sm+d[ik]*d[ik]
    expVar[ik]=(sm/tsum)*100
    if (expVar[ik]>= contRate){
      targetVar <- TRUE
    }

    if (ik>= (min(nrow(x),ncol(x))/2)){
      targetVar <- TRUE
    }
  }

  if(ik<2){
    approX<-(d[1]*u[,1])%*%t(v[,1])
  }
  if(ik>1){
    approX<-u[,1:ik]%*%diag(d[1:ik])%*%t(v[,1:ik])
  }


  iqR<-rowIQRs(origdat,na.rm=TRUE)
  quartile<-rowQuantiles(origdat,na.rm=TRUE)
  q1<-quartile[,2]
  q3<-quartile[,4]

  recmat<-origdat

if (outlier){
  for(i in 1:nrow(origdat)){
  for (j in 1:ncol(origdat)){
    if (is.na(recmat[i,j])|| recmat[i,j]> (q3[i]+iqR[i])|| recmat[i,j]< (q1[i]-iqR[i])) recmat[i,j]<-approX[i,j]
  }
}
} else{
  for(i in 1:nrow(origdat)){
  for (j in 1:ncol(origdat)){
    if (is.na(recmat[i,j])) recmat[i,j]<-approX[i,j]
  }
}
}
 
  imputeDat<-list()
  imputeDat$x<-recmat
  imputeDat$u<-u
  imputeDat$v<-v
  imputeDat$d<-d
  return(imputeDat)
}

combine<-NULL
grwdatP<-NULL
for (i in 1:group){
grwdatP[[i]]<-wlsImp(grwdat[[i]])$x
combine<-cbind(combine,grwdatP[[i]])
}
return(combine)
}
