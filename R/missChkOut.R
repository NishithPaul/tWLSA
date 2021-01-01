missChkOut <-
function(x){
  
  if (!require(matrixStats, quietly = TRUE)) 
  stop("matrixStats package  is required")
  
  nummiss<-sum(is.na(x))

  iqR<-rowIQRs(x,na.rm=TRUE)
  quartile<-rowQuantiles(x,na.rm=TRUE)
  q1<-quartile[,2]
  q3<-quartile[,4]

dat<-x
for(i in 1:nrow(x)){
  for (j in 1:ncol(x)){
     if(!is.na(dat[i,j])&& (dat[i,j]>(q3[i]+iqR[i])||dat[i,j]<(q1[i]-iqR[i]))) dat[i,j]<-4/0
  }
}
out<-NULL
  if(sum(is.infinite(dat))<1 && nummiss <1){
   out<-0
    } else if(sum(is.infinite(dat))<1 && nummiss>0){
     out<-0
    } else if(sum(is.infinite(dat))>0 && nummiss<1){
    out<-1
    } else {
    out<-1
    }
return(out)
}
