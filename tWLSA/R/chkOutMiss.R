chkOutMiss <-
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

  if(sum(is.infinite(dat))<1 && nummiss <1){
    cat("There is no outliers and  missing values in a data matrix\n")
    } else if(sum(is.infinite(dat))<1 && nummiss>0){
    cat("Dataset contains missing values but no outliers\n")
    } else if(sum(is.infinite(dat))>0 && nummiss<1){
    cat("Dataset contains outliers but no missing values\n")
    } else {
    cat("Dataset contains both outliers and missing values\n")
    }
}
