chkOutliers <-
function(x){
  
  if (!require(matrixStats, quietly = TRUE)) 
  stop("matrixStats package  is required")
  
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

  if(sum(is.infinite(dat))<1){
    cat("There is no outlying cell in a data matrix\n")
    }
  else{cat("Dataset contain outlying cell\n")}
}
