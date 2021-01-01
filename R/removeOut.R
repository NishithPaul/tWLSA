removeOut <-
function(x,group=1,nGrpMem=ncol(x)){
if (group!=length(nGrpMem))
stop("Please Enter the Individual Column Number of Each Group")

grwdat<-NULL
nCol<-0
for (i in 1:group){
ncol1<-nCol+1
nCol<-nCol+nGrpMem[i]
grwdat[[i]]<-x[,ncol1:nCol]
}

cleandat<-function(x){
  if (!require(matrixStats, quietly = TRUE)) 
  stop("matrixStats package  is required")

iqR<-rowIQRs(x,na.rm=TRUE)
quartile<-rowQuantiles(x,na.rm=TRUE)
q1<-quartile[,2]
q2<-quartile[,3]
q3<-quartile[,4]

dat<-x

for(i in 1:nrow(x)){
  for (j in 1:ncol(x)){
  if(!is.na(dat[i,j])&& (dat[i,j]>(q3[i]+iqR[i])||dat[i,j]<(q1[i]-iqR[i]))) dat[i,j]<-rnorm(1,q2[i])
  }
}
return(dat)
}

combine<-NULL
grwdatP<-NULL
for (i in 1:group){
converged<-2
grwdatP[[i]]<-cleandat(grwdat[[i]])
while (converged>0){
converged<-missChkOut(grwdatP[[i]])
grwdatP[[i]]<-cleandat(grwdatP[[i]])
}
combine<-cbind(combine,grwdatP[[i]])
}
return(combine)
}
