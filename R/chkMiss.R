chkMiss <-
function(x){
  if(sum(is.na(x))<1){
    cat("There is no missing cell in a data matrix\n")
    }
  else{cat("Dataset contain missing values\n")}
}
