\name{removeOut}
\alias{removeOut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Data cleaning
%%  ~~function to do ... ~~
}
\description{ Checking Outliers using IQR rule and replaced it into the group median
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
removeOut(x, group = 1, nGrpMem = ncol(x))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Metabolomics Data Matrix
%%     ~~Describe \code{x} here~~
}
  \item{group}{ No of group in the Data matrix
%%     ~~Describe \code{group} here~~
}
  \item{nGrpMem}{ number of sbuject in each group
%%     ~~Describe \code{nGrpMem} here~~
}
}


\author{Nishith Kumar <nk.bru09@gmail.com>
%%  ~~who you are~~
}

\examples{
data(dummyDataFull)
modiData<-removeOut(dummyDataFull,group=2,nGrpMem=c(28,17))
}

