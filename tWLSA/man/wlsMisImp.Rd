\name{wlsMisImp}
\alias{wlsMisImp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Two way kernel weight based least square approach for missing value imputation
%%  ~~function to do ... ~~
}
\description{Two way kernel weight based least square approach for missing value imputation
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wlsMisImp(x, lamda = 0.1, conRate = 99, group = 1, noGrpMem = c(ncol(x)), outlier = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Metabolomics Data Matrix
%%     ~~Describe \code{x} here~~
}
  \item{lamda}{ Tuning parameter. Default is 0.1
%%     ~~Describe \code{lamda} here~~
}
  \item{conRate}{ Percent of variation explained by the modiffied dataset. Default is 99.
%%     ~~Describe \code{conRate} here~~
}
  \item{group}{ Number of group in the dataset. Default is 1.
%%     ~~Describe \code{group} here~~
}
  \item{noGrpMem}{ Number of subject of each group.
%%     ~~Describe \code{noGrpMem} here~~
}
  \item{outlier}{ Outlier condition TRUE/FALSE.
%%     ~~Describe \code{outlier} here~~
}
}

\value{Reconstructed data matrix after inputing missing values
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{ Nishith Kumar<nk.bru09@gmail.com>
%%  ~~who you are~~
}

\examples{
data(dummyDataMiss)
recData<-wlsMisImp(dummyDataMiss)
}