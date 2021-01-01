\name{missChkOut}
\alias{missChkOut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Outliers Cleaning
%%  ~~function to do ... ~~
}
\description{ Checking Outliers using IQR rule and replaced it into the group median
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
missChkOut(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Metabolomics Data Matrix
%%     ~~Describe \code{x} here~~
}
}

\value{ Modified Metabolomics Data Matrix by cleaning Outliers
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Nishith Kumar <nk.bru09@gmail.com>
%%  ~~who you are~~
}


\examples{
data(dummyDataFull)
modiData<-missChkOut(dummyDataFull)
}