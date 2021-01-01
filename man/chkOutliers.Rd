\name{chkOutliers}
\alias{chkOutliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checking row wise outliers in a data matrix
%%  ~~function to do ... ~~
}
\description{This function check the row-wise outliers in the data matrix using IQR rule. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chkOutliers(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Metabolomics data matrix
%%     ~~Describe \code{x} here~~
}
}
\details{Checking the row-wise outliers in the data matrix using IQR rule
%%  ~~ If necessary, more details than the description above ~~
}
\value{ Provide the information whether the data matrix contain outliers or not.
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
chkOutliers(dummyDataFull)
}