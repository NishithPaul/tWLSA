\name{chkOutMiss}
\alias{chkOutMiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checking outliers and missing values data matrix
%%  ~~function to do ... ~~
}
\description{Checking outliers and missing values data matrix
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chkOutMiss(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Metabolomics data matrix
%%     ~~Describe \code{x} here~~
}
}
\details{ This function will Check both the outliers and missing values data matrix
%%  ~~ If necessary, more details than the description above ~~
}
\value{Provide the information whether the data matrix contain both the outliers and missing values or not.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Nishith Kumar <nk.bru09@gmail.com>
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(dummyDataMiss)
chkOutMiss(dummyDataMiss)
}