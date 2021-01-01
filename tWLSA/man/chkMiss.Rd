\name{chkMiss}
\alias{chkMiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checking Missing cell in a data matrix
%%  ~~function to do ... ~~
}
\description{ This function will check whether the data matrix contains missing values or not.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chkMiss(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Metabolomics Data Matrix, whose row contains metabolites and column contains subjects
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Provide message about the missing values in the data matrix
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
chkMiss(dummyDataMiss)
}
