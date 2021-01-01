\name{tWLSA-package}
\alias{tWLSA-package}
\alias{tWLSA}
\docType{package}
\title{Two way weighted least square approach for missing value imputation of metabolomics data 
\packageTitle{tWLSA}
}
\description{This package implements a Kernel weight based two way weighted least square approach for missing value imputation of metabolomics data  


\details{
  \tabular{ll}{
        Package: \tab tWLSA\cr
        Type: \tab Package\cr
        Version: \tab 1.0\cr
        Date: \tab 2020-12-14\cr
        License: \tab GPL \cr
        Depends: \tab R (>=2.10)\cr
    }
	

	Package tWLSA has the following functions:
    \tabular{ll}{
         chkMiss(): \tab This function check the missing values in the dataset \cr                                  \tab function.
\cr
         chkOutliers(): \tab This function Checks row wise outliers in a data matrix\cr                              \tab function.
\cr
         chkOutMiss(): \tab This function Checks both outliers and missing values data matrix\cr                              \tab function.
\cr
         missChkOut(): \tab This function clean the outliers\cr                              \tab function.
\cr

         wlsMisImp(): \tab This function impute the missing values using two way kernel weight based least square approach  \cr

	   
    }
}
\author{
Nishith Kumar\cr
Maintainer: Nishith Kumar <nk.bru09@gmail.com>}

\keyword{ package }
