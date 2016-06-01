\name{TableB1m}
\alias{TableB1m} 
\title{Function to display the numbers to put in Table B.1 for men}
\description{The function calls the function \code{TableB1m()} on each risk factor variable listed in Table B.1. It outputs and displays on the screen a list of the results of \code{TableB1m()} (in an object) called on each variable. By \code{TableB1m()}, 4 resulting objects will be displayed per item in the output list of this function.}
\usage{TableB1m()}
\value{
    \item{one}{The prevalences and t-test p-values for current smokers}
    \item{two}{The prevalences and t-test p-values for current-smoked pack-years}
    \item{three}{The prevalences and t-test p-values for former smokers}
    \item{four}{The prevalences and t-test p-values for formerly-smoked pack-years}
    \item{five}{The prevalences and t-test p-values for physical inactivity}
    \item{six}{The prevalences and t-test p-values for unsaturated-to-saturated fat ratio}
    \item{seven}{The prevalences and t-test p-values for dietary fiber}
    \item{eight}{The prevalences and t-test p-values for total cholesterol}
    \item{nine}{The prevalences and t-test p-values for hypercholesterolemia}
    \item{ten}{The prevalences and t-test p-values for cholesterol treatment}
    \item{eleven}{The prevalences and t-test p-values for systolic blood pressure}
    \item{twelve}{The prevalences and t-test p-values for diastolic blood pressure}
    \item{thirt}{The prevalences and t-test p-values for moderate hypertension}
    \item{fourt}{The prevalences and t-test p-values for severe hypertension}
    \item{fift}{The prevalences and t-test p-values for hypertension treatment}
    \item{sixt}{The prevalences and t-test p-values for regression-calibrated BMI}
    \item{sevent}{The prevalences and t-test p-values for overweight}
    \item{eightt}{The prevalences and t-test p-values for obesity}
}


\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xm)
t1m=TableB1m()
}
