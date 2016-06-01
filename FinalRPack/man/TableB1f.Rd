\name{TableB1f}
\alias{TableB1f} 
\title{Function to display the numbers to put in Table B.1 for women}
\description{This function is identical to the function \code{TableB1m()} except it calls the function \code{TableB1pref()} on variables from the \code{Xf} (female) data set.}
\usage{TableB1f()}

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
data(Xf)
t1f=TableB1f()
}
