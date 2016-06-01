\name{TablesB2B3m}
\alias{TablesB2B3m} 
\title{Function to display the numbers to put in Tables B.2 and B.3 for men}
\description{This function calls the function \code{TablesB2B3prem()} on a set of 18 risk factor variables and displays the prevalences in 1993 by occupation group and the summaries of each of the 4 linear model outputs (all from the function \code{TablesB2B3prem()}) on the screen for each variable. The output of this function is a list of 18 objects, each of which contains the set of 5 resultant output objects from \code{TablesB2B3prem()}. \bold{The objects' results used for Table B.2 (in the following order) were \code{three-six}, and the results used for Table B.3 were \code{nine}, \code{ten}, \code{twelve}, \code{thirt}, and \code{fift-eightt}}.}
\usage{TablesB2B3m()}

\value{
    \item{one}{The 1993 prevalences and linear models for current smokers}
    \item{two}{The 1993 prevalences and linear models for current-smoked pack-years}
    \item{three}{The 1993 prevalences and linear models for former smokers}
    \item{four}{The 1993 prevalences and linear models for formerly-smoked pack-years}
    \item{five}{The 1993 prevalences and linear models for physical inactivity}
    \item{six}{The 1993 prevalences and linear models for unsaturated-to-saturated fat ratio}
    \item{seven}{The 1993 prevalences and linear models for dietary fiber}
    \item{eight}{The 1993 prevalences and linear models for total cholesterol}
    \item{nine}{The 1993 prevalences and linear models for hypercholesterolemia}
    \item{ten}{The 1993 prevalences and linear models for cholesterol treatment}
    \item{eleven}{The 1993 prevalences and linear models for systolic blood pressure}
    \item{twelve}{The 1993 prevalences and linear models for diastolic blood pressure}
    \item{thirt}{The 1993 prevalences and linear models for moderate hypertension}
    \item{fourt}{The 1993 prevalences and linear models for severe hypertension}
    \item{fift}{The 1993 prevalences and linear models for hypertension treatment}
    \item{sixt}{The 1993 prevalences and linear models for regression-calibrated BMI}
    \item{sevent}{The 1993 prevalences and linear models for overweight}
    \item{eightt}{The 1993 prevalences and linear models for obesity}
}

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xm)
tB2B3m=TablesB2B3m()
}
