\name{TableB1pref}
\alias{TableB1pref} 
\title{Function to produce the numbers to put in Table B.1 for a risk factor variable  for women}
\description{This function is identical to the function \code{TableB1prem()} except it is called on variables from the \code{Xf} (female) data set.}
\usage{TableB1pref(var,type)}
\arguments{
    \item{var}{The risk factor variable}
    \item{type}{Type of variable: 1 if current smoker pack-years, 2 if former smoker pack-years, 3 if discrete, 0 if continuous}
}

\value{
    \item{a}{the prevalence/mean of the risk factor variable for high occupation groups}
    \item{b}{the prevalence/mean of the risk factor variable for medium occupation groups}
    \item{c}{the prevalence/mean of the risk factor variable for low occupation groups}
    \item{d}{the pairwise t-test results (object)}}

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xf)
TableB1pref(Xf$SMOKCU,3)
}
