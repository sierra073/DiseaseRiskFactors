\name{TableB1prem}
\alias{TableB1prem} 
\title{Function to produce the numbers to put in Table B.1 for a risk factor variable for men}
\description{This function computes the prevalences (or in some cases means) of each of the risk factor variables for men over the entire survey period by occupation group. The function also performs pairwise t-tests using R's built-in \code{pairwise.t.test()} function on the prevalnces/means for each occupation group to see if they differed significantly between occupation groups. The output is a list of the prevalences/means by occupation group and the pairwise t-test result (stored as an object).}
\usage{TableB1prem(var,type)}
\arguments{
    \item{var}{The risk factor variable}
    \item{type}{Type of variable: 1 if current smoker pack-years, 2 if former smoker pack-years, 3 if discrete, 0 if continuous}
}

\value{
    \item{a}{the prevalence/mean of the risk factor variable for high occupation groups}
    \item{b}{the prevalence/mean of the risk factor variable for medium occupation groups}
    \item{c}{the prevalence/mean of the risk factor variable for low occupation groups}
    \item{d}{the pairwise t-test results (object)}
}

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xm)
TableB1prem(Xm$SMOKCU,3)
}
