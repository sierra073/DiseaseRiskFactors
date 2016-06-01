\name{AgeTest}
\alias{AgeTest} 
\title{Function to examine the age distribution in the male and female sample populations}
\description{This function computes the overall, occupational, and educational group-specific mean ages and their standard deviations, and then compares them with analyses of variance (ANOVA) to see whether age adjustment is necessary for the further trends analyses in the report.}
\usage{AgeTest()}

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xm)
data(Xf)
AgeTest()
}