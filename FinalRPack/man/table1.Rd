\name{table1}
\alias{table1}
\title{A function to produce the data for Table 1}
\description{\code{table1()} is a function that computes the distribution of socioeconomic position among men (n=6,688) and women (n=6,647) aged 35-74 years by year of survey.
    }
\usage{table1(data)}
\arguments{
    \item{data}{A data frame of the data collected by Health Bus Project for either male or female participants, including information on occupation and eduction levels}
}

\value{
   \item{table1}{A data frame containing the numbers and the percentages of people in each of the three categories of education and occupation levels by survey year. Depending on whether female or male data is used, the function will output a summary for female or male participants.}
   }

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xm)
data(Xf)
table1(Xm)
table1(Xf)
}
