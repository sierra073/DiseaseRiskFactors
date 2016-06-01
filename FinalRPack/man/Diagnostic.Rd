\name{Diagnostic}
\alias{Diagnostic}
\title{A function that performs model diagnostics: produces Tables C.1 - C.4}
\description{This function builds 4 linear models predicting the current smoker pack-years for medium SEP women. Models 1 and 2 (Tables C.1-C.2) predict the raw pack-years data from time and occupation, and time and education, respectively. Models 3 and 4 (Tables C.3-C.4) predict the log of the pack-years data from time and occupation, and time and education, respectively. Model 3 is the model actually used in the analysis (Table B.2), which is the model being diagnosed. All model summaries are printed on the screen (using the \code{xtable()} function) for comparison, as well as each model's AIC score.
    }
\usage{Diagnostic()}

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xf)
Diagnostic()
}