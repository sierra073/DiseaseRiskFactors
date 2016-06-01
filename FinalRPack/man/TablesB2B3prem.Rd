\name{TablesB2B3prem}
\alias{TablesB2B3prem}
\title{Function to prepare the linear models for a risk factor variable in Tables B.2 and B.3 for men}
\description{This function prepares 4 linear regression models for an inputted risk factor variable from which information will be gathered to put in Tables B.2 and B.3 for that variable for men. 3 models fall into one type: a model predicting the variable over time in years, adjusted for age, is prepared for each occupation group: high, medium, and low, respectively. The p-value for the time slope (testing the null hypothesis that the time slope is 0) in each model will indicate whether the risk factor variable had a significant trend over time for that occupation group. The 4th linear regression model predicts the variable over time in years (adjusted for age) but also from the variable "ALL_UKB" (occupation level variable coded 0, 1 and 2) treated as a continous variable and its interaction with time. The p-value for the interaction slope (testing the null hypothesis that it is 0) will indicate whether there were significant trend differences between occupation groups.}
\usage{TablesB2B3prem(var,vartype)}
\arguments{
    \item{var}{The risk factor variable to be used as the dependent variable in the linear models.}
    \item{vartype}{Type of variable: 1 if discrete, 2 if continous, 3 if the variable requires a log-transform in its linear models, 4 if the variable is current smoker pack-years, and 5 if the variable is former smoker pack-years.}
}

\value{
    \item{firsts}{A 3x1 vector of the risk factor's (\code{var}'s) prevalences in 1993 for men with high, medium and low occupations, respectively. For the risk factor variable's row in Table B.2/B.3, each element of \code{firsts} is represented as the first ("1993") column's value under each occupation level.}
    \item{fit_h}{The linear model predicting \code{var} over time for men in the high occupation group. The slope for the time variable \code{Y_SURVEY} (in years) and its corresponding p-value are read into Table B.2/B.3 under the columns "Slope" and "Slope P" under high occupation.}
    \item{fit_m}{The linear model predicting \code{var} over time for men in the medium occupation group. The slope for the time variable \code{Y_SURVEY} (in years) and its corresponding p-value are read into Table B.2/B.3 under the columns "Slope" and "Slope P" under medium occupation.}
    \item{fit_l}{The linear model predicting \code{var} over time for men in the low occupation group. The slope for the time variable \code{Y_SURVEY} (in years) and its corresponding p-value are read into Table B.2/B.3 under the columns "Slope" and "Slope P" under low occupation.}
    \item{interp}{The linear model predicting \code{var} from which the interaction p-value is extracted. The p-value for \code{Y_SURVEY:ALL_UKB}'s slope is read into the risk factor variable's row in Table B.2/B.3 under the column "Interaction P".}
}

\author{Yun(Albee) Ling, Sierra Costanza, Bingjing Tang <yling1@jhu.edu, scostan1@jhu.edu, btang8@jhu.edu>}

\examples{
data(Xm)
TablesB2B3prem(Xm$SMOKCU,1)
}