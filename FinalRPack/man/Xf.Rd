\name{Xf}
\alias{Xf}
\docType{data}
\title{
The data frame of the data collected from the Geneva Health Bus study for females
}
\description{
Contains the the measured risk factors as well as the time (survey years/quarters) and SEP information for females aged 35-74 from the Geneva Health Bus study collected from 1993-2004.
}
\usage{data(Xf)}
\format{ 
  A data frame with 6,647 observations on the following 35 variables (note, exactly the same variables as in \code{Xm}):
  \describe{
    \item{\code{AGE}}{a numeric vector about the ages of the participants}
    \item{\code{SEXE}}{a numeric binary vector(1=male, 0=female)}
    \item{\code{SURVEY}}{a numeric vector about the actual survey year}
    \item{\code{Y_SURVEY}}{a numeric vector coded from survey year(1=1993, 2=1994,...12=2004)}
    \item{\code{YY_QUART}}{a numeric vectorcoded from survey quarter(1=1993Q1,2=1993Q2, 3=1993Q3, 4=1993Q4, 5=1994Q1, 6=1994Q2...47=2004Q3, 48=3004Q4)}
    \item{\code{NEDUC3}}{a numeric vector about the highest education level(0=HIGH, 1=MEDIUM, 2=LOW)}
    \item{\code{NEDUC3H}}{a dummy variable corresponding to \code{NEDUC3}(1=HIGH, 0 if not)}
    \item{\code{NEDUC3M}}{a dummy variable corresponding to \code{NEDUC3}(1=MEDIUM, 0 if not)}
    \item{\code{NEDUC3L}}{a dummy variable corresponding to \code{NEDUC3}(1=LOW, 0 if not)}
    \item{\code{ALL_UKB}}{a numeric vector about the highest occupation level(0=HIGH, 1=MEDIUM, 2=LOW)}
    \item{\code{ALL_UKBH}}{a dummy variable corresponding to \code{ALL_UKB}(1=HIGH, 0 if not)}
    \item{\code{ALL_UKBM}}{a dummy variable corresponding to \code{ALL_UKB}(1=MEDIUM, 0 if not)}
    \item{\code{ALL_UKBL}}{a dummy variable corresponding to \code{ALL_UKB}(1=LOW, 0 if not)}
    \item{\code{SMOKER3}}{a numeric vector about the cigarette smoking status(0=NON-SMOKER, 1=FORMER SMOKER, 2=CURRENT SMOKER)}
    \item{\code{SMOKNO}}{a dummy variable corresponding to \code{SMOKER3}(1=NON-SMOKER, 0 if not)}
    \item{\code{SMOKEX}}{a dummy variable corresponding to \code{SMOKER3}(1=FORMER SMOKER, 0 if not)}
    \item{\code{SMOKCU}}{a dummy variable corresponding to \code{SMOKER3}(1=CURRENT SMOKER, 0 if not)}
    \item{\code{PACKYRS}}{a continuous numeric vector about the number of pack-years=number of cigarette packs (of 20) x  number of years smoked}
    \item{\code{NOSPORT}}{a numeric binary vector(1=physically inactive, 0 if not)}
    \item{\code{SEDEN}}{a numeric binary vector(1=sedentary, 0 if not )}
    \item{\code{RPOLSAT}}{a numeric continuous vector about unsaturated-to-saturated dietary fat ratio}
    \item{\code{SFIBRG}}{a numeric continuous vector about dietary fiber taken as grams per day}
    \item{\code{TC}}{a numeric continuous vector about total cholesterol (mmol per L)}
    \item{\code{HTC}}{a numeric binary vector about hypercholesterolemia status (1=YES, 0=NO)}
    \item{\code{TT_CHOL}}{a numeric binary vector about high cholesterol treatment status (1=YES, 0= NO)}
    \item{\code{M_SBP}}{a numeric continuous vector about measured systolic blood pressure (mm per Hg)}
    \item{\code{M_DBP}}{a numeric continuous vector about measured diastolic blood pressure (mm per Hg)}
    \item{\code{MBP0}}{a numeric binary vector about moderate hypertension status (1=YES, 0=NO)}
    \item{\code{HBP}}{a numeric binary vector about severe hypertension status (1=YES, 0=NO)}
    \item{\code{TT_BP}}{a numeric binary vector about hypertension treatment status (1=YES, 0=NO)}
    \item{\code{BMI}}{a numeric vector of the measured BMI of the participants}
    \item{\code{RBMI}}{a numeric vector based on BMI but regression-calibrated}
    \item{\code{ROVERWT}}{a numeric binary vector based on a variable \code{RMI3} (in original dataset) that breaks up RBMI into the categories NORMAL, OVERWEIGHT, AND OBESE (1= OVERWEIGHT, 0 if not)}
    \item{\code{ROBESE}}{a numeric binary vector coded by \code{RBMI3} as \code{ROVERWT} is (1= OBESE, 0 if not)}
    \item{\code{INACTIVF}}{a numeric binary vector containing the inactivity data measured in the study from 1994-1996 coded in the variable \code{NOSPORT} in the orginal data set (1=physically inactive, 0 if not) combined with binary sedentarity data measured from 1997-2004 coded in the variable \code{SEDEN} in the original data set (1=sedentary, 0 if not)}
    }
}
\source{Michael C. Costanza, PEU member (retired 2007). \code{Xf} was prepared by extracting the desired columns from a larger data set used in the following study: \cr
Galobardes, Bruna, et al. "Trends In Risk Factors For Lifestyle-Related Diseases By Socioeconomic
Position In Geneva, Switzerland, 1993-2000: Health Inequalities Persist." American Journal of Public Health 93.8 (2003): 1302-1309.}
\examples{
data(Xf)
hist(Xf$BMI)
}
\keyword{datasets}