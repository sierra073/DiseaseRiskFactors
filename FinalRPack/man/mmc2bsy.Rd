\name{mmc2bsy-package}
\alias{mmc2bsy-package}
\docType{package}

\title{Final Project R Package}
\description{
Chronic diseases are the leading cause of death in developed countries today. In order to evaluate the impact of biological and behavioral risk factors affecting lifestyle-related chronic diseases, we look at the trends in risk factors for lifestyle-related diseases by socioeconomic position from data collected in Geneva, Switzerland from 1993 to 2004. This R package contains two data files and sixteen functions used for our Final Project. Xf.Rd is data for women and Xm.Rd for men. 

Below are the R functions in this package
\enumerate{
\item \code{AgeTest} computes the overall, occupational, and educational group-specific mean ages and their standard deviations, and then compares them with analyses of variance (ANOVA) to see whether age adjustment is necessary for the further trends analyses in the report.
\item \code{FigC1}  plots a histogram of the currently-smoked pack-years among medium occupation women shown in Figure C.1 in the Model Selection section of the final report.
\item \code{Diagnostic} builds 4 linear models predicting the current smoker pack-years for medium SEP women to evaluate in the Model Selection section of the final report.
\item \code{FigB1} plots the age-adjusted trends in overweight prevalence in men amidst seasonal and sampling background fluctuations by occupation groups shown in Figure B.1.
\item \code{FigB2} plots the age-adjusted trends in hypercholesterolemia prevalence in men amidst seasonal and sampling background fluctuations by occupation groups shown in Figure B.2.
\item \code{FigB3} plots the age-adjusted trends in overweight prevalence in women amidst seasonal and sampling background fluctuations by occupation groups shown in Figure B.3. 
\item \code{FigB4} plots the age-adjusted trends in hypercholesterolemia prevalence in women amidst seasonal and sampling background fluctuations by occupation groups shown in Figure B.4.
\item \code{table1} summarizes numerically the distributions socioeconomical positions (education and occupation respectively) for 6688 males and 6647 females by survey year 1993-2004.
\item \code{TableB1f} is identical to the function \code{TableB1m()} except it calls the function \code{TableB1pref()} on variables from the \code{Xf} (female) data set.
\item \code{TableB1m} calls the function \code{TableB1m()} on each risk factor variable listed in Table B.1.
\item \code{TableB1pref} is identical to the function \code{TableB1prem()} except it is called on variables from the \code{Xf} (female) data set.
\item \code{TableB1prem} computes the prevalences (or in some cases means) of each of the risk factor variables for men over the entire survey period by occupation group.
\item \code{TableB2B3f} calls the function \code{TablesB2B3pref()} on a set of 18 risk factor variables and displays the prevalences in 1993 by occupation group and the summaries of each of the 4 linear model outputs (all from the function \code{TablesB2B3pref()}) on the screen for each variable.
\item \code{TableB2B3m} calls the function \code{TablesB2B3prem()} on a set of 18 risk factor variables and displays the prevalences in 1993 by occupation group and the summaries of each of the 4 linear model outputs (all from the function \code{TablesB2B3prem()}) on the screen for each variable.
\item \code{TableB2B3pref} is identical to the function \code{TablesB2B3prem()} except it is called on variables from the \code{Xf} (female) data set.
\item \code{TableB2B3prem} prepares 4 linear regression models for an inputted risk factor variable from which information will be gathered to put in Tables B.2 and B.3 for that variable for men.
}

The top-level structure of the folder looks like
\preformatted{
-- mmc2bsy
     |
      -DESCRIPTION
      -NAMESPACE
      -data/
          |
          -Xm.RData
          -Xf.RData
      -R/
          |
          -table1.R
          -TableB2B3prem.R
          -TableB1pref.R
          -TableB1prem.R
          -TableB1m.R
          -TableB1f.R
          -Diagnostic.R
          -TableB2B3pref.R
          -TableB2B3m.R
          -TableB2B3f.R
          -FigB4.R
          -FigB3.R
          -FigB2.R
          -FigB1.R
          -DHist.R
          -AgeTest.R
      -man/
         |
         -mmc2bsy.Rd
         -table1.Rd
         -TableB2B3prem.Rd
         -Xf.Rd
         -TableB1pref.Rd
         -TableB1prem.Rd
         -TableB1m.Rd
         -TableB1f.Rd
         -Diagnostic.Rd
         -Xm.Rd
         -TableB2B3pref.Rd
         -TableB2B3m.Rd
         -TableB2B3f.Rd
         -FigB4.Rd
         -FigB3.Rd
         -FigB2.Rd
         -FigB1.Rd
         -DHist.Rd
         -AgeTest.Rd
      -inst/
         |
         -doc/
             |
             -figure/
                 |
                 -dhist.png
                 -plot2f.png
                 -plot1m.png
                 -plot1f.png
                 -plot2m.png
             }
      -mmc2bsy.Rmd
}

\examples{
data(Xf)  
data(Xm)  
table1(Xf)  
Diagnostic()
}
