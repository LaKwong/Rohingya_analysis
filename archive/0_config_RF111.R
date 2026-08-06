## SETUP
## EarthEnable child microactivity data
## Author: LHKwong

# # define options for project
#
# # define exposure of interest
# # options: "Arsenic", "Nitrate", "Perchlorate"
# # potential options for future research: "Uranium", "Chromium (VI)", "Gross Alpha","Selenium",
# # "Mercury", "Iron", "Manganese", "Vanadium","Chromium (Total)", "Fluoride", "Mercury"
#
# study_exposure <- "Nitrate"
#
# # define outcome of interest
#
# early_ptb      <- "prem_20_to_31"
# late_ptb       <- "prem_32_to_36"
# study_outcomes <- c("prem_20_to_31", "prem_32_to_36")


# load required packages

# import packages
library(haven)
library(readxl)

#  data tidying packages
library(zoo)
library(broom)
library(janitor) #tabyl() adorn_...()

library(magrittr)
library(purrr)

# table packages
library(tableone)

#install.packages("stargazer")
library(stargazer)
library(knitr)
library(kableExtra)

# modeling packages

library(survival)
library(estimatr)
# wow cool: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(jtools)
#install.packages("sandwich")
#library(sandwich)
library(lme4)
library(lmtest)
library(gee)

# other packages
library(sf)
library(RColorBrewer)
library(data.table)
library(lubridate)

library(ggpubr)
library(gtsummary)


# Load tidyverse last so it can overwrite things
library(tidyverse)

## This is for Chris
 here::set_here(path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/")

# # define coordinate reference systems
#
# # geographic CRS (for plotting points)
# crs_geo <- st_crs("+init=epsg:4269 +proj=longlat
#                       +ellps=GRS80 +datum=NAD83
#                       +no_defs +towgs84=0,0,0")  # NAD83, geographic data
#
# # projected CRS (for creating buffer)
# crs_projected <- st_crs("+proj=utm +zone=11 +datum=WGS84")


# define universal function

`%notin%` <- Negate(`%in%`)



# function to write csv output with specified folder, name, and current date
# input: object name and folder (ending in foward slash)
# output: saved cvs file in format: "studyexposure_objectname_date.csv" in specified folder
# note: can be substituted with use of here::here() function

write_output <- function(x, folder) {
  write_csv(x = x, path = paste(
    folder,
    study_exposure, "_",
    deparse(substitute(x)), "_",
    Sys.Date(), ".csv", sep = ""
  )
  )
}


#############################################################################
## Regression model
##############################################################################

# Assumptions for regression
# 1. All relationships are linear
# 2. Observations are independent (affects standard errors)
# -> in DID we are violating this because we are measuring the same ppl before and after
# - No consensus on how to best deal with this, but with large sample can do cluster-robust standard error
# 3. No perfect collinearity and non-zero variance of indep var
# 4. Error term has expected value of zero given any values of indep var (assume error is random) 
# - This is the "Parallel trends assumption" - trend would have been the same wihtout the treatment; --> if this does not hold then there is an endogeneity problem (the error varies with the outcome)
# 5. Error has equal variance given any val of indep var
# 6. Error term in normal distributed


did_fun <- function(df = survey_data, x) {
	did_model <- 
		lm_robust(
			data = df, 
			formula = x ~ timepoint + study_arm_overall + timepoint * study_arm_overall, 
			clusters = fcn_id
		)
	
	summary(did_model)
}

#############################################################################
## Robust standard errors for glm objects
##############################################################################


# This function estimates robust standad error for glm objects and
# returns coefficients as either logit, odd ratios or probabilities.
# logits are default
# argument x must be glm model.


# Credit to Achim here:
# http://stackoverflow.com/questions/27367974/
# different-robust-standard-errors-of-logit-regression-in-stata-and-r
# for the code in line 14 and 15

robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
	# suppressMessages(suppressWarnings(library(lmtest)))
	# suppressMessages(suppressWarnings(library(sandwich)))
	
	sandwich1 <- function(object, ...) sandwich(object) *
		nobs(object) / (nobs(object) - 1)
	# Function calculates SE's
	mod1 <- coeftest(x, vcov = sandwich1) 
	# apply the function over the variance-covariance matrix
	
	if (coef == "logit") {
		return(mod1) # return logit with robust SE's
	} else if (coef == "odd.ratio") {
		mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
		mod1[, 2] <- mod1[, 1] * mod1[, 2]
		return(mod1)
	} else {
		mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
		mod1[, 2] <- mod1[, 2]/4
		return(mod1)
	}
}


## Useful code
# # use the following code to get the output for all options for all vars
# mutate(across(.fns = as.character)) %>%
# 	pivot_longer(cols = everything(), names_to = "var") %>%
# 	count(var, value, name = 'count') %>%
# 	group_by(var) %>%
# 	mutate(N = prop.table(count) * 100)

#############################################################################
# Constants
#############################################################################
BDT_USD_exchange_rate <- 84.88 # as of 2 Nov 2020



# end
