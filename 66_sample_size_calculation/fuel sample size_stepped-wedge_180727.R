# Sample size for fuel instead of fuelwood proposal

library(swCRTdesign)


n4incidence(
  le, lc, m, t, CV, 
  alpha = 0.05, power = 0.80, AR = 1, 
  two.tailed = TRUE, digits = 3
)

# le The anticipated incidence rate, ??E, in the experimental group with the outcome.
# lc The anticipated incidence rate, ??C , in the control group with the outcome.
# m The anticipated average (or actual) cluster size.
# t The planned follow-up time for the study (in weeks, months, etc.)
# CV The coefficient of variation, assumed constant over both the treatment and control
# groups. Note that CV = ??1/??E = ??2/??C , where ??E and ??C represent the
# between-cluster variation in incidence rates for each group.
# AR The Allocation Ratio: AR=1 implies an equal number of subjects per treatment
# and control group (maximum efficiency), > 1, implies more subjects will be
# enrolled in the control group (e.g. in the case of costly intervention), < 1 implies
# more subjects in the tretment group (rarely used).
# alpha The desired type I error rate.
# power The desired level of power, recall power = 1 - type II error.
# two.tailed Logical, If TRUE calculations are based on a two-tailed type I error, if FALSE,
# a one-sided calculation is performed.
# digits Number of digits to round calculations

n4incidence(
  le = 0.55/2, lc = 0.55, m = 30, t = 2, CV = (((0.611 - 0.501)/4) / 0.55), 
  alpha = 0.05, power = 0.80, AR = 1, 
  two.tailed = TRUE, digits = 3
)
# Need 12 clusters of 30 hh in each arm = 12*30*2
# 24 clusters over 18 months -> 2 clusters per month will give us 36 clusters



# For the mask study
# Assume distribution of masks for 6 weeks to the entire camp and that masks are distributed by block. 
# Assume 20 camps, each with 7 blocks -> 140 blocks total
# There are approximately

swDsn(clusters = c(10, 20, 40, 70), tx.effect.frac = 1, extra.time = 0, all.ctl.time0 = FALSE)


# tx.effect.frac	
# numeric (scalar or vector): Fractional treatment effect upon crossing over from control. 
# Note that this is not the treatment effect! If a scalar with value of 1, the standard SW CRT treatment effect will be presumed. 
# If a scalar with a fractional value between 0 and 1, then only the first time point upon crossing over from control will have 
# fractional treatment effect; the remaining time points in SW CRT design will have value of 1. If a vector of fractional treatment 
# effect is specified, each element of the vector corresponds to the (fractional) treatment effect upon crossing over from control; 
# if length of vector less than total number of time points after crossing over, the remaining time points will have 
# treatment effect value of 1; if length of vector greater than total number of time points after crossing over, 
# not all elements of vector will be used. The default value is (scalar) 1.


# swDsn Pwr uses the Hussey and Hughes 2007 method for calculating power
# Applies to a cross-setional stepped-wedged trial

# Guess and check m1 until power is 80%

# swPwr.icccac <- 
swPwr(
  design = 
    swDsn(
      clusters = c(rep(14, 12)), 
      tx.effect.frac = 0.8, # Fractional treatment effect upon crossing over from control. if tx.effect.frac =1 then the crossover effect is the treatment effect
      # Since more and more people are getting masks, I assumed that crossing over only has 80% of the total effect. 
      extra.time = 0, 
      all.ctl.time0 = FALSE # some blocks already have masks, so while they may not have 100% coverage, I'll set not all blocks as control to start with
      ),
  distn="binomial",
  n = 50, # 100 individuals ber block; this is only 10 households
  mu0 = 0.20, 
  mu1 = 0.20*(1-0.13), 
  # eta = NA, # standard deviation of random treatment effects
  # rho = NA, # correlation between random intercepts and random treatment effects
  # gamma = NA, # standard deviation of random time effects
  # sigma = NA, 
  icc = 0.1, 
  cac = 0.125, # Complete guess
  alpha = 0.05,
  retDATA = FALSE
)

# With 100 individuals randomly selected from each of the 172 blocks measured each week, 
# there is 80.4% power to detect a 12.5% reduction in symptoms

# With 50 individuals randomly selected from each of the 172 blocks measured each week, 
# there is 77.6% power to detect a 12.5% reduction in symptoms

# With 50 individuals randomly selected from each of the 172 blocks measured each week, 
# there is 80.7% power to detect a 13% reduction in symptoms

# This may be only 10 households/block but drawing 50 individuals from 10 households does not account for clustering by hh 
# The survey takes only 7-10 minutes so a 30 hh could be done in a day. 
