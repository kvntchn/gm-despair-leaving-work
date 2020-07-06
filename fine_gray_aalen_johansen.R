# Estimating Cumulative Incidence Functions
# Aalen-Johansen &
# Fine & Gray
# Origin: 12/3/2017

install.packages("cmprsk")
library(cmprsk)
library(dplyr)

### FUNCTIONS ######
# The Cuminc() function in the mstate package calculates non-parametric
# CIF (aka Aalen–Johansen estimates) and associated standard errors for the competing events.
####################

# Modifying dataset for analysis
fgdta <- data %>% mutate(failuretime = yod09 - YOUT16, fstatus = as.factor(
	ifelse(suicide == 1, 1, ifelse(cancer == 1, 2, ifelse(all.cause.nsnc == 1,3,0))))) %>% select(STUDYNO, SEX, ageHire, PLANT, job.cat, ndays.off.total, race, retire.early, failuretime, fstatus)

#### Aalen-Johansen
ci_aj <- survfit(Surv(failuretime, fstatus, type = "mstate") ~ retire.early, data = fgdta)
summary(ci_aj, times = c(1,2,5,10)) # differences reported in Overleaf
summary(ci_aj, times = c(1,2,5,10))$pstate[5:8,1] - summary(ci_aj, times = c(1,2,5,10))$pstate[1:4,1]


#### FINE & GRAY MODEL
# The subdistribution for type 1 failures for two covariates Z_i = (Z_{i1}, Z_{i2})
# Pr(T_i \leq t, \epsilon_i = 1 | Z_i) = 1 - [1-p\{1-exp(-t)\}]^{exp(Z_i\beta_{11} + Z_{i2}\beta_{12})}

cens <- rep(0,nrow(fgdta))
cens[fgdta$fstatus == 2 | fgdta$fstatus == 3] <- 2
cens[fgdta$fstatus == 1] <- 1
fg1 <- cuminc(fgdta$failuretime, fstatus = cens, group = fgdta$retire.early, strata = c(fgdta$job.cat), cencode = 2 )
fg1$Tests # CI curves are statistically different

#### ESTIMATES ####
timepoints(fg1,c(1,2,5,10))$est[4,] - timepoints(fg1,c(1,2,5,10))$est[3,] # "ATE" estimates of Fine & Gray

#### The point estimates don't change based on strata, but the variance does (obvi).
fg2 <- cuminc(fgdta$failuretime, fstatus = cens, group= fgdta$retire.early, strata = c(fgdta$PLANT), cencode = 2)
timepoints(fg2,c(1,2,5,10))
