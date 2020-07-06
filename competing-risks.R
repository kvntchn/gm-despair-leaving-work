# Estimating Cumulative Incidence Functions
# Aalen-Johansen &
# Fine & Gray
# Origin: 12/3/2017
# Original code receieved from
# Suzanne and Ivana on February 24, 2020
# Adapated by Kevin Chen
library(survival)
library(cmprsk)
library(mstate)
library(dplyr)

# rm(list = ls()[-which(grepl('cohort_full|cohort_ips', ls()))])
if (!grepl("gm", getwd(), ignore.case = T)) {
	if ("here" %in% .packages()) {
		detach("package:here", unload = T)
	}
	setwd('gm')
	library(here)
}

source(here::here("age-worker-exit.R"))

get.coxph(recent = T, rebuild_cohort_leftwork = T, run_model = F)

### FUNCTIONS ######
# The cuminc() function in the mstate package calculates non-parametric
# CIF (aka Aalen–Johansen estimates) and associated standard errors for the competing events.
####################

# Modifying dataset for analysis
setorder(cohort_leftwork_recent, studyno, year)
cohort_leftwork_recent[,`:=`(
	I = 1:.N,
	N = .N
), by = .(studyno)]

fgdta <- cohort_leftwork_recent[I == N & sex == "M", .(
	studyno,
	year,
	since.leavework2,
	age.year2,
	fstatus = {
		tmp <- `All causes`
		tmp[tmp == 1] <- 3
		tmp[Suicide == 1] <- 1
		tmp[`All cancers` == 1
					# `Chronic obstructive pulmonary disease` == 1 |
					# `All heart diseases` == 1
				] <- 2
		factor(tmp)
	},
	sex,
	hire.age = time_length(difftime(yin, yob), "year"),
	plant,
	jobloss.age.cat = factor(
		jobloss.age.cat,
		levels = rev(levels(cohort_leftwork_recent$jobloss.age.cat))),
	# jobloss.age.cat = factor(
	# 	jobloss.age.cat == "55 years or older",
	# 	levels = c("TRUE", "FALSE"),
	# 	labels = c("55 years or older", "under 55")),
	yob, yin, jobloss.date,
	yod, yoc,
	`All cancers`,
	`Chronic obstructive pulmonary disease`,
	`All heart diseases`,
	Suicide,
	Overdose,
	Despair = Suicide + Overdose
)]

#### Aalen-Johansen
ci_aj <- survfit(
	Surv(since.leavework2,
			 event = fstatus,
			 type = "mstate") ~
		jobloss.age.cat,
	data = fgdta)

summary(ci_aj, times = c(1, 2, 5, 10) * 365)

summary(ci_aj, times = c(1, 2, 5, 10) * 365)$pstate[5:8, 2] -
	summary(ci_aj, times = c(1, 2, 5, 10) * 365)$pstate[1:4, 2]

summary(ci_aj, times = c(1, 2, 5, 10) * 365)$pstate[9:12, 2] -
	summary(ci_aj, times = c(1, 2, 5, 10) * 365)$pstate[1:4, 2]

summary(ci_aj, times = c(1, 2, 5, 10) * 365)$pstate[13:16, 2] -
	summary(ci_aj, times = c(1, 2, 5, 10) * 365)$pstate[1:4, 2]

#### FINE & GRAY MODEL
# The subdistribution for type 1 failures for two covariates Z_i = (Z_{i1}, Z_{i2})
# Pr(T_i \leq t, \epsilon_i = 1 | Z_i) = 1 - [1-p\{1-exp(-t)\}]^{exp(Z_i\beta_{11} + Z_{i2}\beta_{12})}

cens <- rep(0, nrow(fgdta))
cens[fgdta$fstatus == 2 | fgdta$fstatus == 3] <- 2
cens[fgdta$fstatus == 1] <- 1
fg1 <-
	cuminc(
		fgdta$since.leavework2,
		fstatus = cens,
		group = fgdta$jobloss.age.cat,
		cencode = 0
	)
fg1$Tests # CI curves are statistically different

#### ESTIMATES ####
# "ATE" estimates of Fine & Gray
10000 * (timepoints(fg1, c(1, 2, 5, 10) * 365)$est[2:4,] -
		timepoints(fg1, c(1, 2, 5, 10) * 365)$est[1,])

#### The point estimates don't change based on strata,
# but the variance does (obvi)
fg2 <- cuminc(
		ftime = fgdta$since.leavework2,
		fstatus = cens,
		group = fgdta$jobloss.age.cat,
		strata = factor(fgdta$plant),
		cencode = 0
	)

# FG hazard ####
# get.coxph(recent = T, run_categorical = F, run_spline = F, run_finegray = T)
# Sys.sleep(0)
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.coef(recent = T, get_finegray = T)

# get.coxph(recent = T, run_categorical = F, run_spline = F, run_finegray = T, years.max = 5)
# Sys.sleep(0)
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.coef(recent = T, get_finegray = T, years.max = 5)
