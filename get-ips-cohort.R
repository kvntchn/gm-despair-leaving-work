# Cohort generation for IPS ####
# Kevin Chen
# November 20, 2019

library(here)

if (max(grepl("yout.which", ls())) == 0) {
	yout.which <- "employment_end.date"
}


if (max(grepl("middle_adulthood", ls())) == 0) {
	middle_adulthood <- F
}

if (!"include_alcohol" %in% ls()) {
	include_alcohol <- F
}

# Get cohort ####
# rm(cohort_analytic)
if (!('get.cohort_analytic' %in% ls())) {
	source(here::here('../gm-wrangling/wrangling', '00-hello.R'))
}

# if (!('cohort_analytic' %in% ls())) {
cohort_analytic <- get.cohort_analytic(
	outcome_type = 'mortality',
	exposure.lag = 0,
	deathage.max = 100,
	include_alcohol = ifelse(include_alcohol, T, F)
)
# }

cohort_full <- as.data.table(as.data.frame(
	cohort_analytic
))
setorder(cohort_full, studyno, year)
cohort_full <- cohort_full[
	year >= 1941 &
		year(yin) <= 1982 &
		(year <= year(yoc) | is.na(yoc))
	]
cohort_full$filler <- 0

if (!('include_missing_hist' %in% ls()) |
		('include_missing_hist' %in% ls() & !include_missing_hist)) {
	cohort_full <- cohort_full[nohist == 0 & wh == 1]
}

# New column for despair death
cohort_full[,`:=`(`Despair` = as.numeric(`Suicide` == 1 | `Overdose` == 1))]

# Column for year of observation
cohort_full[,`:=`(year_obs = year - year(yin) + 1 - 3)]

# If no employment_end.date, get it from yout
cohort_full[is.na(dateout.date), dateout.date := yout]
cohort_full[is.na(employment_end.date), employment_end.date := yout]
cohort_full[is.na(employment_end.date.legacy), employment_end.date.legacy := yout]

# Pick jobloss.date ####
cohort_full[, jobloss.date := get(yout.which)]

# Machining and Assembly and time spent in plants
cohort_full[, `:=`(
	assembly = assembly.gan + assembly.han + assembly.san,
	machining = machining.gan + machining.han + machining.san,
	off = off.gan +  off.han + off.san
)]

# Fill machining, assembly, off
# years to be filled:	1			2
# count:							2153	318
cohort_full[,`:=`(
	machining = zoo::na.locf(machining, na.rm = F),
	assembly = zoo::na.locf(assembly, na.rm = F),
	off = zoo::na.locf(off, na.rm = F)
), by = .(studyno)]

# Pension eligible after 30 years
cohort_full[, `:=`(
	pension.eligible = ifelse(yearwork > 30, 1, 0)
)]

# Look only at people still at risk of exposure in 1970 on
year.min <- 1970
# year.min <- 1941
cohort_ips <- cohort_full[
	# Remove pt not at-risk for outcome
	immortal == 0 &
		# Person-time stops by end of 1994
		year <= 1994 &
		# Person-time doesn't start until 1970
		year >= year.min &
		# At-risk for exposure in or after 1970
		year(jobloss.date) >= year.min &
		# Started working before 65 years of age
		time_length(difftime(yin, yob), 'years') < 65
	# # Person-time ends before reaching 5 years of age
	# age.year2 < 65
	]

if (middle_adulthood) {
	cohort_ips <- cohort_ips[
		# Left work after reaching 30 years of age
		time_length(difftime(jobloss.date, yob), 'years') >= 30 &
			# Person-time begins after reaching 30 years of age
			age.year1 >= 30]
}

setorder(cohort_ips, studyno, year)

# Two suicide cases and one overdose case at age 30, but individual was 29 at start of year
#    studyno year age.year1 age.year2 Suicide Overdose
# 1:  103109 1988  29.92638  30.36942       1        0
# 2:  121220 1986  29.41877  30.02192       0        1
# 3:  146175 1983  29.08580  30.00917       1        0

# Fill in work history after job loss (end of record) ####
# Simplify plant proportion
cohort_ips[!(
	(record.years.gan > 0) +
		(record.years.han > 0) +
		(record.years.san > 0)
) > 1, `:=`(
	record.years.gan = ifelse(record.years.gan > 0, 1, 0),
	record.years.han = ifelse(record.years.han > 0, 1, 0),
	record.years.san = ifelse(record.years.san > 0, 1, 0)
)]

cohort_ips[(
	(record.years.gan > 0) +
		(record.years.han > 0) +
		(record.years.san > 0)
) == 0, `:=`(
	record.years.gan = NA,
	record.years.han = NA,
	record.years.san = NA
)]

# People who left work on 1970-01-01
cohort_ips[jobloss.date <= as.Date('1973-01-01'), `:=`(
	record.years.gan = ifelse(plant == 1, 1, 0),
	record.years.han = ifelse(plant == 2, 1, 0),
	record.years.san = ifelse(plant == 3, 1, 0)
)]

# Carry forward last plant
cohort_ips[, `:=`(
	record.years.gan = zoo::na.locf(record.years.gan, na.rm = F),
	record.years.han = zoo::na.locf(record.years.han, na.rm = F),
	record.years.san = zoo::na.locf(record.years.san, na.rm = F)
), by = .(studyno)]

# No assembly, mahcining, or off after losing job
cohort_ips[year > year(jobloss.date), `:=`(
	assembly = 0,
	machining = 0,
	off = 0
)]

cohort_ips[,`:=`(
	off.cum = cumsum(off)
), by = .(studyno)]

# Every individual should have 25 rows (year.min through 1994)

cohort_ips[, `:=`(I = 1:.N,
									I.max = .N), by = .(studyno)]

cohort_ips <- rbindlist(list(
	# Add years from minimum year to Year of hire
	cohort_ips[I == 1 & year > year.min][, .(
		year = rep(year, length(year.min:(year - 1))),
		jobloss.date,
		yob,
		yin,
		finrace,
		race,
		sex,
		# Time varying covariates that carry forward
		plant,
		yod,
		age.year1,
		age.year2,
		# `Job loss`,
		Despair,
		Suicide,
		Overdose,
		`All causes`,
		pension.eligible,
		machining,
		assembly,
		off,
		off.cum,
		record.years.gan,
		record.years.han,
		record.years.san,
		yout,
		filler = 1,
		I
	), by = .(studyno)],
	cohort_ips,
	cohort_ips[I == I.max & year < 1994][, .(
		year = rep(year, length((year + 1):1994)),
		jobloss.date,
		yob,
		yin,
		finrace,
		race,
		sex,
		# Time varying covariates that carry forward
		plant,
		yod,
		age.year1,
		age.year2,
		# `Job loss`,
		Despair,
		Suicide,
		Overdose,
		`All causes`,
		pension.eligible,
		machining,
		assembly,
		off,
		off.cum,
		record.years.gan,
		record.years.han,
		record.years.san,
		yout,
		filler = 1,
		I
	), by = .(studyno)]),
	use.names = T,
	fill = T
)
cohort_ips[is.na(filler) | filler != 0, filler := 1 ]

# Order data file
setorder(cohort_ips, studyno, year)

# Time!
setorder(cohort_ips, studyno, year)
cohort_ips[, `:=`(
	I = 1:.N,
	time = year.min:1994,
	year_obs = {
		if (is.na(year_obs[1])) {
			zoo::na.locf(
				zoo::na.locf(year_obs, fromLast = T, na.rm = F))
		} else {
			zoo::na.locf(year_obs)}
	}
), by = .(studyno)]

# # Verify
# sum(
# 	!(complete.cases(cohort_ips[,.(
# 	I,
# 	studyno,
# 	year,
# 	yob,
# 	yin,
#   finrace,
# 	race,
# 	sex,
# 	year_obs,
# 	age.year2,
# 	assembly,
# 	machining,
# 	off,
# 	plant,
# 	years.gan = record.years.gan,
# 	years.san = record.years.san,
# 	years.han = record.years.han,
#   pension.eligible,
# 	`Job loss`,
# 	Despair,
# 	`All causes`
# )]))
# )
#
# View(cohort_ips[,.(
# 	I,
# 	studyno,
# 	year,
# 	yob,
# 	yin,
#   finrace,
# 	race,
# 	sex,
# 	year_obs,
# 	age.year2,
# 	assembly,
# 	machining,
# 	off,
# 	plant,
# 	years.gan = record.years.gan,
# 	years.san = record.years.san,
# 	years.han = record.years.han,
#   pension.eligible,
# 	`Job loss`,
# 	Despair,
# 	`All causes`
# )])

# Tabulation of person-years ####
# View(cohort_ips[,.(N = .N,
# 									 `At risk` = n_distinct(studyno[filler == 0]),
# 									 `Left work` = sum(
# 									 	year(jobloss.date[filler == 0]) == time),
# 									 `Suicide deaths` = sum(Suicide[filler == 0])), by = time])

studyno.sample <- sample(unique(cohort_ips$studyno),
												 5000,
												 replace = F)

source(here::here('get-ips-cohort-recent.R'))
