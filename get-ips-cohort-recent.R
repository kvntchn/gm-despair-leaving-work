# Get IPS cohort, removing those who left work after 1994
# Kevin Chen
# December 16, 2019

# Look only at people still at risk of exposure in 1970 on
cohort_recent <- cohort_full[
	# Remove pt not at-risk for outcome
	immortal == 0 &
		# # Person-time stops by end of 1994
		# year <= 1994 &
		# Person-time doesn't start until 1970
		year >= 1970 &
		# At-risk for exposure in or after 1970
		year(jobloss.date) >= 1970 &
		# # Known date of leaving work, or censor
		# (year(jobloss.date) < 1995 | (year(jobloss.date) >= 1995 & year < 1995)) &
		# Started working before 65 years of age
		time_length(difftime(yin, yob), 'years') < 65
	# # Person-time ends before reaching 5 years of age
	# age.year2 < 65
	]

if (middle_adulthood) {
	cohort_recent <- cohort_recent[
		# Left work after reaching 30 years of age
		time_length(difftime(jobloss.date, yob), 'years') >= 30 &
			# Person-time begins after reaching 30 years of age
			age.year1 >= 30]
}

setorder(cohort_recent, studyno, year)
year.min <- 1970
# year.min <- 1941
year.max <- 2015

# Two suicide cases and one overdose case at age 30, but individual was 29 at start of year
#    studyno year age.year1 age.year2 Suicide Overdose
# 1:  103109 1988  29.92638  30.36942       1        0
# 2:  121220 1986  29.41877  30.02192       0        1
# 3:  146175 1983  29.08580  30.00917       1        0

# Fill in work history after job loss (end of record) ####
# Simplify plant proportion
cohort_recent[!(
	(record.years.gan > 0) +
		(record.years.han > 0) +
		(record.years.san > 0)
) > 1, `:=`(
	record.years.gan = ifelse(record.years.gan > 0, 1, 0),
	record.years.han = ifelse(record.years.han > 0, 1, 0),
	record.years.san = ifelse(record.years.san > 0, 1, 0)
)]

cohort_recent[(
	(record.years.gan > 0) +
		(record.years.han > 0) +
		(record.years.san > 0)
) == 0, `:=`(
 record.years.gan = NA,
	record.years.han = NA,
	record.years.san = NA
)]

# People who left work on 1970-01-01
cohort_recent[jobloss.date <= as.Date('1973-01-01'), `:=`(
	record.years.gan = ifelse(plant == 1, 1, 0),
	record.years.han = ifelse(plant == 2, 1, 0),
	record.years.san = ifelse(plant == 3, 1, 0)
)]

# Carry forward last plant
cohort_recent[, `:=`(
	record.years.gan = zoo::na.locf(record.years.gan, na.rm = F),
	record.years.han = zoo::na.locf(record.years.han, na.rm = F),
	record.years.san = zoo::na.locf(record.years.san, na.rm = F)
	), by = .(studyno)]

# No assembly, mahcining, or off after losing job
cohort_recent[year > year(jobloss.date), `:=`(
	assembly = 0,
	machining = 0,
	off = 0
)]

cohort_recent[,`:=`(
	off.cum = cumsum(off)
), by = .(studyno)]

# Every individual should have 25 rows (year.min through year.max)

cohort_recent[, `:=`(I = 1:.N,
									I.max = .N), by = .(studyno)]

cohort_recent <- rbindlist(list(
	# Add years from minimum year to Year of hire
	cohort_recent[I == 1 & year > year.min][, .(
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
	cohort_recent,
	cohort_recent[I == I.max & year < year.max][, .(
		year = rep(year, length((year + 1):year.max)),
		jobloss.date,
		yob,
		yin,
		finrace,
		race,
		sex,
		# Time varying covariates that carry forward
		plant,
		yod,
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
cohort_recent[is.na(filler) | filler != 0, filler := 1 ]

# Order data file
setorder(cohort_recent, studyno, year)

# Jobloss by yout
cohort_recent[,`:=`(
	`Job loss (by yout)` = ifelse(
		year >= year(yout),
		1, 0
	)
)]
cohort_recent[year(yout) == 1995, `:=`(
	`Job loss (by yout)` = 0
)]

# # Overwrite my jobloss
# cohort_recent[,`:=`(
# 	jobloss.date = yout,
# 	`Job loss` = `Job loss (by yout)`
# )]
# cohort_recent[year(jobloss.date) == 1995, `:=`(
# 					 jobloss.date = NA
# 					 )]

# Time!
setorder(cohort_recent, studyno, year)
cohort_recent[, `:=`(
	I = 1:.N,
	time = year.min:2015,
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
# 	!(complete.cases(cohort_recent[,.(
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
# View(cohort_recent[,.(
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

# # Tabulation of person-years ####
# View(cohort_recent[,.(N = .N,
#               `At risk` = n_distinct(studyno[filler == 0]),
#               `Left work` = sum(
#               	year(jobloss.date[filler == 0]) == time),
#               `Suicide deaths` = sum(Suicide[filler == 0])), by = time])

studyno_before95.sample <- sample(unique(cohort_recent$studyno),
			 15000,
			 replace = F)
