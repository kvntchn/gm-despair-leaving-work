# Age at leaving work analyses ####
# December 5, 2019
# Kevin Chen
# rm(list = ls()[-which(grepl('cohort_full|cohort_ips', ls()))])

if (!grepl("gm", getwd(), ignore.case = T)) {
	if ("here" %in% .packages()) {
		detach("package:here", unload = T)
	}
	setwd('gm')
	library(here)
}


middle_adulthood <- F
include_missing_hist <- T
include_alcohol <- T
ref.age <- 55
if (ifelse(
	!('cohort_recent' %in% ls()), T,
	is.null(cohort_recent$jobloss.age))) {
	source(here::here('despair', 'get-1970-cohort.R'))

	cohort_analytic[is.na(employment_end.date) & !is.na(yout),
									employment_end.date := yout]
	cohort_analytic[is.na(employment_end.date.legacy) & !is.na(yout),
									employment_end.date.legacy := yout]
	cohort_analytic[, jobloss.date := get(yout.which)]
	cohort_analytic[,`:=`(
		jobloss.age = time_length(
			difftime(jobloss.date, yob), 'years'),
		filler = 0)]

	cohort_full[,jobloss.age := time_length(
		difftime(jobloss.date, yob), 'years')]
	cohort_ips[,jobloss.age := time_length(
		difftime(jobloss.date, yob), 'years')]
	cohort_recent[,jobloss.age := time_length(
		difftime(jobloss.date, yob), 'years')]

	cohort_full <- cohort_full[immortal == 0 & filler == 0]
	cohort_full[, known_status := ifelse(
		year(jobloss.date) < 1995 |
			(year(jobloss.date) >= 1995 & year < 1995), 1, 0)]

	cohort_recent <- cohort_recent[immortal == 0 & filler == 0]
	cohort_recent[, known_status := ifelse(
		year(jobloss.date) < 1995 |
			(year(jobloss.date) >= 1995 & year < 1995), 1, 0)]

	cohort_full[, `:=`(
		jobloss.age.cat = cut(
			jobloss.age,
			c(19, 30, 40, 55, Inf), include.lowest = T,
			right = F
		),
		died_while_employed = factor(
			ifelse(yod <= jobloss.date,
						 "Employed", "Not employed"),
			levels = c("Not employed","Employed"
			))
	)]

	cohort_ips[, `:=`(
		jobloss.age.cat = cut(
			jobloss.age,
			c(19, 30, 40, 55, Inf), include.lowest = T,
			right = F
		),
		died_while_employed = factor(
			ifelse(yod <= jobloss.date,
						 "Employed", "Not employed"),
			levels = c("Not employed","Employed"
			))
	)]

	cohort_recent[, `:=`(
		jobloss.age.cat = cut(
			jobloss.age,
			c(19, 30, 40, 55, Inf), include.lowest = T,
			right = F
		),
		died_while_employed = factor(
			ifelse(yod <= jobloss.date,
						 "Employed", "Not employed"),
			levels = c("Not employed","Employed"
			))
	)]


	cohort_full[, `:=`(
		jobloss.age.lower = as.numeric(substring(
			jobloss.age.cat, 2,
			unlist(
				regexpr(",", jobloss.age.cat)
			) - 1)),
		jobloss.age.upper = as.numeric(substring(
			jobloss.age.cat,
			unlist(regexpr(",", jobloss.age.cat)) + 1,
			nchar(levels(jobloss.age.cat)[as.numeric(jobloss.age.cat)]) - 1))
	)]

	cohort_full[, `:=`(
		jobloss.age.cat = {
			lower <- jobloss.age.lower
			lower <- paste0(lower, ifelse(
				lower != max(lower)
				, " to", ""))

			upper <- jobloss.age.upper - 1
			inf.which <- which(!is.finite(upper))
			upper[inf.which] <- "years or older"
			upper[-inf.which] <- paste(upper[-inf.which], "years")

			factor(paste(lower, upper),
						 levels = c("19 to 29 years",
						 					 "30 to 39 years",
						 					 "40 to 54 years",
						 					 "55 years or older"))
		}
	)]

	cohort_ips[, `:=`(
		jobloss.age.lower = as.numeric(substring(
			jobloss.age.cat, 2,
			unlist(
				regexpr(",", jobloss.age.cat)
			) - 1)),
		jobloss.age.upper = as.numeric(substring(
			jobloss.age.cat,
			unlist(regexpr(",", jobloss.age.cat)) + 1,
			nchar(levels(jobloss.age.cat)[as.numeric(jobloss.age.cat)]) - 1))
	)]

	cohort_ips[, `:=`(
		jobloss.age.cat = {
			lower <- jobloss.age.lower
			lower <- paste0(lower, ifelse(
				lower != max(lower)
				, " to", ""))

			upper <- jobloss.age.upper - 1
			inf.which <- which(!is.finite(upper))
			upper[inf.which] <- "years or older"
			upper[-inf.which] <- paste(upper[-inf.which], "years")

			factor(paste(lower, upper),
						 levels = c("19 to 29 years",
						 					 "30 to 39 years",
						 					 "40 to 54 years",
						 					 "55 years or older"))
		}
	)]

	cohort_recent[, `:=`(
		jobloss.age.lower = as.numeric(substring(
			jobloss.age.cat, 2,
			unlist(
				regexpr(",", jobloss.age.cat)
			) - 1)),
		jobloss.age.upper = as.numeric(substring(
			jobloss.age.cat,
			unlist(regexpr(",", jobloss.age.cat)) + 1,
			nchar(levels(jobloss.age.cat)[as.numeric(jobloss.age.cat)]) - 1))
	)]

	cohort_recent[, `:=`(
		jobloss.age.cat = {
			lower <- jobloss.age.lower
			lower <- paste0(lower, ifelse(
				lower != max(lower)
				, " to", ""))

			upper <- jobloss.age.upper - 1
			inf.which <- which(!is.finite(upper))
			upper[inf.which] <- "years or older"
			upper[-inf.which] <- paste(upper[-inf.which], "years")

			factor(paste(lower, upper),
						 levels = c("19 to 29 years",
						 					 "30 to 39 years",
						 					 "40 to 54 years",
						 					 "55 years or older"))
		}
	)]

	# Remove people with unknown cause of death
	cohort_analytic <- cohort_analytic[!(status15 == 6 & (is.na(icd)))]
	# Remove people with unknown cause of death
	cohort_recent <- cohort_recent[!(status15 == 6 & (is.na(icd)))]
	# Remove people with unknown cause of death
	cohort_full <- cohort_full[!(status15 == 6 & (is.na(icd)))]

}
source("~/HeadRs/00-my-theme.R")

# Histograms ####
get.hist <- function(
	in_ips = F,
	recent = T,
	width = 3.5,
	height = 2.75,
	hist.which = NULL,
	directory = here::here("reports/resources")
) {

	dir.create(directory, F, T)

	if (!in_ips) {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_full))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	} else {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_ips))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	}

	dat <- dat[filler != 1 & sex == "M"]

	dat <- dat[age.year1 < age.year2]

	setorder(dat, studyno, year)
	dat[,`:=`(I = 1:.N,
						N = .N), by = .(studyno)]

	# Age at worker exit ####
	if (is.null(hist.which) | (
		sum(grepl('age', hist.which, ignore.case = T)) > 0 &
		sum(grepl('leave', hist.which, ignore.case = T)) > 0)) {
		tikz(file = paste0(
			directory,
			paste0('/hist_age_leavework', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width, height = height)

		print(
			dat[, .(
				jobloss.date = jobloss.date[1],
				jobloss.age = jobloss.age[1],
				yob = yob[1]), by = .(studyno)] %>% ggplot(
					aes(x = jobloss.age)
				) + geom_histogram(bins = 40, color = "black") +
				# stat_density(color = "red",
				# 						 fill = 'red',
				# 						 alpha = 0.2) +
				coord_cartesian(ylim = c(0, 1800)) +
				labs(x = "Age at worker exit",
						 y = "Count") +
				mytheme + theme(plot.margin = margin(3, 3, 4, 3))
		)

		dev.off()
	}

	# Year of worker exit ####
	if (is.null(hist.which) | (
		sum(grepl('year', hist.which, ignore.case = T)) > 0 &
		sum(grepl('leave', hist.which, ignore.case = T)) > 0)) {
		tikz(file = paste0(
			directory,
			paste0('/hist_year_leavework', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height)

		print(
			dat[I == 1,.(
				jobloss.date,
				jobloss.age = factor(time_length(difftime(
					jobloss.date, yob
				), 'years') > 55,
				levels = c("TRUE", "FALSE"),
				labels = c("Left work at age 55 or older", "Left work under the age of 55"))
			)] %>% ggplot(
				aes(x = jobloss.date)
			) + geom_histogram(
				bins = ifelse(in_ips | recent, 30, 40), color = "black") +
				facet_wrap(. ~ jobloss.age, ncol = 2) +
				labs(
					y = "Count",
					x = 'Year of worker exit') +
				mytheme
		)

		dev.off()
	}

	# Suicide - worker exit ####
	if (is.null(hist.which) | (
		sum(grepl('time', hist.which, ignore.case = T)) > 0 &
		sum(grepl('suicide', hist.which, ignore.case = T)) > 0)) {

		lb <- floor(min(dat[Suicide == 1 , time_length(difftime(
			yod, jobloss.date
		), 'years')]))
		ub <- ceiling(max(dat[Suicide == 1 , time_length(difftime(
			yod, jobloss.date
		), 'years')]))
		full_window.ggplot <- ggplot(
			dat[Suicide == 1,.(
				yod,
				jobloss.date,
				outcome = 'Suicide',
				time_to_event = time_length(difftime(
					yod, jobloss.date
				), 'years'),
				view = "A) All suicide cases"
			)], aes(x = time_to_event,
							fill = factor(ifelse(time_to_event > 0,
																	 "Not employed","Employed"
							)))
		) + geom_histogram(
			breaks = unique(c(lb, seq(-2, 25, 1), seq(25, ub, 5))),
			closed = "right", color = "black") +
			# stat_bin(aes(label = ..count..), geom = "text",
			# 				 breaks = unique(c(lb, seq(-2, 25, 1), seq(25, ub, 5))),
			# 				closed = "right") +
			scale_y_continuous(breaks = seq(
				0, 100, 20)) +
			labs(
				y = "Count",
				x = 'Years',
				fill = "Employment status at time of death:") +
			scale_fill_viridis(discrete = T) +
			# scale_fill_manual(values = c(
			# 	rgb(255, 210, 5, max = 255),
			# 	rgb(68, 1, 84, max = 255)
			# 	# rgb(49, 104, 142, max = 255),
			# 	# rgb(53, 183, 121, max = 255),
			# )) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)

		tikz(file = paste0(
			directory,
			paste0('/hist_time_till_suicide', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height * 1.2)
		print(
			full_window.ggplot + theme(
				legend.key.size = unit(7, 'pt'),
				legend.position = "bottom")
		)
		dev.off()

		zoom.lb <- floor(min(dat[Suicide == 1 , .(V1 = time_length(difftime(
			yod, jobloss.date
		), 'days'))]$V1))
		ub <- ceiling(max(dat[Suicide == 1 , time_length(difftime(
			yod, jobloss.date
		), 'days')]))
		zoomed_view.ggplot <- ggplot(
			dat[Suicide == 1,.(
				yod,
				jobloss.date,
				outcome = 'Suicide',
				time_to_event = time_length(difftime(
					yod, jobloss.date
				), 'days'),
				view = "B) Cases within 2 weeks of worker exit"
			)][time_to_event <= 366], aes(
				x = time_to_event,
				fill = factor(ifelse(time_to_event > 0,
														 "Not employed", "Employed"))
			)
		) + geom_histogram(
			breaks = zoom.lb:366,
			closed = "right", color = "black") +
			# stat_bin(aes(label = ..count..), geom = "text",
			# 				 breaks = zoom.lb:366,
			# 				closed = "right") +
			coord_cartesian(xlim = c(-14, 14)) +
			scale_fill_viridis(discrete = T) +
			scale_y_continuous(breaks = seq(
				0, 20, 2)) +
			labs(
				y = "Count",
				x = 'Days',
				fill = "Employment status at time of death:") +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)

		tikz(file = paste0(
			directory,
			paste0('/hist_time_till_suicide_zoom', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height * 1.2)
		print(
			zoomed_view.ggplot + theme(
				legend.key.size = unit(7, 'pt'),
				legend.position = "bottom")
		)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0('/hist_time_till_suicide_two_views', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 2, height = height * 1.2)
		print(
			gridExtra::grid.arrange(
				full_window.ggplot + facet_wrap(~ view) + theme(
					legend.position = 'none'),
				zoomed_view.ggplot + facet_wrap(~ view) + theme(
					axis.title.y = element_blank(),
					legend.position = 'none'
				),
				get_legend(
					zoomed_view.ggplot + theme(
						legend.key.size = unit(7, 'pt'),
						legend.position = 'bottom')
				),
				ncol = 2,
				layout_matrix = matrix(c(1, 2, 3, 3), ncol = 2, byrow = T),
				heights = c(1, 0.1)
			)
		)
		dev.off()
	}

	# Suicide + Overdose - worker exit ####
	if (is.null(hist.which) | (
		sum(grepl('time', hist.which, ignore.case = T)) > 0 &
		sum(grepl('despair', hist.which, ignore.case = T)) > 0)) {

		lb <- floor(min(dat[Despair == 1 , time_length(difftime(
			yod, jobloss.date
		), 'years')]))
		ub <- ceiling(max(dat[Despair == 1 , time_length(difftime(
			yod, jobloss.date
		), 'years')]))
		full_window.ggplot <- ggplot(
			dat[Despair == 1,.(
				yod,
				jobloss.date,
				outcome = 'Despair',
				time_to_event = time_length(difftime(
					yod, jobloss.date
				), 'years'),
				view = "A) All despair cases"
			)], aes(x = time_to_event,
							fill = factor(ifelse(time_to_event > 0,
																	 "Not employed","Employed"
							)))
		) + geom_histogram(
			breaks = unique(c(lb, seq(-2, 25, 1), seq(25, ub, 5))),
			closed = "right", color = "black") +
			# stat_bin(aes(label = ..count..), geom = "text",
			# 				 breaks = unique(c(lb, seq(-2, 25, 1), seq(25, ub, 5))),
			# 				closed = "right") +
			scale_y_continuous(breaks = seq(
				0, 100, 20)) +
			labs(
				y = "Count",
				x = 'Years',
				fill = "Employment status at time of death:") +
			scale_fill_viridis(discrete = T) +
			# scale_fill_manual(values = c(
			# 	rgb(255, 210, 5, max = 255),
			# 	rgb(68, 1, 84, max = 255)
			# 	# rgb(49, 104, 142, max = 255),
			# 	# rgb(53, 183, 121, max = 255),
			# )) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)

		tikz(file = paste0(
			directory,
			paste0('/hist_time_till_despair', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height * 1.2)
		print(
			full_window.ggplot + theme(
				legend.key.size = unit(7, 'pt'),
				legend.position = "bottom")
		)
		dev.off()

		zoom.lb <- floor(min(dat[Despair == 1 , .(V1 = time_length(difftime(
			yod, jobloss.date
		), 'days'))]$V1))
		ub <- ceiling(max(dat[Despair == 1 , time_length(difftime(
			yod, jobloss.date
		), 'days')]))
		zoomed_view.ggplot <- ggplot(
			dat[Despair == 1,.(
				yod,
				jobloss.date,
				outcome = 'Despair',
				time_to_event = time_length(difftime(
					yod, jobloss.date
				), 'days'),
				view = "B) Cases within 2 weeks of worker exit"
			)][time_to_event <= 366], aes(
				x = time_to_event,
				fill = factor(ifelse(time_to_event > 0,
														 "Not employed", "Employed"))
			)
		) + geom_histogram(
			breaks = zoom.lb:366,
			closed = "right", color = "black") +
			# stat_bin(aes(label = ..count..), geom = "text",
			# 				 breaks = zoom.lb:366,
			# 				closed = "right") +
			coord_cartesian(xlim = c(-14, 14)) +
			scale_fill_viridis(discrete = T) +
			scale_y_continuous(breaks = seq(
				0, 20, 2)) +
			labs(
				y = "Count",
				x = 'Days',
				fill = "Employment status at time of death:") +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)

		tikz(file = paste0(
			directory,
			paste0('/hist_time_till_despair_zoom', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height * 1.2)
		print(
			zoomed_view.ggplot + theme(
				legend.key.size = unit(7, 'pt'),
				legend.position = "bottom")
		)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0('/hist_time_till_despair_two_views', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 2, height = height * 1.2)
		print(
			gridExtra::grid.arrange(
				full_window.ggplot + facet_wrap(~ view) + theme(
					legend.position = 'none'),
				zoomed_view.ggplot + facet_wrap(~ view) + theme(
					axis.title.y = element_blank(),
					legend.position = 'none'
				),
				get_legend(
					zoomed_view.ggplot + theme(
						legend.key.size = unit(7, 'pt'),
						legend.position = 'bottom')
				),
				ncol = 2,
				layout_matrix = matrix(c(1, 2, 3, 3), ncol = 2, byrow = T),
				heights = c(1, 0.1)
			)
		)
		dev.off()
	}

	# Age suicide ####
	if (is.null(hist.which) | (
		sum(grepl('age', hist.which, ignore.case = T)) > 0)) {
		tikz(file = paste0(
			directory,
			paste0('/hist_age_suicide_by_age_leftwork', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height)
		print(
			dat[Suicide == 1,.(
				yob,
				yod,
				outcome = 'Suicide',
				jobloss.age = factor(time_length(difftime(
					jobloss.date, yob
				), 'years') > 55,
				levels = c("TRUE", "FALSE"),
				labels = c("Left work at age 55 or older", "Left work under the age of 55"))
			)] %>% ggplot(
				aes(x = time_length(difftime(
					yod, yob
				), 'years'))
			) + geom_histogram(breaks = seq(20, 90, 5), closed = "right", color = "black") +
				facet_wrap(. ~ jobloss.age, ncol = 2) +
				labs(
					y = "Count",
					x = 'Age at death due to suicide') +
				mytheme + theme(
					strip.text = element_text(hjust = 0)
				)
		)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0('/hist_age_suicide', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width, height = height)
		print(
			dat[Suicide == 1,.(
				yob,
				yod,
				outcome = 'Suicide'
			)] %>% ggplot(
				aes(x = time_length(difftime(
					yod, yob
				), 'years'))
			) + geom_histogram(breaks = seq(20, 90, 5), closed = "right", color = "black") +
				labs(
					y = "Count",
					x = 'Age at death due to suicide') +
				mytheme
		)
		dev.off()

		# Age overdose ####
		tikz(file = paste0(
			directory,
			paste0('/hist_age_despair_by_age_leftwork', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height)
		print(
			dat[Despair == 1,.(
				yob,
				yod,
				outcome = 'Despair',
				jobloss.age = factor(time_length(difftime(
					jobloss.date, yob
				), 'years') > 55,
				levels = c("TRUE", "FALSE"),
				labels = c("Left work at age 55 or older", "Left work under the age of 55"))
			)] %>% ggplot(
				aes(x = time_length(difftime(
					yod, yob
				), 'years'))
			) + geom_histogram(breaks = seq(20, 90, 5), closed = "right", color = "black") +
				facet_wrap(. ~ jobloss.age, ncol = 2) +
				labs(
					y = "Count",
					x = 'Age at death due to suicide and fatal overdose') +
				mytheme + theme(
					strip.text = element_text(hjust = 0)
				)
		)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0('/hist_age_despair', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width, height = height)
		print(
			dat[Despair == 1,.(
				yob,
				yod,
				outcome = 'Despair'
			)] %>% ggplot(
				aes(x = time_length(difftime(
					yod, yob
				), 'years'))
			) + geom_histogram(breaks = seq(20, 90, 5), closed = "right", color = "black") +
				labs(
					y = "Count",
					x = 'Age at death due to suicide and fatal overdose') +
				mytheme
		)
		dev.off()

		# Both ####
		tikz(file = paste0(
			directory,
			paste0('/hist_age_suicide_despair', ifelse(in_ips,	ifelse(
				!recent, "_in_ips", "_recent"), ifelse(
					recent, "_recent",	"")), '.tex')),
			standAlone = T, width = width * 1.85, height = height)
		print(
			rbindlist(list(
				dat[Suicide == 1,.(
					yob,
					yod,
					outcome = 'A) Suicide'
				)],
				dat[Overdose == 1,.(
					yob,
					yod,
					outcome = 'B) Fatal overdose'
				)]
			)) %>% ggplot(
				aes(x = time_length(difftime(
					yod, yob
				), 'years'))
			) + geom_histogram(breaks = seq(20, 90, 5), closed = "right", color = "black") +
				labs(
					y = "Count",
					x = 'Age at death') +
				facet_wrap(. ~ outcome, ncol = 2) +
				mytheme + theme(
					strip.text = element_text(hjust = 0)
				)
		)
		dev.off()
	}
}

# Who's at work plots ####
get.atwork <- function(
	in_ips = F,
	recent = T,
	width = 3.5,
	height = 2.75,
	directory = here::here("reports/resources")
) {
	dir.create(directory, F, T)

	# Get appropariate data
	if (in_ips) {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_ips)) } else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
	} else {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_full)) } else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
	}

	dat <- dat[filler != 1 & year < 1995]

	dat$plant <- factor(dat$plant,
											levels = 1:3,
											labels = c("Plant 1", "Plant 2", "Plant 3"))
	tikz(file = paste0(
		directory,
		paste0('/at_work', ifelse(in_ips,	ifelse(
			!recent, "_in_ips", "_recent"), ifelse(
				recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width, height = height)
	print(
		ggplot(dat[year <= year(jobloss.date), .(
			`At work` = length(studyno),
			`Left work` = length(studyno[
				year == year(jobloss.date)])
		), by = .(year, plant)],
		aes(x = year,
				color = plant)
		) + geom_line(aes(y = `At work`,
											linetype = "At work")) +
			geom_line(aes(y = `Left work`,
										linetype = 'Left work')) +
			labs(y = "Count",
					 x = "Year",
					 color = "",
					 linetype = "") +
			mytheme + theme(
				legend.position = 'bottom',
				legend.margin = margin(0,0,-10,0),
				legend.box.margin = margin(-5,0,5,0),
				legend.box = "vertical",
				plot.margin = margin(5,10,5,5))
	)
	dev.off()

}

# Time to outcome ####
get.since_leavework_summary <- function(
	in_ips = F,
	make.graph = T,
	recent = T,
	directory = here::here("reports/resources")
) {
	dir.create(directory, F, T)

	if (!in_ips) {
		dat <- as.data.table(as.data.frame(cohort_full))
	} else {
		if (!recent)	{
			dat <- as.data.table(as.data.frame(cohort_ips))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	}

	dat <- dat[filler == 0]

	probs <- sort(unique(c(0.01, 0.05,
												 0.99, 0.95,
												 0.5,
												 seq(0, 1, 0.20))))

	full <-  quantile(dat[Suicide == 1, time_length(difftime(yod, jobloss.date), 'years')],
										probs)

	lower_20 <- quantile(dat[
		Suicide == 1 &
			time_length(difftime(yod, jobloss.date), 'years') <= full[names(full) == "20%"], time_length(difftime(yod, jobloss.date), 'days')],
		probs)

	lower_half <- quantile(dat[
		Suicide == 1 &
			time_length(difftime(yod, jobloss.date), 'years') <= full[names(full) == "50%"], time_length(difftime(yod, jobloss.date), 'years')],
		probs)

	upper_half <- quantile(dat[
		Suicide == 1 &
			time_length(difftime(yod, jobloss.date), 'years') >= full[names(full) == "50%"], time_length(difftime(yod, jobloss.date), 'years')],
		probs)

	upper_20 <- quantile(dat[
		Suicide == 1 &
			time_length(difftime(yod, jobloss.date), 'years') >= full[names(full) == "80%"], time_length(difftime(yod, jobloss.date), 'years')],
		probs)

	tmp.tab <- data.frame(
		Quantile = probs,
		'Full range (years)' = full,
		'Lower quintile (days)' = lower_20,
		'Lower half (years)' = lower_half,
		'Upper half (years)' = upper_half,
		'Upper quntile (years)' = upper_20,
		check.names = F
	)

	saveRDS(tmp.tab,
					paste0(directory, paste0(
						'/suicide_since_leftwork',
						ifelse(in_ips, ifelse(!recent, '_in_ips', '_recent'),
									 ifelse(!recent, "_in_full", "_recent")), '.rds')
					))

}

# Age at leaving work by hire cohort ####
get.leavework_summary <- function(
	in_ips = F,
	make.graph = T,
	recent = T,
	width = 6.5, height = 4.5,
	directory = here::here("reports/resources")
) {
	dir.create(directory, F, T)

	if (!in_ips) {
		dat <- as.data.table(as.data.frame(cohort_full))
	} else {
		if (!recent)	{
			dat <- as.data.table(as.data.frame(cohort_ips))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	}
	dat[filler == 0 & year(jobloss.date) < 1995, .(
		studyno,
		year,
		age = floor(age.year1/365.25),
		yin,
		jobloss.date
	)] %>% as.data.frame %>% as.data.table -> leavework.tab

	leavework.tab <- leavework.tab[year <= year(jobloss.date)]

	leavework.tab[,
								`Year of hire` := cut(
									year(yin),
									quantile(year(yin),
													 seq(0, 1, 0.25)),
									dig.lab = 4, include.lowest = T, right = F)
								]


	leavework.tab[,.(
		`At work` = .N,
		`Left work` = sum(year == year(jobloss.date) &
												year(jobloss.date) != 1995)
	), by = .(age, `Year of hire`)][order(age), .(
		age,
		`At work`,
		`Left work`,
		`Left work (cumulative)` = cumsum(`Left work`)
	), by = .(`Year of hire`)] -> leavework.tab

	leavework.tab[,`:=`(
		`Year of hire` = factor(
			`Year of hire`,
			labels = paste0(
				as.numeric(substr(
					levels(leavework.tab$`Year of hire`), 2, 5
				)) + c(0, 1, 1, 1),
				' to ',
				substr(
					levels(leavework.tab$`Year of hire`), 7, 10
				))
		))]

	if (make.graph) {
		tikz(paste0(directory, paste0(
			'/leftwork_byage', ifelse(in_ips,
															 ifelse(!recent,
															 			 '_in_ips',
															 			 '_recent'), ifelse(!recent, "_in_full", "_recent")), '.tex')
		),
		width = width, height = height, standAlone = T)
		print(gridExtra::grid.arrange(
			# Number at work by age
			ggplot(leavework.tab,
						 aes(x = age,
						 		y = `At work`,
						 		color = `Year of hire`)) +
				geom_path() +
				coord_cartesian(ylim = c(0, 7500)) +
				labs(x = "Age") +
				mytheme +
				theme(plot.margin = margin(3, 3, 4, 3),
							legend.position = 'none'),

			# Number left work at age
			ggplot(leavework.tab,
						 aes(x = age,
						 		y = `Left work`,
						 		color = `Year of hire`)) +
				geom_path() +
				coord_cartesian(ylim = c(0, 540)) +
				labs(x = "Age") +
				mytheme +
				theme(plot.margin = margin(3, 3, 4, 3),
							legend.position = 'none'),

			# loess
			ggplot(leavework.tab,
						 aes(x = age,
						 		y = `Left work`/`At work`,
						 		color = `Year of hire`)) +
				geom_smooth(method = 'loess', se = F, size = 0.65, color = "black") +
				coord_cartesian(ylim = c(0, 0.525)) +
				labs(color = "Year of hire:",
						 x = "Age",
						 y = "Risk of worker exit (smoothed)") +
				mytheme + theme(
					plot.margin = margin(0, 1.65, 0.2, 1.65, "inches"),
					legend.margin = margin(0,0,0,0),
					legend.box.margin = margin(-10,-10,-10,-10),
					legend.position = 'bottom')
			,
			widths = c(0.5, 0.5),
			heights = c(1, 1.05),
			ncol = 2,
			nrow = 2,
			layout_matrix = rbind(c(1, 2), c(3, 3))
		))
		dev.off()
	}

	if (in_ips) {
		message("For IPS cohort")
	} else {
		message("For mortality cohort")
	}
	return(
		leavework.tab[,.(
			Minimum = summary(rep(age, `Left work`))[1],
			Q1 = summary(rep(age, `Left work`))[2],
			Median = summary(rep(age, `Left work`))[3],
			Mean = summary(rep(age, `Left work`))[4],
			Q3 = summary(rep(age, `Left work`))[5],
			Maximum = summary(rep(age, `Left work`))[6],
			`SD` = sqrt(sum(age^2 * `Left work`) / sum(`Left work`) - (sum(age * `Left work`) / sum(`Left work`))^2)
		), by = .(`Year of hire`)][order(`Year of hire`)]
	)

}

# Crude cumulative incidence ####
get.cruderisk_byage <- function(
	in_ips = F,
	recent = T,
	width = 3.5,
	height = 2.75,
	get_rate = F,
	directory = here::here("reports/resources")
) {
	dir.create(directory, F, T)

	# Get appropariate data
	if (in_ips) {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_ips)) } else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
	} else {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_full)) } else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
	}

	dat <- dat[filler != 1, .(
		Suicide = max(Suicide),
		Overdose = max(Overdose),
		jobloss.date = jobloss.date[1],
		jobloss.age = time_length(difftime(jobloss.date[1], yob[1]),
															'years'),
		hire.age = time_length(difftime(yin[1], yob[1]),
													 'years'),
		yin = yin[1]
	), by = .(studyno)]

	dat.og <- as.data.table(as.data.frame(dat))

	dat <- rbindlist(
		lapply(sort(unique(floor(dat$jobloss.age))), function(x) {
			dat[,.(
				`At work` = length(studyno[floor(jobloss.age) <= x & (hire.age + 3) <= x]),
				`Left work` = length(studyno[floor(jobloss.age) == x]),
				`Suicide` = sum(Suicide[floor(jobloss.age) == x]),
				`Overdose` = sum(Overdose[floor(jobloss.age) == x]),
				jobloss.age = x
			)]
		})
	)

	dat[, jobloss.age.cat := cut(
		jobloss.age,
		c(seq(min(jobloss.age), 55, 5), max(jobloss.age)),
		right = F,
		include.lowest = T)]

	dat[, `:=`(
		lower = as.numeric(substring(
			jobloss.age.cat, 2,
			unlist(
				regexpr(",", jobloss.age.cat)
			) - 1)),
		upper = as.numeric(substring(
			jobloss.age.cat,
			unlist(regexpr(",", jobloss.age.cat)) + 1,
			nchar(levels(jobloss.age.cat)[as.numeric(jobloss.age.cat)]) - 1
		)))]

	dat[, x := apply(data.frame(lower, upper), 1 , median)]

	suicide.ggtab <- dat[,.(
		n = sum(`Left work`),
		outcome = sum(Suicide),
		x = x[1]
	), by = .(jobloss.age.cat)]

	suicide.ggtab[, `:=`(
		risk = outcome/n,
		upper.ci = outcome/n +
			qnorm(0.975) * sqrt(outcome/n * (1 - outcome/n)/n),
		lower.ci = outcome/n -
			qnorm(0.975) * sqrt(outcome/n * (1 - outcome/n)/n)
	)]

	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide', ifelse(in_ips,	ifelse(
			!recent, "_in_ips", "_recent"), ifelse(
				recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width, height = height)
	print(
		ggplot() +
			geom_pointrange(
				data = suicide.ggtab,
				aes(x = x,
						y = risk,
						ymin = lower.ci,
						ymax = upper.ci),
				size = 0.3,
				fatten = 0.3) +
			geom_rug(data = dat.og[,.(x = rep(jobloss.age, Suicide))],
							 aes(x = x, y = NULL)) +
			labs(y = "Risk of suicide",
					 x = "Age at worker exit") +
			mytheme
	)
	dev.off()
}

get.cruderisk_bycalendar <- function(
	in_ips = F,
	recent = T,
	width = 3.5,
	height = 2.75,
	get_rate = T,
	get_landscape = T,
	get_portrait = T,
	directory = here::here("reports/resources"),
	get_known_white = T
) {
	dir.create(directory, F, T)

	# Get appropariate data
	if (in_ips) {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_ips)) } else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
	} else {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_full))} else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
	}

	dat.og <- as.data.table(as.data.frame(dat))

	get.dat <- function(
		include95 = T,
		get.rate = T,
		known_white = F) {

		if (!include95) {
			dat <- dat.og[known_status == 1]
		} else {
			dat <- as.data.table(as.data.frame(dat.og))
		}

		if (known_white) {
			dat <- dat[finrace == 1]
		}

		dat <- dat[sex == "M"]

		# Risk calculation ####
		if (!get.rate) {
			dat <- dat[filler != 1, .(
				Suicide = max(Suicide),
				Overdose = max(Overdose),
				jobloss.date = jobloss.date[1],
				jobloss.age = time_length(difftime(jobloss.date[1], yob[1]),
																	'years'),
				yod = yod[1],
				yin = yin[1]
			), by = .(studyno)]

			baseline.dat <- as.data.table(as.data.frame(dat))

			dat <- rbindlist(
				lapply(sort(unique(year(dat$yod))), function(x) {
					dat[,.(
						`At risk` = sum((year(yod) >= x | is.na(yod)) & year(yin) <= (x - 3)),
						`Dead` = sum(year(yod) == x, na.rm = T),
						`Suicide` = sum(Suicide[year(yod) == x], na.rm = T),
						`Overdose` = sum(Overdose[year(yod) == x], na.rm = T),
						year = x
					)]
				})
			)

			dat[, year.cat := cut(
				year,
				if (in_ips | recent) {
					sort(unique(c(min(year), seq(1980, 2010, 5), max(year))))
				} else {
					sort(unique(c(min(year), seq(1955, 2010, 5), max(year))))
				},
				include.lowest = T, right = F)]

			dat[, `:=`(
				lower = as.numeric(substring(
					year.cat, 2,
					unlist(
						regexpr(",", year.cat)
					) - 1)),
				upper = as.numeric(substring(
					year.cat,
					unlist(regexpr(",", year.cat)) + 1,
					nchar(levels(year.cat)[as.numeric(year.cat)]) - 1
				)))]

			dat[, x := apply(data.frame(lower, upper), 1 , median)]


			# Poisson approximation
			dat[,`:=`(
				Despair = Suicide + Overdose)]
			dat[,`:=`(
				suicide.risk = Suicide/`At risk`,
				suicide.upper.ci = Suicide/`At risk` +
					qnorm(0.975) * sqrt(Suicide/`At risk` #* (1 - Suicide/`At risk`)
															/`At risk`),
				suicide.lower.ci = Suicide/`At risk` -
					qnorm(0.975) * sqrt(Suicide/`At risk` #* (1 - Suicide/`At risk`)
															/`At risk`),
				despair.risk = Despair/`At risk`,
				despair.upper.ci = Despair/`At risk` +
					qnorm(0.975) * sqrt(Despair/`At risk` #* (1 - Despair/`At risk`)
															/`At risk`),
				despair.lower.ci = Despair/`At risk` -
					qnorm(0.975) * sqrt(Despair/`At risk` #* (1 - Despair/`At risk`)
															/`At risk`)
			)]

			ggtab <- dat[,.(
				n = sum(`At risk`),
				Suicide = sum(Suicide),
				Despair = sum(Suicide + Overdose),
				x = x[1]
			), by = .(year.cat)]

			ggtab[, `:=`(
				suicide.risk = Suicide/n,
				suicide.upper.ci = Suicide/n +
					qnorm(0.975) * sqrt(Suicide/n #* (1 - Suicide/n)
															/n),
				suicide.lower.ci = Suicide/n -
					qnorm(0.975) * sqrt(Suicide/n #* (1 - Suicide/n)
															/n),
				despair.risk = Despair/n,
				despair.upper.ci = Despair/n +
					qnorm(0.975) * sqrt(Despair/n #* (1 - Despair/n)
															/n),
				despair.lower.ci = Despair/n -
					qnorm(0.975) * sqrt(Despair/n #* (1 - Despair/n)
															/n)
			)]

		} else {
			# Rate calculation ####
			dat <- dat[filler != 1]

			baseline.dat <- as.data.table(as.data.frame(dat))

			dat <- rbindlist(
				lapply(sort(unique(year(dat$yod))), function(x) {
					dat[, .(
						`At risk` = sum(py[year == x]),
						`Dead` = sum(year(yod) == x, na.rm = T),
						`Suicide` = sum(Suicide[year(yod) == x], na.rm = T),
						`Overdose` = sum(Overdose[year(yod) == x], na.rm = T),
						year = x
					)]
				})
			)

			dat[, year.cat := cut(
				year,
				if (in_ips | recent) {
					sort(unique(c(min(year), seq(1980, 2010, 5), max(year))))
				} else {
					sort(unique(c(min(year), seq(1955, 2010, 5), max(year))))
				},
				include.lowest = T, right = F)]

			dat[, `:=`(
				lower = as.numeric(substring(
					year.cat, 2,
					unlist(
						regexpr(",", year.cat)
					) - 1)),
				upper = as.numeric(substring(
					year.cat,
					unlist(regexpr(",", year.cat)) + 1,
					nchar(levels(year.cat)[as.numeric(year.cat)]) - 1
				)))]

			dat[, x := apply(data.frame(lower, upper), 1 , median)]

			# Rothman and Greenland method
			dat[,`:=`(
				Despair = Suicide + Overdose)]
			dat[,`:=`(
				suicide.risk = Suicide/`At risk`,
				suicide.upper.ci = exp(
					log(Suicide/`At risk`) + qnorm(0.975) * 1/sqrt(Suicide)),
				suicide.lower.ci = exp(
					log(Suicide/`At risk`) - qnorm(0.975) * 1/sqrt(Suicide)),
				despair.risk = Despair/`At risk`,
				despair.upper.ci = exp(
					log(Despair/`At risk`) + qnorm(0.975) * 1/sqrt(Despair)),
				despair.lower.ci = exp(
					log(Despair/`At risk`) - qnorm(0.975) * 1/sqrt(Despair))
			)]


			ggtab <- dat[,.(
				n = sum(`At risk`),
				Suicide = sum(Suicide),
				Despair = sum(Suicide + Overdose),
				x = x[1]
			), by = .(year.cat)]

			ggtab[, `:=`(
				suicide.risk = Suicide/n,
				suicide.upper.ci = exp(
					log(Suicide/n) + qnorm(0.975) * 1/sqrt(Suicide)),
				suicide.lower.ci = exp(
					log(Suicide/n) - qnorm(0.975) * 1/sqrt(Suicide)),
				despair.risk = Despair/n,
				despair.upper.ci = exp(
					log(Despair/n) + qnorm(0.975) * 1/sqrt(Despair)),
				despair.lower.ci = exp(
					log(Despair/n) - qnorm(0.975) * 1/sqrt(Despair))
			)]
		}

		if (include95) {
			dat[, include_95 := "B) All records"]
			baseline.dat[, include_95 := "B) All records"]
			ggtab[, include_95 := "B) All records"]
		} else {
			dat[, include_95 := "A) Complete work records"]
			baseline.dat[, include_95 := "A) Complete work records"]
			ggtab[, include_95 := "A) Complete work records"]
		}

		return(list(
			dat = as.data.table(as.data.frame(dat)),
			baseline.dat = as.data.table(as.data.frame(baseline.dat)),
			ggtab = as.data.table(as.data.frame(ggtab))
		))

	}

	# Get data ####
	dta <- get.dat(include95 = T)
	dat <- dta$dat
	baseline.dat <- dta$baseline.dat
	ggtab <- dta$ggtab

	if (get_known_white) {
		known_white.dta <- get.dat(include95 = T, known_white = T)
		known_white.dat <- known_white.dta$dat
		known_white.baseline.dat <- known_white.dta$baseline.dat
		known_white.ggtab <- known_white.dta$ggtab
			}

	# # Suicide ####
	# tikz(file = paste0(
	# 	directory,
	# 	paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide_by_calyear', ifelse(in_ips,	ifelse(
	# 		!recent, "_in_ips", "_recent"), ifelse(
	# 			recent, "_recent",	"")), '.tex')),
	# 	standAlone = T, width = width, height = height)
	# print(
	# ggplot() +
	# 	geom_pointrange(
	# 		data = ggtab,
	# 		aes(x = x,
	# 				y = suicide.risk * 100000,
	# 				ymin = suicide.lower.ci * 100000,
	# 				ymax = suicide.upper.ci * 100000),
	# 		size = 0.3,
	# 		fatten = 0.3) +
	# 	geom_smooth(data = dat,
	# 							aes(x = year,
	# 									y = suicide.risk * 100000),
	# 							method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
	# 	geom_rug(data = baseline.dat[Suicide == 1 ,.(x = rep(
	# 		year(yod) + time_length(difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
	# 		Suicide))],
	# 		aes(x = x, y = NULL)) +
	# 	labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " of suicide (per 100,000)"),
	# 			 x = "Year") +
	# 	scale_x_continuous(breaks = seq(
	# 		if_else((in_ips | recent), 1970, 1940), 2015, 10)) +
	# 	mytheme
	# )
	# dev.off()
	#
	# # Despair ####
	# tikz(file = paste0(
	# 	directory,
	# 	paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_despair_by_calyear', ifelse(in_ips,	ifelse(
	# 		!recent, "_in_ips", "_recent"), ifelse(
	# 			recent, "_recent",	"")), '.tex')),
	# 	standAlone = T, width = width, height = height)
	# print(
	# 	ggplot() +
	# 		geom_pointrange(
	# 			data = ggtab,
	# 			aes(x = x,
	# 					y = despair.risk * 100000,
	# 					ymin = despair.lower.ci * 100000,
	# 					ymax = despair.upper.ci * 100000),
	# 			size = 0.3,
	# 			fatten = 0.3) +
	# 		geom_smooth(data = dat,
	# 								aes(x = year,
	# 										y = Despair/`At risk` * 100000, color = "black"),
	# 								method = 'loess', alpha = 0.2, size = 0.3) +
	# 		geom_rug(data = baseline.dat[Overdose == 1 | Suicide == 1,.(x = rep(
	# 			year(yod) + time_length(difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
	# 			Suicide + Overdose))],
	# 			aes(x = x, y = NULL)) +
	# 		labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " of suicide  or fatal overdose (per 100,000)"),
	# 				 x = "Year") +
	# 		scale_x_continuous(breaks = seq(
	# 			if_else((in_ips | recent), 1970, 1940), 2015, 10)) +
	# 		mytheme
	# )
	# dev.off()

	if (get_known_white) {
	# Suicide/Despair: All records - Known white ####
	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide_despair_by_calyear_landscape',
					 ifelse(in_ips,	ifelse(
					 	!recent, "_in_ips", "_recent"), ifelse(
					 		recent, "_recent",	"")), "_all-records_known-white", '.tex')),
		standAlone = T, width = width * 1.85, height = height)
	print(
		ggplot() +
			geom_pointrange(
				data = rbindlist(
					list(known_white.ggtab[grepl("All records", include_95),.(
						outcome = "A) Suicide",
						x,
						risk = suicide.risk,
						lower.ci = suicide.lower.ci,
						upper.ci = suicide.upper.ci
					)],
					known_white.ggtab[grepl("All records", include_95),.(
						outcome = "B) Suicide and fatal overdose",
						x,
						risk = despair.risk,
						lower.ci = despair.lower.ci,
						upper.ci = despair.upper.ci
					)])),
				aes(x = x,
						y = risk * 100000,
						ymin = lower.ci * 100000,
						ymax = upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = rbindlist(
				list(known_white.dat[,.(
					outcome = "A) Suicide",
					year,
					n = Suicide,
					N = `At risk`
				)],
				known_white.dat[,.(
					outcome = "B) Suicide and fatal overdose",
					year,
					n = Suicide + Overdose,
					N = `At risk`
				)])
			),
			aes(x = year,
					y = n/N * 100000),
			method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = rbindlist(list(
				known_white.baseline.dat[Suicide == 1,.(
					x = rep(
						year(yod) + time_length(
							difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
						Suicide),
					outcome = "A) Suicide")],
				known_white.baseline.dat[Overdose == 1 | Suicide == 1,.(
					x = rep(
						year(yod) + time_length(
							difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
						Suicide + Overdose),
					outcome = "B) Suicide and fatal overdose")])),
				aes(x = x, y = NULL)) +
			facet_wrap(. ~ outcome, ncol = 2) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " (per 100,000)"),
					 x = "Year") +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940),2015, 10)) +
			coord_cartesian(ylim = c(-5, 50)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()
	}

	# Suicide/Despair: All records ####
	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide_despair_by_calyear_landscape',
					 ifelse(in_ips,	ifelse(
					 	!recent, "_in_ips", "_recent"), ifelse(
					 		recent, "_recent",	"")), "_all-records", '.tex')),
		standAlone = T, width = width * 1.85, height = height)
	print(
		ggplot() +
			geom_pointrange(
				data = rbindlist(
					list(ggtab[grepl("All records", include_95),.(
						outcome = "A) Suicide",
						x,
						risk = suicide.risk,
						lower.ci = suicide.lower.ci,
						upper.ci = suicide.upper.ci
					)],
					ggtab[grepl("All records", include_95),.(
						outcome = "B) Suicide and fatal overdose",
						x,
						risk = despair.risk,
						lower.ci = despair.lower.ci,
						upper.ci = despair.upper.ci
					)])),
				aes(x = x,
						y = risk * 100000,
						ymin = lower.ci * 100000,
						ymax = upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = rbindlist(
				list(dat[,.(
					outcome = "A) Suicide",
					year,
					n = Suicide,
					N = `At risk`
				)],
				dat[,.(
					outcome = "B) Suicide and fatal overdose",
					year,
					n = Suicide + Overdose,
					N = `At risk`
				)])
			),
			aes(x = year,
					y = n/N * 100000),
			method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = rbindlist(list(
				baseline.dat[Suicide == 1,.(
					x = rep(
						year(yod) + time_length(
							difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
						Suicide),
					outcome = "A) Suicide")],
				baseline.dat[Overdose == 1 | Suicide == 1,.(
					x = rep(
						year(yod) + time_length(
							difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
						Suicide + Overdose),
					outcome = "B) Suicide and fatal overdose")])),
				aes(x = x, y = NULL)) +
			facet_wrap(. ~ outcome, ncol = 2) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " (per 100,000)"),
					 x = "Year") +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940),2015, 10)) +
			coord_cartesian(ylim = c(-5, 50)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()

	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide_despair_by_calyear',
					 ifelse(in_ips,	ifelse(
					 	!recent, "_in_ips", "_recent"), ifelse(
					 		recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width, height = height * 1.85)
	print(
		ggplot() +
			geom_pointrange(
				data = rbindlist(
					list(ggtab[grepl("All records", include_95),.(
						outcome = "A) Suicide",
						x,
						risk = suicide.risk,
						lower.ci = suicide.lower.ci,
						upper.ci = suicide.upper.ci
					)],
					ggtab[grepl("All records", include_95),.(
						outcome = "B) Suicide and fatal overdose",
						x,
						risk = despair.risk,
						lower.ci = despair.lower.ci,
						upper.ci = despair.upper.ci
					)])),
				aes(x = x,
						y = risk * 100000,
						ymin = lower.ci * 100000,
						ymax = upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = rbindlist(
				list(dat[,.(
					outcome = "A) Suicide",
					year,
					n = Suicide,
					N = `At risk`
				)],
				dat[,.(
					outcome = "B) Suicide and fatal overdose",
					year,
					n = Suicide + Overdose,
					N = `At risk`
				)])
			),
			aes(x = year,
					y = n/N * 100000),
			method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = rbindlist(list(
				baseline.dat[Suicide == 1,.(
					x = rep(
						year(yod) + time_length(
							difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
						Suicide),
					outcome = "A) Suicide")],
				baseline.dat[Overdose == 1 | Suicide == 1,.(
					x = rep(
						year(yod) + time_length(
							difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
						Suicide + Overdose),
					outcome = "B) Suicide and fatal overdose")])),
				aes(x = x, y = NULL)) +
			facet_wrap(. ~ outcome, ncol = 1) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " (per 100,000)"),
					 x = "Year") +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940),2015, 10)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()

	# No 95ers ####
	no95.dta <- get.dat(include95 = F)

	no95.dat <- no95.dta$dat
	no95.baseline.dat <- no95.dta$baseline.dat
	no95.ggtab <- no95.dta$ggtab

	dat <- rbindlist(list(
		dat,
		no95.dat
	))

	baseline.dat <- rbindlist(list(
		baseline.dat,
		no95.baseline.dat
	))

	ggtab <- rbindlist(list(
		ggtab,
		no95.ggtab
	))

	# Suicide Portrait ####
	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide_by_calyear_by_status95', ifelse(in_ips,	ifelse(
			!recent, "_in_ips", "_recent"), ifelse(
				recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width, height = height * 1.85)
	print(
		ggplot() +
			geom_pointrange(
				data = ggtab,
				aes(x = x,
						y = suicide.risk * 100000,
						ymin = suicide.lower.ci * 100000,
						ymax = suicide.upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = dat,
									aes(x = year,
											y = Suicide/`At risk` * 100000),
									method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = baseline.dat[Suicide == 1 ,.(x = rep(
				year(yod) + time_length(difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
				Suicide))],
				aes(x = x, y = NULL)) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " of suicide (per 100,000)"),
					 x = "Year") +
			facet_wrap(. ~ include_95, ncol = 1) +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940), 2015, 10)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()

	# Suicide Landscape ####
	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_suicide_by_calyear_by_status95_landscape', ifelse(in_ips,	ifelse(
			!recent, "_in_ips", "_recent"), ifelse(
				recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width * 1.85, height = height)
	print(
		ggplot() +
			geom_pointrange(
				data = ggtab,
				aes(x = x,
						y = suicide.risk * 100000,
						ymin = suicide.lower.ci * 100000,
						ymax = suicide.upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = dat,
									aes(x = year,
											y = Suicide/`At risk` * 100000),
									method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = baseline.dat[Suicide == 1 ,.(x = rep(
				year(yod) + time_length(difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
				Suicide))],
				aes(x = x, y = NULL)) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " of suicide (per 100,000)"),
					 x = "Year") +
			facet_wrap(. ~ include_95, ncol = 2) +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940), 2015, 10)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()

	# Despair Portrait ####
	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_despair_by_calyear_by_status95', ifelse(in_ips,	ifelse(
			!recent, "_in_ips", "_recent"), ifelse(
				recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width, height = height * 1.85)
	print(
		ggplot() +
			geom_pointrange(
				data = ggtab,
				aes(x = x,
						y = despair.risk * 100000,
						ymin = despair.lower.ci * 100000,
						ymax = despair.upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = dat,
									aes(x = year,
											y = Despair/`At risk` * 100000),
									method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = baseline.dat[Overdose == 1 | Suicide == 1,.(x = rep(
				year(yod) + time_length(difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
				Suicide + Overdose))],
				aes(x = x, y = NULL)) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " of suicide  or fatal overdose (per 100,000)"),
					 x = "Year") +
			facet_wrap(. ~ include_95, ncol = 1) +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940), 2015, 10)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()

	# Despair Landscape ####
	tikz(file = paste0(
		directory,
		paste0('/crude_', paste0(ifelse(get_rate, 'rate', 'risk')), '_despair_by_calyear_by_status95_landscape', ifelse(in_ips,	ifelse(
			!recent, "_in_ips", "_recent"), ifelse(
				recent, "_recent",	"")), '.tex')),
		standAlone = T, width = width * 1.85, height = height)
	print(
		ggplot() +
			geom_pointrange(
				data = ggtab,
				aes(x = x,
						y = despair.risk * 100000,
						ymin = despair.lower.ci * 100000,
						ymax = despair.upper.ci * 100000),
				size = 0.3,
				fatten = 0.3) +
			geom_smooth(data = dat,
									aes(x = year,
											y = Despair/`At risk` * 100000),
									method = 'loess', alpha = 0.2, size = 0.3, color = "black") +
			geom_rug(data = baseline.dat[Overdose == 1 | Suicide == 1,.(x = rep(
				year(yod) + time_length(difftime(yod, as.Date(paste0(year(yod), "-01-01"))), 'years'),
				Suicide + Overdose))],
				aes(x = x, y = NULL)) +
			labs(y = paste0(ifelse(get_rate, "Rate", "Risk"), " of suicide  or fatal overdose (per 100,000)"),
					 x = "Year") +
			facet_wrap(. ~ include_95, ncol = 2) +
			scale_x_continuous(breaks = seq(
				if_else((in_ips | recent), 1970, 1940), 2015, 10)) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
	)
	dev.off()

}

# Follow-up chart ####
get.followup <- function(
	in_ips = F,
	recent = T,
	width = 5,
	height = 5.5,
	get_overdose = T,
	get_suicide = T,
	include_95 = F,
	directory = here::here("reports/resources")
) {
	dir.create(directory, F, T)

	# Get appropariate data
	if (!in_ips) {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_full))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	} else {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_ips))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	}
	dat.og <- dat

	dat <- dat.og[sex == "M"]

	if (!include_95) {
		dat <- dat[known_status == 1]
	}


	make.ggplot <- function(
		dat.ggplot = dat[Suicide == 1],
		outcome = "Suicide") {
		dat.ggplot[,`:=`(
			in_recent = ifelse(
				sex == "M" &
					immortal == 0 &
					year >= 1970 &
					year(jobloss.date) >= 1970 &
					time_length(difftime(yin, yob), 'years') < 65,
				"Yes",
				"No"),
			hire2death = time_length(difftime(
				yod, yin
			), 'years'),
			hire2leave = time_length(difftime(
				jobloss.date, yin
			), 'years'),
			died_on_job = ifelse(jobloss.date > yod,
													 "Died on the job",
													 "Died after worker exit")
		)]

		dat.ggplot[,studyno := 1:(.N)]

		dat.periodized <- rbindlist(list(
			dat.ggplot[,.(
				studyno,
				x = hire2leave,
				xend = hire2death,
				period = 'Not employed',
				hire2leave,
				hire2death,
				in_recent,
				jobloss.age.cat,
				died_while_employed
			)],
			dat.ggplot[,.(
				studyno,
				x = 0,
				xend = hire2leave,
				period = 'Employed',
				hire2leave,
				hire2death,
				in_recent,
				jobloss.age.cat,
				died_while_employed
			)]
		))

		dat.periodized[,period := factor(
			period, levels = c('Not employed', 'Employed')
		)]

		job_duration.ggplot <- ggplot(dat.periodized[
			order(hire2leave, studyno,
						decreasing = T)],
			aes(color = jobloss.age.cat)) +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period), size = 0.3) +
			geom_segment(aes(
				x = hire2leave,
				xend = hire2leave,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Years of follow-up (starting 3 years after hire)",
					 color = "Age at worker exit:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				nrow = 2,
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		job_duration_colorby_died_while_employed.ggplot <- ggplot(
			dat.periodized[
				order(hire2leave, studyno,
							decreasing = T)],
			aes(color = died_while_employed)) +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period), size = 0.3) +
			geom_segment(aes(
				x = hire2leave,
				xend = hire2leave,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Years of follow-up (starting 3 years after hire)",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		survival_after_jobloss.ggplot <- ggplot(dat.periodized[
			order(
				hire2leave - hire2death, studyno,
				decreasing = F)],
			aes(color = jobloss.age.cat)) +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = hire2leave,
				xend = hire2leave,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Years of follow-up (starting 3 years after hire)",
					 color = "Age at worker exit:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				nrow = 2,
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		survival_after_jobloss_colorby_died_while_employed.ggplot <- ggplot(dat.periodized[
			order(
				hire2leave - hire2death, studyno,
				decreasing = F)],
			aes(color = died_while_employed)) +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = hire2leave,
				xend = hire2leave,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Years of follow-up (starting 3 years after hire)",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-job_duration",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(job_duration.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-job_duration-employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(job_duration_colorby_died_while_employed.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-survival_after_jobloss",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(survival_after_jobloss.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-survival_after_jobloss-employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(survival_after_jobloss_colorby_died_while_employed.ggplot)
		dev.off()
	}

	bycalendar.ggplot <- function(
		dat.ggplot = dat[Suicide == 1],
		outcome = "Suicide") {
		dat.ggplot[,`:=`(
			in_recent = ifelse(
				sex == "M" &
					immortal == 0 &
					year >= 1970 &
					year(jobloss.date) >= 1970 &
					time_length(difftime(yin, yob), 'years') < 65,
				"Yes",
				"No"),
			died_on_job = ifelse(jobloss.date > yod,
													 "Died on the job",
													 "Died after worker exit")
		)]

		dat.ggplot[,studyno := 1:(.N)]

		dat.ggplot <- dat.ggplot[,.(
			studyno,
			yin = date.to.gm(yin) + 3,
			jobloss.date = date.to.gm(jobloss.date),
			yod = date.to.gm(yod),
			in_recent,
			died_while_employed,
			jobloss.age.cat
		)]

		dat.periodized <- rbindlist(list(
			dat.ggplot[,.(
				studyno,
				yin,
				jobloss.date,
				yod,
				in_recent,
				period = "Employed",
				x = yin, xend = jobloss.date,
				died_while_employed,
				jobloss.age.cat
			)],
			dat.ggplot[,.(
				studyno,
				yin,
				jobloss.date,
				yod,
				in_recent,
				period = "Not employed",
				x = jobloss.date, xend = yod,
				died_while_employed,
				jobloss.age.cat
			)]
		)
		)

		dat.periodized[, period := factor(
			period, levels = c("Not employed", "Employed")
		)]

		job_duration.ggplot <- ggplot(dat.periodized[
			order(jobloss.date - yin, studyno,
						decreasing = T)],
			aes(color = jobloss.age.cat)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Age at worker exit:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				nrow = 2,
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		job_duration_died_while_employed.ggplot <- ggplot(dat.periodized[
			order(jobloss.date - yin, studyno,
						decreasing = T)],
			aes(color = died_while_employed)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		survival_after_jobloss.ggplot <- ggplot(dat.periodized[
			order(
				jobloss.date - yod, studyno,
				decreasing = F)],
			aes(color = jobloss.age.cat)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Age at worker exit:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				nrow = 2,
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		survival_after_jobloss_died_while_employed.ggplot <- ggplot(
			dat.periodized[
				order(
					jobloss.date - yod, studyno,
					decreasing = F)],
			aes(color = died_while_employed)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		hire_date.ggplot <- ggplot(dat.periodized[
			order(
				yin, studyno,
				decreasing = F)],
			aes(color = jobloss.age.cat)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				position = 'identity',
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				position = 'identity',
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Age at worker exit:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				nrow = 2,
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		hire_date_died_while_employed.ggplot <- ggplot(dat.periodized[
			order(
				yin, studyno,
				decreasing = F)],
			aes(color = died_while_employed)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		jobloss_date.ggplot <- ggplot(dat.periodized[
			order(
				jobloss.date, studyno,
				decreasing = T)],
			aes(color = jobloss.age.cat)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Age at worker exit:",
					 linetype = "Employment status:") +
			guides(color = guide_legend(
				nrow = 2,
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		jobloss_date_died_while_employed.ggplot <- ggplot(
			dat.periodized[
				order(
					jobloss.date, studyno,
					decreasing = T)],
			aes(color = died_while_employed)) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			# geom_rect(aes(xmin = ifelse(recent | in_ips, 1970, 1941), xmax = 1995, ymin = -Inf, ymax = Inf, color = NULL), alpha = 0.01, fill = 'lightgrey') +
			geom_segment(aes(
				y = rep(1:length(table(studyno)), each = 2),
				yend = rep(1:length(table(studyno)), each = 2),
				x = x,
				xend = xend,
				linetype = period),
				size = 0.3) +
			geom_segment(aes(
				x = jobloss.date,
				xend = jobloss.date,
				y = rep(1:length(table(studyno)), each = 2) - 0.3,
				yend = rep(1:length(table(studyno)), each = 2) + 0.3),
				size = 0.3) +
			# scale_color_viridis(discrete = T) +
			mytheme +
			guides(color = guide_legend(
				override.aes = list(size = 0.7)),
				linetype = guide_legend(
					override.aes = list(size = 0.7))) +
			labs(y = paste(Hmisc::capitalize(outcome)),
					 x = "Year",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:") +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(axis.text.y = element_blank(),
						axis.ticks.y = element_blank(),
						legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"),
						panel.grid.major.y = element_blank(),
						panel.grid.minor.y = element_blank())

		tikz(file = paste0(
			directory,
			paste0("/followup-bycalendar-", tolower(outcome), "-job_duration",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(job_duration.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-bycalendar-", tolower(outcome), "-job_duration-employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(job_duration_died_while_employed.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-bycalendar-", tolower(outcome), "-survival_after_jobloss",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(survival_after_jobloss.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-bycalendar-", tolower(outcome), "-survival_after_jobloss-employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(survival_after_jobloss_died_while_employed.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-hire_date",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(hire_date.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-hire_date-employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(hire_date_died_while_employed.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-jobloss_date",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(jobloss_date.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/followup-", tolower(outcome), "-jobloss_date-employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width, height = ifelse(
			agrepl("suicide", outcome, ignore.case = T), height * 1.635, height))
		print(jobloss_date_died_while_employed.ggplot)
		dev.off()
	}


	if (get_overdose) {
		dat.overdose <- dat[Overdose == 1]
		make.ggplot(dat = dat.overdose, outcome = 'overdose')
		bycalendar.ggplot(dat = dat.overdose, outcome = 'overdose')
	}

	if (get_suicide) {
		dat.suicide <- dat[Suicide == 1]
		make.ggplot(dat = dat.suicide, outcome = 'suicide')
		bycalendar.ggplot(dat = dat.suicide, outcome = "suicide")
	}

}

get.lexis <- function(
	in_ips = F,
	recent = T,
	width = 5.5,
	height = 5.5,
	get_overdose = T,
	get_suicide = T,
	include_95 = F,
	directory = here::here("reports/resources")
) {
	dir.create(directory, F, T)

	# Get appropariate data
	if (!in_ips) {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_full))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	} else {
		if (!recent) {
			dat <- as.data.table(as.data.frame(cohort_ips))
		} else {
			dat <- as.data.table(as.data.frame(cohort_recent))
		}
	}
	dat.og <- dat

	dat <- dat.og[sex == "M"]

	if (!include_95) {
		dat <- dat[known_status == 1]
	}


	make.ggplot <- function(
		dat.ggplot = dat[Suicide == 1],
		outcome = "Suicide"
	) {

		dat.ggplot[, in_recent := ifelse(
			sex == "M" &
				immortal == 0 &
				year >= 1970 &
				year(jobloss.date) >= 1970 &
				time_length(difftime(yin, yob), 'years') < 65,
			"Yes",
			"No")]

		dat.ggplot <- dat.ggplot[, .(
			in_recent = in_recent[1],
			died_while_employed = died_while_employed[1],
			jobloss.age.cat = jobloss.age.cat[1],
			Calendar = c(
				start = date.to.gm(yin[1] + years(3)),
				jobloss = date.to.gm(jobloss.date[1]),
				death = date.to.gm(yod[1])),
			Age = c(
				start = time_length(difftime(
					yin[1] + years(3), yob[1]), 'years'),
				jobloss = time_length(difftime(
					jobloss.date[1], yob[1]), 'years'),
				death = time_length(difftime(
					yod[1], yob[1]), 'years')),
			Period = c("start", "jobloss", "death")
		), by = .(studyno)]

		dat.periodized <- rbindlist(list(
			dat.ggplot[,.(
				in_recent,
				died_while_employed,
				jobloss.age.cat,
				x = Calendar[Period == "start"],
				xend = Calendar[Period == "jobloss"],
				y = Age[Period == "start"],
				yend = Age[Period == "jobloss"],
				Period = "Employed"
			), by = .(studyno)],
			dat.ggplot[,.(
				in_recent,
				died_while_employed,
				jobloss.age.cat,
				x = Calendar[Period == "jobloss"],
				xend = Calendar[Period == "death"],
				y = Age[Period == "jobloss"],
				yend = Age[Period == "death"],
				Period = "Not employed"
			), by = .(studyno)]
		))

		dat.periodized[, Period := factor(
			Period, levels = c('Not employed', 'Employed')
		)]

		dat.periodized[, pend := factor(
			Period, levels = c('Employed', 'Not employed'),
			labels = c("End of employment", "Deceased")
		)]

		lexis.ggplot <- ggplot(
			dat.periodized,
			aes(color = jobloss.age.cat,
					x = x, xend = xend,
					y = y, yend = yend,
					# linetype = Period,
					group = studyno)
		) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			geom_segment(size = 0.1) +
			geom_point(
				#data = dat.periodized[Period == "Employed"],
				aes(x = xend, y = yend,
						shape = pend,
						color = jobloss.age.cat),
				size = 0.05) +
			scale_shape_manual(values = c(4, 1)) +
			# scale_color_viridis(discrete = T) +
			scale_color_manual(values = c(
				rgb(68, 1, 84, max = 255),
				rgb(49, 104, 142, max = 255),
				rgb(53, 183, 121, max = 255),
				rgb(255, 210, 5, max = 255)
			)) +
			mytheme +
			labs(y = "Age",
					 x = "Year",
					 color = "Age at worker exit:",
					 linetype = "Employment status:",
					 shape = "Event:") +
			guides(
				color = guide_legend(
					override.aes = list(size = 1),
					nrow = 2),
				shape = guide_legend(
					override.aes = list(size = 1))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"))

		lexis_died_while_employed.ggplot <- ggplot(
			dat.periodized,
			aes(color = died_while_employed,
					x = x, xend = xend,
					y = y, yend = yend,
					# linetype = Period,
					group = studyno)
		) +
			# geom_vline(aes(xintercept = 2015.99),
			# 					 linetype = "dotted") +
			geom_segment(size = 0.1) +
			geom_point(
				#data = dat.periodized[Period == "Employed"],
				aes(x = xend, y = yend,
						shape = pend,
						color = died_while_employed),
				size = 0.05) +
			scale_shape_manual(values = c(4, 1)) +
			mytheme +
			labs(y = "Age",
					 x = "Year",
					 color = "Employment status at time of death:",
					 linetype = "Employment status:",
					 shape = "Event:") +
			guides(
				color = guide_legend(
					override.aes = list(size = 1)),
				shape = guide_legend(
					override.aes = list(size = 1))) +
			scale_x_continuous(breaks = seq(1940, 2015, 10)) +
			theme(legend.position = 'bottom',
						legend.box = "vertical",
						legend.spacing.y = unit(-7, "pt"))


		tikz(file = paste0(
			directory,
			paste0("/lexis-", tolower(outcome),
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width,
		# ifelse(
		# 	agrepl("Overdose", outcome, ignore.case = T),
		# 	width, width * 1.5),
		height = height
		# ifelse(
		# 	agrepl("Overdose", outcome, ignore.case = T),
		# 	height, height * 1.5)
		)
		print(lexis.ggplot)
		dev.off()

		tikz(file = paste0(
			directory,
			paste0("/lexis-", tolower(outcome), "-_employment_status",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "", "_recent")),
						 # ifelse(is.finite(years.max),
						 # 			 paste0("_", years.max),
						 # 			 ""),
						 # ifelse(include_women,
						 # 			 "_women",
						 # 			 ""),
						 ".tex")
		), standAlone = T,
		width = width,
		# ifelse(
		# 	agrepl("Overdose", outcome, ignore.case = T),
		# 	width, width * 1.5),
		height = height
		# ifelse(
		# 	agrepl("Overdose", outcome, ignore.case = T),
		# 	height, height * 1.5)
		)
		print(lexis_died_while_employed.ggplot)
		dev.off()
	}

	if (get_overdose) {
		dat.overdose <- dat[Overdose == 1]
		make.ggplot(dat = dat.overdose, outcome = 'overdose')
	}

	if (get_suicide) {
		dat.suicide <- dat[Suicide == 1]
		make.ggplot(dat = dat.suicide, outcome = 'suicide')
	}

}


# Cox modeling ####
get.coxph <- function(in_ips = F,
											recent = T,
											years.max = NA,
											rebuild_cohort_leftwork = F,
											get_despair = T,
											get_suicide = T,
											ref_age = ref.age,
											include_women = F,
											days_to_suicide.min = NULL,
											to_add = F,
											print_coef = T,
											age_cat_by_quant = F,
											use_finrace = F,
											run_model = T,
											run_categorical = T,
											run_spline = T,
											run_finegray = F,
											run_cause_specific = F,
											directory = to_drive_D(here::here("despair/resources"))
											) {
	require(survival)

	sapply(c('dta', 'exposure', grep('jobhist', ls(), value = T)),
				 function(x) {
				 	if (x %in% ls(envir = .GlobalEnv)) {
				 		rm(list = x, envir = .GlobalEnv)
				 	}})

	if (rebuild_cohort_leftwork) {
		if (!in_ips) {
			if (!recent) {
				dat <- as.data.table(as.data.frame(cohort_full))
			} else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
		} else {
			if (!recent) {
				dat <- as.data.table(as.data.frame(cohort_ips))
			} else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
		}


		dat <- dat[filler == 0 & known_status == 1]

		# Better names for finrace
		dat[,finrace := factor(finrace,
													 levels = c(1, 2, 9),
													 labels = c("White", "Black", "Unknown"))]

		suicide.who <- unique(dat[Suicide == 1, studyno])
		overdose.who <- unique(dat[Overdose == 1, studyno])

		if (to_add) {
			# Minimum years since leave work among those who died after job loss
			if (is.null(days_to_suicide.min)) {
				since.leavework2.suicide.min <- min(dat[
					jobloss.date <= yod &
						Suicide == 1, .(
							since.leavework2 = time_length(
								difftime(yod + days(1), jobloss.date),
								'days')
						)])
				since.leavework2.suicide.min } else {
					since.leavework2.suicide.min <- days_to_suicide.min
				}

			if (is.null(days_to_suicide.min)) {
				since.leavework2.overdose.min <- min(dat[
					jobloss.date <= yod &
						Overdose == 1, .(
							since.leavework2 = time_length(
								difftime(yod + days(1), jobloss.date),
								'days')
						)])
				since.leavework2.overdose.min } else {
					since.leavework2.overdose.min <- days_to_overdose.min
				}

			# Give "baseline" suicide cases more person-time
			dat[jobloss.date > yod & studyno %in% suicide.who, `:=`(
				suicide.to_extend = 1
			)]

			to.add <- as.data.table(as.data.frame(dat[suicide.to_extend == 1 & Suicide == 1]))

			if (nrow(to.add) > 0) {
				to.add[, new_yod := jobloss.date + days(
					ceiling(since.leavework2.suicide.min))]
				to.add[,`:=`(nrows = length(year(new_yod):year(yod)) - 1),
							 by = .(studyno)]
				to.add <- to.add[rep(1:.N, nrows),]

				if (nrow(to.add) > 0) {
					setorder(to.add, studyno)

					to.add[, `:=`(
						year = as.integer(seq(year + 1, length.out = nrows)),
						Suicide = as.double(c(seq(0, length.out = nrows - 1), 1)),
						Despair = as.double(c(seq(0, length.out = nrows - 1), 1)),
						yearwork = seq(yearwork + 1, length.out = nrows)
					), by = .(studyno)]

					dat <- rbindlist(list(
						dat, to.add[,-c('nrows', "new_yod")]
					))

					setorder(dat, studyno, year)
				}}

			dat[suicide.to_extend == 1, `:=`(
				yod = jobloss.date + days(
					ceiling(since.leavework2.suicide.min))
			),
			by = .(studyno)]

			dat[suicide.to_extend == 1, Suicide := ifelse(year == year(yod), 1, 0)]
			dat[suicide.to_extend == 1,`:=`(
				age.year1 = floor(time_length(difftime(as.Date(
					paste0(year, "-01-01")), yob), 'day')),
				age.year2 = floor(time_length(difftime(as.Date(
					paste0(year + 1, "-01-01")), yob), 'day'))
			)]
			dat[suicide.to_extend == 1 & year == year(yod), `:=`(
				age.year2 = floor(time_length(difftime(yod + days(1), yob), 'day')))]

			# Give "baseline" overdose caes more person-time
			dat[jobloss.date > yod & studyno %in% overdose.who, `:=`(
				overdose.to_extend = 1
			)]

			to.add <- as.data.table(as.data.frame(dat[overdose.to_extend == 1 & Overdose == 1]))

			if (nrow(to.add) > 0) {
				to.add[, new_yod := jobloss.date + days(
					ceiling(since.leavework2.overdose.min))]
				to.add[,`:=`(nrows = length(year(new_yod):year(yod)) - 1),
							 by = .(studyno)]
				to.add <- to.add[rep(1:.N, nrows),]

				if (nrow(to.add) > 0) {
					setorder(to.add, studyno)

					to.add[, `:=`(
						year = as.integer(seq(year + 1, length.out = nrows)),
						Overdose = as.double(c(seq(0, length.out = nrows - 1), 1)),
						Despair = as.double(c(seq(0, length.out = nrows - 1), 1)),
						yearwork = seq(yearwork + 1, length.out = nrows)
					), by = .(studyno)]

					dat <- rbindlist(list(
						dat, to.add[,-c('nrows', "new_yod")]
					))

					setorder(dat, studyno, year)
				}}

			dat[overdose.to_extend == 1, `:=`(
				yod = jobloss.date + days(
					ceiling(since.leavework2.overdose.min))
			),
			by = .(studyno)]

			dat[overdose.to_extend == 1, Overdose := ifelse(year == year(yod), 1, 0)]
			dat[overdose.to_extend == 1,`:=`(
				age.year1 = floor(time_length(difftime(as.Date(
					paste0(year, "-01-01")), yob), 'day')),
				age.year2 = floor(time_length(difftime(as.Date(
					paste0(year + 1, "-01-01")), yob), 'day'))
			)]
			dat[overdose.to_extend == 1 & year == year(yod), `:=`(
				age.year2 = floor(time_length(difftime(yod + days(1), yob), 'day')))]
		}

		setorder(dat, studyno, year)
		# dat[(Suicide == 1 | Overdose == 1) & Despair != 1]
		# dat[filler == 0,.(n = sum(Despair)), by = .(studyno)][n > 1]
		# dat[filler == 0,.(n = sum(Suicide)), by = .(studyno)][n > 1]

		dat <- dat[
			# Start follow-up in year of job loss
			year >= year(jobloss.date) &
				# Remove cases that occurred before jobloss date
				(is.na(yod) | yod > jobloss.date)]

		# Make time and variables
		dat[, `:=`(
			since.leavework1 = time_length(
				difftime(as.Date(paste0(year, "-01-01")),
								 jobloss.date),
				'days'),
			since.leavework2 = time_length(
				difftime(apply(data.frame(
					as.Date(paste0(year + 1, "-01-01")),
					yod + days(1),
					yoc + days(1)),
					1, min, na.rm = T),
					jobloss.date),
				'days'),
			jobloss.age = time_length(
				difftime(jobloss.date, yob),
				'years'),
			yin.num = as.numeric(yin, origin = "1970-01-01"),
			jobloss.date.num = as.numeric(jobloss.date, origin = '1970-01-01'),
			hire.age = time_length(difftime(yin, yob), 'years')
		)]

		# First person-year start at 0 for all folks
		dat[year == year(jobloss.date), `:=`(
			since.leavework1 = 0
		)]

		# Special jobloss.age for splined model
		# (want risk to be constant for those of the referent age group)
		dat[,`:=`(
			jobloss.age.restricted = {
				x <- jobloss.age
				x[x >= ref_age] <- ref_age
				x}
		)]

		# # Folks who died within a day of the first year
		# dat[(since.leavework2 - since.leavework1) <= 1, `:=`(
		# 	since.leavework2 = as.numeric(
		# 		as.Date(since.leavework1, origin = '1970-01-01') + days(1))
		# )]

		# Capitalize `Sex`
		dat[,Sex := sex]

		# Correct false precision
		dat[,`:=`(
			since.leavework1 = floor(since.leavework1),
			since.leavework2 = floor(since.leavework2)
		)]

		# Save prepped analytic file to global environment
		assign(paste0(
			'cohort_leftwork',
			ifelse(in_ips,
						 ifelse(!recent,
						 			 "_in_ips",
						 			 "_recent"),
						 ifelse(!recent,
						 			 "_in_full",
						 			 "_recent"))),
			dat,
			envir = .GlobalEnv)
	} else {
		dat <- as.data.table(
			as.data.frame(get(paste0('cohort_leftwork', ifelse(in_ips, ifelse(
				!recent, "_in_ips", "_recent"), ifelse(!recent, "_in_full", "_recent"))))
			))
	}

	if (run_model) {
		dir.create(directory, F, T)

		if (!include_women) {
			dat <- dat[Sex == "M"]
		}

		if (get_despair) {
			# Despair model ####
			if (age_cat_by_quant) {
				dat[,`:=`(
					`Age at job loss` = cut(jobloss.age, c(
						min(jobloss.age),
						floor(quantile(jobloss.age[Despair == 1 & jobloss.age < ref_age],
													 c(1/3, 2/3))),
						ref_age,
						max(jobloss.age)),
						right = F, include.lowest = T
					)
				)]
				dat[,`:=`(
					`Age at job loss` = factor(
						`Age at job loss`, levels = levels(`Age at job loss`)[
							length(table(`Age at job loss`)):1]
					))]
			} else {
				dat[,`:=`(
					`Age at job loss` = cut(jobloss.age, c(
						19, 30, 40 , 55, max(jobloss.age)),
						right = F, include.lowest = T
					)
				)]
				dat[,`:=`(
					`Age at job loss` = factor(
						`Age at job loss`, levels = levels(`Age at job loss`)[
							length(table(`Age at job loss`)):1]
					))]
			}

			# Age at hire categories
			hire.age.cutpoints <- quantile(
				dat[Despair == 1, hire.age],
				seq(0, 1, 0.2)
			)

			hire.age.cutpoints <- unique(c(
				floor(min(dat$hire.age)),
				round(hire.age.cutpoints)[-c(1, length(hire.age.cutpoints))],
				ceiling(max(dat$hire.age))
			))

			dat[,`:=`(
				hire.age.cat = cut(hire.age, hire.age.cutpoints,
													 right = F, include.lowest = T)
			)]

			# Year of hire hire categories
			yin.cutpoints <- quantile(dat[Despair == 1, date.to.gm(yin)],
																seq(0, 1, 0.25))

			yin.cutpoints <- unique(c(
				floor(date.to.gm(min(dat$yin))),
				round(yin.cutpoints)[-c(1, length(yin.cutpoints))],
				ceiling(date.to.gm(max(dat$yin)))))

			dat[,`:=`(
				yin.cat = cut(date.to.gm(yin),
											yin.cutpoints,
											include.lowest = T,	right = F)
			)]

			if (run_categorical) {
				# Despair categorical model ####
				leftwork_despair.coxph <<- coxph(
					as.formula(paste(
						"Surv(since.leavework1, since.leavework2,
						 Despair) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}}
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_despair.coxph)$coefficients)[,.(
						name = rownames(summary(leftwork_despair.coxph)$coefficients),
						`HR of despair` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_despair.coxph,
								file = paste0(
									directory, paste0(
										'/leftwork_despair',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))
			}

			if (run_cause_specific) {
				# Not Despair categorical model ####
				dat[, `:=`(`Not despair` = {
					tmp <- `All causes`
					tmp[Despair == 1] <- 0
					tmp
				})]
				leftwork_not_despair.coxph <<- coxph(
					as.formula(paste(
						"Surv(since.leavework1, since.leavework2,
						 `Not despair`) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}}
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_not_despair.coxph)$coefficients)[,.(
						name = rownames(summary(leftwork_not_despair.coxph)$coefficients),
						`HR of despair` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_not_despair.coxph,
								file = paste0(
									directory, paste0(
										'/leftwork_not_despair',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))
			}

			if (run_spline) {
				# Despair splined model ####
				splined_leftwork_despair.coxph <<- coxph(
					as.formula(paste(
						"Surv(since.leavework1, since.leavework2,
						 Despair) ~
					pspline(jobloss.age.restricted, df = 4) +
					pspline(jobloss.date.num, df = 0) +",
						# "hire.age.cat +"
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = {if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}}
				)

				saveRDS(splined_leftwork_despair.coxph,
								file = paste0(
									directory, paste0(
										'/splined_leftwork_despair',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))
			}

			if (run_finegray) {
				# Despair FG model ####
				dat[,`:=`(fgstatus = factor(ifelse(
					Despair == 1,
					1,
					ifelse(`All causes` == 1, 2, 0)),
					0:2,
					c("censor", "despair", "death")))]
				despair.fg.dat <- finegray(
					Surv(since.leavework1, since.leavework2,
							 fgstatus) ~ .,
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}},
					etype = "despair",
					id = studyno
				)
				not_despair.fg.dat <- finegray(
					Surv(since.leavework1, since.leavework2,
							 fgstatus) ~ .,
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}},
					etype = "death",
					id = studyno
				)

				# Despair ####
				leftwork_despair.fg <<- coxph(
					as.formula(paste(
						"Surv(fgstart, fgstop, fgstatus) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	despair.fg.dat,
					weight = fgwt
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_despair.fg)$coefficients)[,.(
						name = rownames(summary(leftwork_despair.fg)$coefficients),
						`sdHR despair` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_despair.fg,
								file = paste0(
									directory, paste0(
										'/leftwork_despair',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.fg.rds')
								))

				# Not Despair ####
				leftwork_not_despair.fg <<- coxph(
					as.formula(paste(
						"Surv(fgstart, fgstop, fgstatus) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	not_despair.fg.dat,
					weight = fgwt
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_not_despair.fg)$coefficients)[,.(
						name = rownames(summary(leftwork_not_despair.fg)$coefficients),
						`sdHR death not despair` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_not_despair.fg,
								file = paste0(
									directory, paste0(
										'/leftwork_not_despair',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.fg.rds')
								))
			}

		}

		if (get_suicide) {
			# Suicide model ####
			if (age_cat_by_quant) {
				dat[,`:=`(
					`Age at job loss` = cut(jobloss.age, c(
						min(jobloss.age),
						floor(quantile(jobloss.age[Suicide == 1 & jobloss.age < ref_age],
													 c(1/3, 2/3))),
						ref_age,
						max(jobloss.age)),
						right = F, include.lowest = T
					)
				)]
				dat[,`:=`(
					`Age at job loss` = factor(
						`Age at job loss`, levels = levels(`Age at job loss`)[
							length(table(`Age at job loss`)):1]
					))]
			} else {
				dat[,`:=`(
					`Age at job loss` = cut(
						jobloss.age, c(19, 30, 40, 55, max(jobloss.age)),
						right = F, include.lowest = T
					)
				)]
				dat[,`:=`(
					`Age at job loss` = factor(
						`Age at job loss`, levels = levels(`Age at job loss`)[
							length(table(`Age at job loss`)):1]
					))]
			}

			# Age at hire categories
			hire.age.cutpoints <- quantile(
				dat[Suicide == 1, hire.age],
				seq(0, 1, 0.2)
			)

			hire.age.cutpoints <- unique(c(
				floor(min(dat$hire.age)),
				round(hire.age.cutpoints)[-c(1, length(hire.age.cutpoints))],
				ceiling(max(dat$hire.age))
			))

			dat[,`:=`(
				hire.age.cat = cut(hire.age, hire.age.cutpoints,
													 right = F, include.lowest = T)
			)]

			# Year of hire hire categories
			yin.cutpoints <- quantile(dat[Suicide == 1, date.to.gm(yin)],
																seq(0, 1, 0.25))

			yin.cutpoints <- unique(c(
				floor(date.to.gm(min(dat$yin))),
				round(yin.cutpoints)[-c(1, length(yin.cutpoints))],
				ceiling(date.to.gm(max(dat$yin)))))

			dat[,`:=`(
				yin.cat = cut(date.to.gm(yin),
											yin.cutpoints,
											include.lowest = T,	right = F)
			)]

			if (run_finegray) {
				fg.dat <- as.data.table(as.data.frame(dat))
				setorder(fg.dat, studyno, year)
				fg.dat[,`:=`(
					I = 1:.N,
					N = .N
				), by = .(studyno)]
			}

			if (run_categorical) {
				# Suicide categorical model ####
				leftwork_suicide.coxph <<- coxph(
					as.formula(paste(
						"Surv(since.leavework1, since.leavework2,
						 Suicide) ~
					`Age at job loss` +
					pspline(jobloss.date, df = 0) +",
						# "hire.age.cat +"
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = {if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}}
				)

				if (print_coef){
					print(as.data.table(summary(leftwork_suicide.coxph)$coefficients)[,.(
						name = rownames(summary(leftwork_suicide.coxph)$coefficients),
						`HR of suicide` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_suicide.coxph,
								file = paste0(
									directory, paste0(
										'/leftwork_suicide',
										ifelse(
											in_ips,
											ifelse(!recent, "_in_ips", "_recent"),
											ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))
			}

			if (run_cause_specific) {
				# Not Suicide categorical model ####
				dat[, `:=`(`Not suicide` = {
					tmp <- `All causes`
					tmp[Suicide == 1] <- 0
					tmp
				})]
				leftwork_not_suicide.coxph <<- coxph(
					as.formula(paste(
						"Surv(since.leavework1, since.leavework2,
						 `Not suicide`) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}}
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_not_suicide.coxph)$coefficients)[,.(
						name = rownames(summary(leftwork_not_suicide.coxph)$coefficients),
						`HR of suicide` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_not_suicide.coxph,
								file = paste0(
									directory, paste0(
										'/leftwork_not_suicide',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))
			}

			if (run_spline) {
				# Suicide splined model ####s
				splined_leftwork_suicide.coxph <<- coxph(
					as.formula(paste(
						"Surv(since.leavework1, since.leavework2,
						 Suicide) ~
					pspline(jobloss.age.restricted, df = 4) +
					pspline(jobloss.date.num, df = 0) +",
						# "hire.age.cat +"
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = {if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}}
				)

				saveRDS(splined_leftwork_suicide.coxph,
								file = paste0(
									directory, paste0(
										'/splined_leftwork_suicide',
										ifelse(
											in_ips,
											ifelse(!recent, "_in_ips", "_recent"),
											ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))
			}

			if (run_finegray) {
				# Suicide FG model ####
				dat[,`:=`(fgstatus = factor(ifelse(
					Suicide == 1,
					1,
					ifelse(`All causes` == 1, 2, 0)),
					0:2,
					c("censor", "suicide", "death")))]
				suicide.fg.dat <- finegray(
					Surv(since.leavework1, since.leavework2,
							 fgstatus) ~ .,
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}},
					etype = "suicide",
					id = studyno
				)
				not_suicide.fg.dat <- finegray(
					Surv(since.leavework1, since.leavework2,
							 fgstatus) ~ .,
					data = 	{if (is.finite(years.max)) {
						as.data.table(
							as.data.frame(
								dat[since.leavework2 <= years.max * 365.25]))
					} else {dat}},
					etype = "death",
					id = studyno
				)

				# Suicide ####
				leftwork_suicide.fg <<- coxph(
					as.formula(paste(
						"Surv(fgstart, fgstop, fgstatus) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	suicide.fg.dat,
					weight = fgwt
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_suicide.fg)$coefficients)[,.(
						name = rownames(summary(leftwork_suicide.fg)$coefficients),
						`sdHR suicide` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_suicide.fg,
								file = paste0(
									directory, paste0(
										'/leftwork_suicide',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.fg.rds')
								))

				# Not Suicide ####
				leftwork_not_suicide.fg <<- coxph(
					as.formula(paste(
						"Surv(fgstart, fgstop, fgstatus) ~
					`Age at job loss` +
					pspline(jobloss.date.num, df = 0) +",
						# hire.age.cat +
						"plant +", ifelse(use_finrace, "finrace", "race"),
						ifelse(include_women, "+ Sex", ""))),
					data = 	not_suicide.fg.dat,
					weight = fgwt
				)

				if (print_coef) {
					print(as.data.table(summary(leftwork_not_suicide.fg)$coefficients)[,.(
						name = rownames(summary(leftwork_not_suicide.fg)$coefficients),
						`sdHR death not suicide` = exp(coef),
						lower = exp(coef - `se(coef)` * 1.96),
						upper = exp(coef + `se(coef)` * 1.96)
					)]
					)
				}

				saveRDS(leftwork_not_suicide.fg,
								file = paste0(
									directory, paste0(
										'/leftwork_not_suicide',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.fg.rds')
								))
			}



		}}

}

get.simplecoxph <- function(in_ips = F,
														recent = T,
														years.max = NA,
														get_despair = T,
														get_suicide = T,
														ref_age = ref.age,
														include_women = F,
														baseline_sinceleavework.max = NA,
														rebuild_cohort_sincehire = F,
														use_finrace = F,
														py_rows = F,
														run_model = T,
														run_categorical = T,
														run_finegray = F,
														directory = to_drive_D(here::here("despair/resources"))
																															) {
	require(survival)
	if (run_model | run_categorical | run_finegray) {
		dir.create(directory, F, T)
	}

	sapply(c('dta', 'exposure', grep('jobhist', ls(), value = T)),
				 function(x) {
				 	if (x %in% ls(envir = .GlobalEnv)) {
				 		rm(list = x, envir = .GlobalEnv)
				 	}})

	if (rebuild_cohort_sincehire) {
		if (!in_ips) {
			if (!recent) {
				dat <- as.data.table(as.data.frame(cohort_full))
			} else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
		} else {
			if (!recent) {
				dat <- as.data.table(as.data.frame(cohort_ips))
			} else {
				dat <- as.data.table(as.data.frame(cohort_recent))
			}
		}

		dat <- dat[filler != 1 & known_status == 1]

		# Better names for finrace
		dat[,finrace := factor(finrace,
													 levels = c(1, 2, 9),
													 labels = c("White", "Black", "Unknown"))]

		dat <- dat[age.year1 < age.year2]
		dat[, hire.age := time_length(difftime(yin, yob), 'years')]

		# No job loss after death
		dat[,`Job loss` := ifelse(
			year >= year(jobloss.date + days(1)), 1, 0)]
		dat[yod <= jobloss.date,
				`Job loss` := 0]

		# Make year of hire numeric
		dat[, yin.gm := date.to.gm(yin)]

		# Categorical yin for Despair
		yin.cutpoints <- quantile(
			dat[Despair == 1, yin.gm],
			seq(0, 1, 1/4))
		yin.cutpoints <- c(floor(min(dat[, yin.gm])),
											 round(yin.cutpoints[-c(1, length(yin.cutpoints))]),
											 ceiling(max(dat[, yin.gm])))

		dat[, despair_yin.cat := cut(
			yin.gm, yin.cutpoints, include.lowest = T,
			dig.lab = 4, right = F)]

		# Categorical yin for Suicide
		yin.cutpoints <- quantile(
			dat[Suicide == 1, yin.gm],
			seq(0, 1, 1/4))
		yin.cutpoints <- c(floor(min(dat[, yin.gm])),
											 round(yin.cutpoints[-c(1, length(yin.cutpoints))]),
											 ceiling(max(dat[, yin.gm])))

		dat[, suicide_yin.cat := cut(
			yin.gm, yin.cutpoints, include.lowest = T,
			dig.lab = 4, right = F)]

		assign(paste0('cohort_sincehire',
									ifelse(in_ips, ifelse(!recent, "_in_ips", "_recent"),
												 ifelse(!recent, "_in_full", "_recent"))),
					 dat,
					 envir = .GlobalEnv)

	} else {
		dat <- as.data.table(as.data.frame(get(paste0('cohort_sincehire',
																									ifelse(in_ips, ifelse(!recent, "_in_ips", "_recent"),
																												 ifelse(!recent, "_in_full", "_recent"))))))
	}

	setorder(dat, studyno, year)
	dat[,`:=`(
		I = 1:.N,
		N = .N
	), by = .(studyno)]

	# Exclude women
	if (!include_women) {
		dat <- dat[sex == "M"]
	}

	# Maximum follow-up time
	if (is.finite(years.max)) {
		dat <- dat[time_length(difftime(as.Date(paste0(year, "-01-01")),
																		jobloss.date), 'years') <= years.max]
	}

	# make a numeric jobloss.date
	dat[, jobloss.date.gm := date.to.gm(jobloss.date)]

	# Move up job loss date if close to time of death
	suicide.who <- unique(dat[Suicide == 1, studyno])
	overdose.who <- unique(dat[Overdose == 1, studyno])

	if (is.finite(baseline_sinceleavework.max)) {

		dat_suicide <- as.data.table(as.data.frame(dat))
		dat_suicide[
			studyno %in% suicide.who &
				time_length(difftime(yod, jobloss.date), 'days') < baseline_sinceleavework.max &
				yod > jobloss.date, `:=`(
					jobloss.date = yod,
					changed_jobloss.date = 1)]

		dat_suicide[,`Job loss` := ifelse(
			year >= year(jobloss.date + days(1)), 1, 0)]
		dat_suicide[yod <= jobloss.date,
								`Job loss` := 0]

		dat_despair <- as.data.table(as.data.frame(dat))
		dat_despair[,`Job loss` := ifelse(
			year >= year(jobloss.date + days(1)), 1, 0)]
		dat_despair[yod <= jobloss.date,
								`Job loss` := 0]

	} else {
		dat_suicide <- as.data.table(as.data.frame(dat))
		dat_despair <- as.data.table(as.data.frame(dat))
	}

	if (!py_rows) {
		# Special attention to leaving work ####
		# Suicide ####
		dat_suicide <- rbindlist(list(
			# Years other than year of leaving work
			dat_suicide[year != year(jobloss.date) |
										year(jobloss.date) >= 1995,.(
											studyno,
											year,
											age.year1,
											age.year2,
											yob,
											Suicide,
											`All causes`,
											yod,
											`Job loss`,
											jobloss.date,
											jobloss.date.gm,
											plant,
											race,
											finrace,
											suicide_yin.cat
										)],
			# Year of leaving work; recorded yod
			dat_suicide[year == year(jobloss.date) &
										year(jobloss.date) < 1995,.(
											studyno,
											year,
											age.year1,
											age.year2 = floor(time_length(difftime(
												apply(data.frame(
													as.Date(paste0(year + 1, "-01-01")),
													jobloss.date + days(1),
													yod + days(1)),
													1, min, na.rm = T), yob), 'day')),
											yob,
											Suicide = ifelse(Suicide == 1 & yod <= jobloss.date, 1, 0),
											`All causes`,
											yod,
											`Job loss` = 0,
											jobloss.date,
											jobloss.date.gm,
											plant,
											race,
											finrace,
											suicide_yin.cat
										)],
			# Year of leaving jobloss end of year
			dat_suicide[year == year(jobloss.date) &
										year(jobloss.date) < 1995 &
										!(month(jobloss.date) == 12 & day(jobloss.date) == 31) & (
											jobloss.date < yod | is.na(yod)),.(
												studyno,
												year,
												age.year1 = floor(time_length(difftime(
													apply(data.frame(
														as.Date(paste0(year + 1, "-01-01")),
														jobloss.date + days(1),
														yod + days(1)),
														1, min, na.rm = T), yob), 'day')),
												age.year2,
												yob,
												Suicide = ifelse(Suicide == 1 & yod > jobloss.date, 1, 0),
												`All causes`,
												yod,
												`Job loss` = 1,
												jobloss.date,
												jobloss.date.gm,
												plant,
												race,
												finrace,
												suicide_yin.cat
											)]))

		dat_suicide[
			year(jobloss.date) == 1995 | yod <= jobloss.date,
			`Job loss` := 0]
		dat_suicide[year > year(jobloss.date) & year(jobloss.date) < 1995,
								`Job loss` := 1]
		setorder(dat_suicide, studyno, year, age.year1)

		# Despair ####
		dat_despair <- rbindlist(list(
			# Years other than year of leaving work
			dat_despair[year != year(jobloss.date) |
										year(jobloss.date) >= 1995,.(
											studyno,
											year,
											age.year1,
											age.year2,
											yob,
											Despair,
											yod,
											`Job loss`,
											jobloss.date,
											jobloss.date.gm,
											plant,
											race,
											finrace,
											despair_yin.cat
										)],
			# Year of leaving work; recorded yod
			dat_despair[year == year(jobloss.date) &
										year(jobloss.date) < 1995,.(
											studyno,
											year,
											age.year1,
											age.year2 = floor(time_length(difftime(
												apply(data.frame(
													as.Date(paste0(year + 1, "-01-01")),
													jobloss.date + days(1),
													yod + days(1)),
													1, min, na.rm = T), yob), 'day')),
											yob,
											Despair = ifelse(Despair == 1 & yod <= jobloss.date, 1, 0),
											yod,
											`Job loss` = 0,
											jobloss.date,
											jobloss.date.gm,
											plant,
											race,
											finrace,
											despair_yin.cat
										)],
			dat_despair[year == year(jobloss.date) &
										year(jobloss.date) < 1995 &
										!(month(jobloss.date) == 12 & day(jobloss.date) == 31) & (
											jobloss.date < yod | is.na(yod)),.(
												studyno,
												year,
												age.year1 = floor(time_length(difftime(
													apply(data.frame(
														as.Date(paste0(year + 1, "-01-01")),
														jobloss.date + days(1),
														yod + days(1)),
														1, min, na.rm = T), yob), 'day')),
												age.year2,
												yob,
												Despair = ifelse(Despair == 1 & yod > jobloss.date, 1, 0),
												yod,
												`Job loss` = 1,
												jobloss.date,
												jobloss.date.gm,
												plant,
												race,
												finrace,
												despair_yin.cat
											)]))

		dat_despair[
			year(jobloss.date) == 1995 | yod <= jobloss.date,
			`Job loss` := 0]
		dat_despair[year > year(jobloss.date) & year(jobloss.date) < 1995,
								`Job loss` := 1]

		setorder(dat_despair, studyno, year, age.year1)

	}

	if (run_model) {

		if (run_finegray) {
			if (get_despair) {
				fg.dat_despair <- as.data.table(as.data.frame(
					dat_despair))
				setorder(fg.dat_despair, studyno, year)
				fg.dat_despair[,`:=`(
					I = 1:.N,
					N = .N
				), by = .(studyno)]
				# fg.dat_despair <- fg.dat_despair[I == N]
			}

			if (get_suicide) {
				fg.dat_suicide <- as.data.table(as.data.frame(
					dat_suicide))
				setorder(fg.dat_suicide, studyno, year)
				fg.dat_suicide[,`:=`(
					I = 1:.N,
					N = .N
				), by = .(studyno)]
				# fg.dat_suicide <- fg.dat_suicide[I == N]
			}
		}


		if (get_despair) {

			if (run_categorical) {
				# Despair model ####
				sincehire_despair.coxph <<- coxph(
					as.formula(paste(
						"Surv(age.year1, age.year2,
						 Despair) ~
					`Job loss` +
					plant + ", ifelse(use_finrace, "finrace ", "race +"),
						"pspline(year, df = 0) +
					despair_yin.cat",
						ifelse(include_women, "+ sex", ""))),
					data = 	dat_despair
				)

				print(as.data.table(summary(sincehire_despair.coxph)$coefficients)[,.(
					name = rownames(summary(sincehire_despair.coxph)$coefficients),
					`HR of despair` = exp(coef),
					lower = exp(coef - `se(coef)` * 1.96),
					upper = exp(coef + `se(coef)` * 1.96)
				)]
				)

				saveRDS(sincehire_despair.coxph,
								file = paste0(
									directory, paste0(
										'/sincehire_despair',
										ifelse(in_ips,
													 ifelse(!recent, "_in_ips", "_recent"),
													 ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(is.finite(baseline_sinceleavework.max),
													 paste0("_baseline", baseline_sinceleavework.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))}

		}

		if (get_suicide) {

			if (run_categorical) {
				# Suicide model ####
				sincehire_suicide.coxph <<- coxph(
					as.formula(paste(
						"Surv(age.year1, age.year2,
						 Suicide) ~
					`Job loss` +
					plant +", ifelse(use_finrace, "finrace ", "race +"),
						"pspline(year, df = 0) +
					suicide_yin.cat",
						ifelse(include_women, "+ sex", ""))),
					data = dat_suicide
				)

				print(as.data.table(summary(sincehire_suicide.coxph)$coefficients)[,.(
					name = rownames(summary(sincehire_suicide.coxph)$coefficients),
					`HR of suicide` = exp(coef),
					lower = exp(coef - `se(coef)` * 1.96),
					upper = exp(coef + `se(coef)` * 1.96)
				)]
				)

				saveRDS(sincehire_suicide.coxph,
								file = paste0(
									directory, paste0(
										'/sincehire_suicide',
										ifelse(
											in_ips,
											ifelse(!recent, "_in_ips", "_recent"),
											ifelse(!recent, "_in_full", "_recent")),
										ifelse(is.finite(years.max),
													 paste0("_", years.max),
													 ""),
										ifelse(is.finite(baseline_sinceleavework.max),
													 paste0("_baseline", baseline_sinceleavework.max),
													 ""),
										ifelse(include_women,
													 "_women",
													 ""),
										'.coxph.rds')
								))}

		}

	}
}

# Visualizing splines ####
get.spline <- function(
	in_ips = F,
	recent = T,
	ref_age = ref.age,
	years.max = NA,
	get_despair = T,
	get_suicide = T,
	include_women = F,
	width = 3.5,
	height = 2.75,
	model.dir = to_drive_D(here::here("despair/resources")),
	output.dir = here::here("reports/resources/splines")) {

	dir.create(output.dir, F, T)

	require(graphics); require(MASS); require(survival)

	rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))

	# For prediction
	dat <- as.data.table(as.data.frame(get(
		paste0('cohort_leftwork', ifelse(
			in_ips,
			ifelse(!recent, "_in_ips", "_recent"),
			ifelse(!recent, "_in_full", "_recent")))
	)))

	if (is.finite(years.max)) {
		dat <- dat[
			since.leavework2 <= years.max * 365.25]
	}

	if (!include_women) {
		dat <- dat[Sex == "M"]
	}

	# For rug plot
	rugplot.dat <- dat[
		jobloss.age <= ref_age,.(
			jobloss.age = jobloss.age[1],
			Despair = sum(Despair),
			Suicide = sum(Suicide)
		), by = .(studyno)]

	lapply(c('Despair', 'Suicide')[c(get_despair,
																	 get_suicide)],
				 # outcome <- "Suicide"
				 function(outcome = "Despair") {

				 	# For spline
				 	tmp.name <- paste0('splined_leftwork_', tolower(outcome))
				 	tmp.coxph <- readRDS(
				 		paste0(
				 			model.dir, paste0(
				 				tmp.name, "/",
				 				ifelse(in_ips,
				 							 ifelse(!recent, "_in_ips", "_recent"),
				 							 ifelse(!recent, "_in_full", "_recent")),
				 				ifelse(is.finite(years.max),
				 							 paste0("_", years.max),
				 							 ""),
				 				ifelse(include_women,
				 							 "_women",
				 							 ""),
				 				'.coxph.rds'))
				 	)

				 	# Using predict() is not feasible for data of this size
				 	tmp.termplot <- termplot(
				 		model = tmp.coxph,
				 		data = dat, # this argument is pretty much worthless
				 		terms = 1,
				 		se = T,
				 		plot = F
				 	)

				 	tmp.ggtab <- data.frame(
				 		jobloss.age = tmp.termplot$jobloss.age.restricted$x,
				 		fit = tmp.termplot$jobloss.age.restricted$y,
				 		se = tmp.termplot$jobloss.age.restricted$se,
				 		ref = with(tmp.termplot$jobloss.age.restricted,
				 							 y[x == ref_age]),
				 		check.names = F
				 	)
				 	setDT(tmp.ggtab)

				 	assign(
				 		paste0(tolower(outcome), "_by_age_jobloss.ggplot"),
				 		ggplot(tmp.ggtab[jobloss.age <= ref_age], aes(
				 			x = jobloss.age,
				 			y = exp(fit - ref)
				 		)) +
				 			stat_function(fun = function(x) {1}, geom = 'line',
				 										color = 'grey') +
				 			geom_ribbon(aes(
				 				ymin = exp(fit - ref - qnorm(0.975) * se),
				 				ymax = exp(fit - ref + qnorm(0.975) * se)),
				 				alpha = 0.1) +
				 			geom_line() +
				 			geom_rug(data = rugplot.dat[
				 				{if (outcome == 'Suicide') {
				 					Suicide == 1} else {
				 						Despair == 1
				 					}}],
				 				aes(x = jobloss.age,
				 						y = 1),
				 				sides = 'b') +
				 			# Plot window ####
				 		coord_cartesian(ylim = c(0, ifelse(in_ips | recent, 4, 4))) +
				 			labs(
				 				x = "Age at worker exit",
				 				y = paste0(
				 					"HR of ", ifelse(outcome == 'Suicide', outcome,
				 													 "suicide or overdose"))) +
				 			mytheme + theme(
				 				strip.text = element_text(hjust = 0)
				 			),
				 		envir = .GlobalEnv)

				 	if (get_suicide & get_despair) {
				 		tmp.ggtab[,Outcome := paste0(outcome)]
				 		assign(paste0(tolower(outcome), '.ggtab'),
				 					 tmp.ggtab, envir = .GlobalEnv)
				 	}

				 })

	if (get_despair & get_suicide) {

		tmp.ggtab <- rbindlist(list(
			suicide.ggtab,
			despair.ggtab
		))

		tmp.ggtab[, Outcome := factor(
			Outcome,
			levels = c("Suicide", "Despair"),
			labels = c("A) Suicide", "B) Suicide and fatal overdose"))]

		suicide.rugplot <- as.data.table(as.data.frame(rugplot.dat[Suicide == 1]))
		suicide.rugplot[,`:=`(Outcome = "A) Suicide")]

		despair.rugplot <- as.data.table(as.data.frame(rugplot.dat[Despair == 1]))
		despair.rugplot[,`:=`(Outcome = "B) Suicide and fatal overdose")]

		rugplot.dat <- rbindlist(list(
			suicide.rugplot,
			despair.rugplot
		))

		rugplot.dat[, Outcome := factor(
			Outcome,
			levels = c("A) Suicide", "B) Suicide and fatal overdose"),
			labels = c("A) Suicide", "B) Suicide and fatal overdose"))]


		rugplot.dat[, `:=`(
			Indicator = 1)]

		# Write tex
		tikz(file = paste0(
			output.dir,
			paste0("/despair_suicide_by_age_jobloss",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width * 1.85, height = height *1.1)
		print(
			ggplot(tmp.ggtab[jobloss.age <= ref_age], aes(
				x = jobloss.age,
				y = exp(fit - ref)
			)) +
				stat_function(fun = function(x) {1}, geom = 'line',
											color = 'grey') +
				geom_ribbon(aes(
					ymin = exp(fit - ref - qnorm(0.975) * se),
					ymax = exp(fit - ref + qnorm(0.975) * se)),
					alpha = 0.1) +
				geom_line() +
				geom_rug(data = rugplot.dat,
								 aes(x = jobloss.age,
								 		y = 1),
								 sides = 'b') +
				# Plot window ####
			coord_cartesian(ylim = c(0, ifelse(in_ips | recent, 4, 4))) +
				labs(
					x = "Age at worker exit",
					y = "Hazard ratio") +
				facet_wrap(. ~ Outcome, ncol = 2) +
				mytheme + theme(
					strip.text = element_text(hjust = 0)
				)
		)
		dev.off()
		tikz(file = paste0(
			output.dir,
			paste0("/despair_by_age_jobloss",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width, height = height)
		print(despair_by_age_jobloss.ggplot)
		dev.off()
		tikz(file = paste0(
			output.dir,
			paste0("suicide_by_age_jobloss",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width, height = height)
		print(suicide_by_age_jobloss.ggplot)
		dev.off()
	} else {
		# Show plot
		if (get_despair) {
			print(despair_by_age_jobloss.ggplot)
		} else {
			print(suicide_by_age_jobloss.ggplot)
		}

		# Write tex
		tikz(file = paste0(
			output.dir, "/",
			paste0(ifelse(get_despair, 'despair', 'suicide'),
						 "_by_age_jobloss",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width, height = height)
		print(get(paste0(ifelse(get_despair, 'despair', 'suicide'),
										 "_by_age_jobloss.ggplot"),
							envir = .GlobalEnv))
		dev.off()
	}

	rm(list = ls(envir = .GlobalEnv)[
		grepl("ggtab|rugplot|ggplot", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

	rm(list = ls()[grepl("ggtab|rugplot|ggplot", ls())])
}

get.spline_df <- function(
	in_ips = F,
	ref_age = ref.age,
	years.max = NA,
	get_despair = T,
	get_suicide = T,
	recent = T,
	include_women = F,
	width = 3.25,
	height = 2.5,
	directory = to_drive_D(here::here("despair/resources"))) {
	require(graphics); require(MASS); require(survival)

	lapply(c('Despair', 'Suicide')[c(get_despair,
																	 get_suicide)],
				 # outcome <- "Suicide"
				 function(outcome) {
				 	tmp.name <- paste0('splined_leftwork_', tolower(outcome))
				 	tmp.coxph <- readRDS(
				 		paste0(
				 			directory, "/", paste0(
				 				tmp.name,
				 				ifelse(in_ips,
				 							 ifelse(!recent, "_in_ips", "_recent"),
				 							 ifelse(!recent, "_in_full", "_recent")),
				 				ifelse(is.finite(years.max),
				 							 paste0("_", years.max),
				 							 ""),
				 				ifelse(include_women,
				 							 "_women",
				 							 ""),
				 				'.coxph.rds'))
				 	)

				 	return(data.frame(
				 		'FU restriction' = ifelse(
				 			is.na(years.max), "None", paste(years.max)),
				 		Outcome= outcome,
				 		df = tmp.coxph$df[1]))

				 })
}

# get.spline_df(recent = T)
# get.spline_df(in_ips = F)
#
# get.spline_df(recent = T, years.max = 10)
# get.spline_df(in_ips = F, years.max = 10)
#
# get.spline_df(recent = T, years.max = 5)
# get.spline_df(in_ips = F, years.max = 5)

get.simplespline <- function(
	in_ips = F,
	years.max = NA,
	get_despair = T,
	get_suicide = T,
	recent = T,
	include_women = F,
	width = 3.25,
	height = 2.5,
	baseline_sinceleavework.max = NA,
	model.dir = to_drive_D(here::here("despair/resources")),
	output.dir = here::here("reports/resources/splines")) {
	require(graphics); require(MASS); require(survival)

	dir.create(output.dir, F, T)

	rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))

	# For prediction
	dat <- as.data.table(as.data.frame(get(paste0('cohort_sincehire',
																								ifelse(in_ips, ifelse(!recent, "_in_ips", "_recent"),
																											 ifelse(!recent, "_in_full", "_recent"))))))

	# Maximum follow-up
	if (is.finite(years.max)) {
		dat <- dat[time_length(difftime(as.Date(paste0(year, "-01-01")),
																		jobloss.date), 'years') <= years.max]
	}

	if (!include_women) {
		dat <- dat[sex == "M"]
	}

	# Move up job loss date if close to time of death
	suicide.who <- unique(dat[Suicide == 1, studyno])
	overdose.who <- unique(dat[Overdose == 1, studyno])

	if (is.finite(baseline_sinceleavework.max)) {

		dat_suicide <- as.data.table(as.data.frame(dat))
		dat_suicide[
			studyno %in% suicide.who &
				time_length(difftime(yod, jobloss.date), 'days') < baseline_sinceleavework.max &
				yod > jobloss.date, `:=`(
					jobloss.date = jobloss.date + days(baseline_sinceleavework.max),
					changed_jobloss.date = 1)]
		dat_suicide[, `:=`(
			`Job loss` = ifelse(
				year == year(jobloss.date),
				time_length(difftime(
					jobloss.date, as.Date(paste0(year, '-01-01')), 'years')),
				ifelse(year > year(jobloss.date), 1, 0))
		)]
		dat_suicide[yod <= jobloss.date, `Job loss` := 0]

		dat_despair <- as.data.table(as.data.frame(dat))
		dat_despair[
			studyno %in% c(suicide.who, overdose.who) &
				time_length(difftime(yod, jobloss.date), 'days') < baseline_sinceleavework.max &
				yod > jobloss.date, `:=`(
					jobloss.date = jobloss.date + days(baseline_sinceleavework.max),
					changed_jobloss.date = 1)]
		dat_despair[, `:=`(
			`Job loss` = ifelse(
				year == year(jobloss.date),
				time_length(difftime(
					jobloss.date, as.Date(paste0(year, '-01-01')), 'years')),
				ifelse(year > year(jobloss.date), 1, 0))
		)]
		dat_despair[yod <= jobloss.date, `Job loss` := 0]

	} else {
		dat_suicide <- as.data.table(as.data.frame(dat))
		dat_despair <- as.data.table(as.data.frame(dat))
	}

	lapply(c('Despair', 'Suicide')[c(get_despair,
																	 get_suicide)],
				 # outcome <- "Suicide"
				 function(outcome = "Suicide") {
				 	# For rug plot
				 	dat <- as.data.table(as.data.frame(
				 		get(paste0('dat_', tolower(outcome)))))
				 	rugplot.dat <- dat[
				 		year == year(yod), .(
				 			year = year(yod) + time_length(difftime(
				 				yod, as.Date(paste0(year(yod), "-01-01"))),
				 				'days'
				 			) / 365.25,
				 			Despair = sum(Despair),
				 			Suicide = sum(Suicide)
				 		), by = .(studyno)]

				 	# For spline
				 	tmp.coxph <- readRDS(
				 		paste0(
				 			model.dir, "/", paste0(
				 				'sincehire_', tolower(outcome),
				 				ifelse(in_ips,
				 							 ifelse(!recent, "_in_ips", "_recent"),
				 							 ifelse(!recent, "_in_full", "_recent")),
				 				ifelse(is.finite(years.max),
				 							 paste0("_", years.max),
				 							 ""),
				 				ifelse(is.finite(baseline_sinceleavework.max),
				 							 paste0("_baseline", baseline_sinceleavework.max),
				 							 ""),
				 				ifelse(include_women,
				 							 "_women",
				 							 ""),
				 				'.coxph.rds')
				 		))

				 	# Using predict() is not feasible for data of this size
				 	tmp.termplot <- termplot(
				 		model = tmp.coxph,
				 		data = dat, # this argument is pretty much worthless
				 		terms = which(grepl("spline\\(year", attr(tmp.coxph$terms, 'term.labels'))),
				 		se = T,
				 		plot = F
				 	)

				 	tmp.ggtab <- data.frame(
				 		year = tmp.termplot$year$x,
				 		fit = tmp.termplot$year$y,
				 		se = tmp.termplot$year$se,
				 		check.names = F
				 	)
				 	setDT(tmp.ggtab)

				 	assign(
				 		paste0(tolower(outcome), "_by_year.ggplot"),
				 		ggplot(tmp.ggtab, aes(
				 			x = year,
				 			y = exp(fit)
				 		)) +
				 			# stat_function(fun = function(x) {1}, geom = 'line',
				 			# 							color = 'grey') +
				 			geom_ribbon(aes(
				 				ymin = exp(fit - qnorm(0.975) * se),
				 				ymax = exp(fit + qnorm(0.975) * se)),
				 				alpha = 0.1) +
				 			geom_line() +
				 			geom_rug(data = rugplot.dat[
				 				{if (outcome == 'Suicide') {
				 					Suicide == 1} else {
				 						Despair == 1
				 					}}],
				 				aes(x = year,
				 						y = 1),
				 				sides = 'b') +
				 			# Plot window ####
				 		coord_cartesian(ylim = c(0, ifelse(in_ips | recent, 3, 3))) +
				 			labs(
				 				x = "Year",
				 				y = paste0(
				 					"Linear predictor ($\\exp(\\hat\\beta)$) of ", ifelse(outcome == 'Suicide', outcome,
				 																																"suicide or overdose"))) +
				 			mytheme,
				 		envir = .GlobalEnv, inherits = T)

				 	tmp.ggtab[, Outcome := paste0(outcome)]
				 	rugplot.dat[, Outcome := paste0(outcome)]

				 	if (get_suicide & get_despair) {
				 		assign(paste0(tolower(outcome), '.ggtab'),
				 					 tmp.ggtab, envir = .GlobalEnv)
				 		assign(paste0(tolower(outcome), ".rugplot"),
				 					 rugplot.dat[
				 					 	{if (outcome == 'Suicide') {
				 					 		Suicide == 1} else {
				 					 			Despair == 1
				 					 		}}], envir = .GlobalEnv)
				 	}

				 })

	if (get_despair & get_suicide) {
		tmp.ggtab <- rbindlist(list(
			suicide.ggtab,
			despair.ggtab
		))

		tmp.ggtab[, Outcome := factor(
			Outcome,
			levels = c("Suicide", "Despair"),
			labels = c("A) Suicide", "B) Suicide and fatal overdose"))]

		rugplot.dat <- rbindlist(list(
			suicide.rugplot[Suicide == 1],
			despair.rugplot[Despair == 1]
		))

		rugplot.dat[, `:=`(
			Outcome = factor(
				Outcome,
				levels = c("Suicide", "Despair"),
				labels = c("A) Suicide", "B) Suicide and fatal overdose")),
			Indicator = 1)]

		# Write tex
		tikz(file = paste0(
			output.dir, "/",
			paste0("despair_suicide_by_year",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(is.finite(baseline_sinceleavework.max),
						 			 paste0("_baseline", baseline_sinceleavework.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width * 1.85, height = height)
		ggplot(tmp.ggtab, aes(
			x = year,
			y = exp(fit)
		)) +
			# stat_function(fun = function(x) {1}, geom = 'line',
			# 							color = 'grey') +
			geom_ribbon(aes(
				ymin = exp(fit - qnorm(0.975) * se),
				ymax = exp(fit + qnorm(0.975) * se)),
				alpha = 0.1) +
			geom_line() +
			geom_rug(data = rugplot.dat,
							 aes(x = year,
							 		y = 1),
							 sides = 'b') +
			# Plot window ####
		coord_cartesian(ylim = c(0, ifelse(in_ips | recent, 3, 3))) +
			labs(
				x = "Year",
				y = "Linear predictor ($\\exp(\\hat\\beta)$)") +
			facet_wrap(. ~ Outcome, ncol = 2) +
			mytheme + theme(
				strip.text = element_text(hjust = 0)
			)
		dev.off()
		tikz(file = paste0(
			output.dir, "/",
			paste0("despair_by_year",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(is.finite(baseline_sinceleavework.max),
						 			 paste0("_baseline", baseline_sinceleavework.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width, height = height)
		print(despair_by_year.ggplot)
		dev.off()
		tikz(file = paste0(
			output.dir, "/",
			paste0("suicide_by_year",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(is.finite(baseline_sinceleavework.max),
						 			 paste0("_baseline", baseline_sinceleavework.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width, height = height)
		print(suicide_by_year.ggplot)
		dev.off()
	} else {
		# Show plot
		if (get_despair) {
			print(despair_by_year.ggplot)
		} else {
			print(suicide_by_year.ggplot)
		}

		# Write tex
		tikz(file = here::here(
			output.dir, "/",
			paste0(ifelse(get_despair, 'despair', 'suicide'),
						 "_by_year",
						 ifelse(in_ips,
						 			 ifelse(!recent, "_in_ips", "_recent"),
						 			 ifelse(!recent, "_in_full", "_recent")),
						 ifelse(is.finite(years.max),
						 			 paste0("_", years.max),
						 			 ""),
						 ifelse(is.finite(baseline_sinceleavework.max),
						 			 paste0("_baseline", baseline_sinceleavework.max),
						 			 ""),
						 ifelse(include_women,
						 			 "_women",
						 			 ""),
						 ".tex")
		), standAlone = T, width = width, height = height)
		print(get(paste0(ifelse(get_despair, 'despair', 'suicide'),
										 "_by_year.ggplot"),
							envir = .GlobalEnv))
		dev.off()
	}

	rm(list = ls(envir = .GlobalEnv)[
		grepl("ggtab|rugplot|ggplot", ls(envir = .GlobalEnv))], envir = .GlobalEnv)

	rm(list = ls()[grepl("ggtab|rugplot|ggplot", ls())])
}


# HR table for categorical models ####
get.coef <- function(
	recent = T,
	in_ips = F,
	years.max = NA,
	get_despair = T,
	get_suicide = T,
	ref_age = ref.age,
	include_women = F,
	use_finrace = F,
	mathmode = T,
	get_finegray = F,
	model.dir = to_drive_D(here::here("despair/resources")),
	output.dir = here::here("reports/resources/estimates")) {

	dir.create(output.dir, F, T)

	rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))

	# For case counts
	dat <- as.data.table(as.data.frame(get(
		paste0('cohort_leftwork',
					 ifelse(in_ips, ifelse(!recent, "_in_ips", "_recent"), ifelse(!recent, "_in_full", "_recent"))
		), envir = .GlobalEnv)))

	if (is.finite(years.max)) {
		dat <- dat[since.leavework2 <= years.max * 365.25]
	}

	if (!include_women) {
		dat <- dat[Sex == "M"]
	}

	dat <- dat[,.(
		jobloss.age = jobloss.age[1],
		Despair = max(Despair),
		Suicide = max(Suicide),
		race = race[1],
		finrace = finrace[1],
		plant = plant[1],
		Sex = Sex[1],
		hire.age = hire.age[1],
		yin.num = yin.num[1],
		yin = yin[1]
	), by = .(studyno)]

	lapply(c('Despair', 'Suicide')[c(
		ifelse(get_despair, T, F),
		ifelse(get_suicide, T, F)
	)], function(outcome = "Suicide") {

		# # Age at hire categories
		# hire.age.cutpoints <- quantile(
		# 	dat[get(outcome) == 1, hire.age],
		# 	seq(0, 1, 0.25)
		# )
		#
		# hire.age.cutpoints <- unique(c(
		# 	floor(min(dat$hire.age)),
		# 	round(hire.age.cutpoints)[-c(1, length(hire.age.cutpoints))],
		# 	ceiling(max(dat$hire.age))
		# ))
		#
		# dat[,`:=`(
		# 	hire.age.cat = cut(hire.age, hire.age.cutpoints,
		# 										 right = F, include.lowest = T)
		# )]
		#
		# # Year of hire hire categories
		# yin.cutpoints <- quantile(dat[get(outcome), date.to.gm(yin)],
		# 													seq(0, 1, 0.25))
		#
		# yin.cutpoints <- unique(c(
		# 	floor(date.to.gm(min(dat$yin))),
		# 	round(yin.cutpoints)[-c(1, length(yin.cutpoints))],
		# 	ceiling(date.to.gm(max(dat$yin)))))
		#
		# dat[,`:=`(
		# 	yin.cat = cut(date.to.gm(yin),
		# 								yin.cutpoints,
		# 								include.lowest = T,	right = F)
		# )]


		# For model results
		tmp.name <- paste0('leftwork_', tolower(outcome))
		tmp.coxph <- readRDS(paste0(
			model.dir, paste0(
				"/", tmp.name,
				ifelse(
					in_ips,
					ifelse(!recent, "_in_ips", "_recent"),
					ifelse(!recent, "_in_full", "_recent")),
				ifelse(is.finite(years.max), paste0("_", years.max), ""),
				ifelse(include_women, "_women", ""),
				ifelse(get_finegray, ".fg", ".coxph"), '.rds')))

		# Drop splined term (yin)
		tmp.tab <- 	summary(tmp.coxph)$coefficients
		tmp.tab <- as.data.frame(cbind(
			rownames(tmp.tab), as.data.frame(tmp.tab)))

		if (sum(grepl('spline\\(yin', tmp.coxph$formula))) {
			splined.covariate <- "Year of hire"
			tmp.tab <- tmp.tab[-grep('yin', rownames(tmp.tab)), c(1:3)]
		}

		if (sum(grepl('spline\\(jobloss.date', tmp.coxph$formula))) {
			splined.covariate <- "Year of worker exit"
			tmp.tab <- tmp.tab[-grep('jobloss.date', rownames(tmp.tab)), c(1:3)]
		}

		tmp.tab$`rownames(tmp.tab)` <- with(
			tmp.tab,
			levels(`rownames(tmp.tab)`)[as.numeric(`rownames(tmp.tab)`)])
		setDT(tmp.tab)

		agecat.which <- with(tmp.tab,
												 which(grepl("Age at ", `rownames(tmp.tab)`))
		)

		hireage.which <- with(tmp.tab,
													which(grepl("hire.age", `rownames(tmp.tab)`))
		)

		yincat.which <- with(tmp.tab,
												 which(grepl("yin.cat", `rownames(tmp.tab)`))
		)

		tmp.tab <- tmp.tab[, .(
			Covariate = NA,
			level = c(paste0(
				ifelse(mathmode, "$", ""),
				substr(`rownames(tmp.tab)`[agecat.which], 19,
							 unlist(regexpr(",", `rownames(tmp.tab)`[agecat.which])) - 1),
				ifelse(mathmode, "$ to $", " to "),
				{upper <- as.numeric(gsub("\\]|)", "", substring(
					`rownames(tmp.tab)`[agecat.which],
					unlist(regexpr(",", `rownames(tmp.tab)`[agecat.which])) + 1)))
				upper <- upper - 1
				upper
				},
				ifelse(mathmode, "$", "")
			),
			if (sum(grepl('hire.age', tmp.tab$`rownames(tmp.tab)`)) > 0) {
				c(
					paste0(
						ifelse(mathmode, "$", ""),
						substr(`rownames(tmp.tab)`[hireage.which], 14,
									 unlist(regexpr(",", `rownames(tmp.tab)`[hireage.which])) - 1),
						ifelse(mathmode, "$ to $", " to "),
						{upper <- as.numeric(gsub("\\]|)", "", substring(
							`rownames(tmp.tab)`[hireage.which],
							unlist(regexpr(",", `rownames(tmp.tab)`[hireage.which])) + 1)))
						upper[-length(upper)] <- upper[-length(upper)] - 1
						upper
						},
						ifelse(mathmode, "$", "")
					)
				)
			},
			'Plant 2',
			'Plant 3',
			'Black',
			if (use_finrace) {'Unknown'},
			if (include_women) {'Female'}),
			HR = paste0(
				ifelse(mathmode, "$", ""),
				formatC(round(exp(coef), digits = 1),
								format = 'f', digits = 1),
				ifelse(mathmode, "$", "")),
			SE = paste0(
				ifelse(mathmode, "$", ""),
				formatC(round(exp(`se(coef)`), digits = 1),
								format = 'f', digits = 1),
				ifelse(mathmode, "$", "")),
			`95\\% CI` = paste0(
				ifelse(mathmode, "$(", "("),
				formatC(round(exp(coef - qnorm(0.975) * `se(coef)`), digits = 1),
								format = 'f', digits = 1),
				",\\,",
				formatC(round(exp(coef + qnorm(0.975) * `se(coef)`), digits = 1),
								format = 'f', digits = 1),
				ifelse(mathmode, ")$", ")")
			),
			lower = {
				lower <- rep(NA, nrow(tmp.tab))
				lower[agecat.which] <- substr(
					`rownames(tmp.tab)`[agecat.which], 19,
					unlist(regexpr(",", `rownames(tmp.tab)`[agecat.which])) - 1)
				if (length(hireage.which) > 0) {
					lower[hireage.which] <- substr(
						`rownames(tmp.tab)`[hireage.which], 14,
						unlist(regexpr(",", `rownames(tmp.tab)`[hireage.which])) - 1)}
				as.numeric(lower)
			},
			upper = {
				upper <- rep(NA, nrow(tmp.tab))
				upper[agecat.which] <- (gsub(
					'\\]|)', '', substring(
						`rownames(tmp.tab)`[agecat.which],
						unlist(regexpr(",", `rownames(tmp.tab)`[agecat.which])) + 1)))
				if (length(hireage.which) > 0) {
					upper[hireage.which] <- (gsub(
						'\\]|)', '', substring(
							`rownames(tmp.tab)`[hireage.which],
							unlist(regexpr(",", `rownames(tmp.tab)`[hireage.which])) + 1)))
					upper <- as.numeric(upper)
					upper[-max(hireage.which)] <- upper[-max(hireage.which)] - 1}
				as.numeric(upper)
			}
		)]

		tmp.tab[,`:=`(Covariate = as.character(Covariate))]
		# tmp.tab[hireage.which[1], Covariate := "Age at hire"]

		tmp.tab <- rbindlist(list(
			data.table(
				Covariate = c('Age at job loss'),
				level = c(paste0(ifelse(mathmode, "$", ""),
												 ref_age,
												 ifelse(mathmode, "$", ""),
												 ' or older'))
			),
			tmp.tab[agecat.which,],
			# if (sum(grepl('hire.age', names(tmp.coxph$coefficients))) > 0) {
			# 	tmp.tab[hireage.which[1]]
			# },
			if (sum(grepl('hire.age', names(tmp.coxph$coefficients))) > 0) {
				data.table(
					Covariate = "Age at hire",
					level = c(paste0(
						ifelse(mathmode, "$", ""),
						substring(levels(dat$hire.age.cat)[1], 2,
											regexpr(",", levels(dat$hire.age.cat)[1]) - 1),
						ifelse(mathmode, "$", ""),
						' to ',
						ifelse(mathmode, "$", ""),
						as.numeric(
							gsub("\\]|)", "",
									 substring(levels(dat$hire.age.cat)[1],
									 					regexpr(",", levels(dat$hire.age.cat)[1]) + 1))) - 1,
						ifelse(mathmode, "$", ""))),
					lower = substring(
						levels(dat$hire.age.cat)[1], 2,
						regexpr(",", levels(dat$hire.age.cat)[1]) - 1),
					upper = as.numeric(
						gsub("\\]|)", "",
								 substring(levels(dat$hire.age.cat)[1],
								 					regexpr(",", levels(dat$hire.age.cat)[1]) + 1))) - 1
				)
			},
			if (sum(grepl('hire.age', names(tmp.coxph$coefficients))) > 0) {
				tmp.tab[hireage.which,]
			},
			data.table(
				Covariate = c('Plant'),
				level = c("Plant 1")
			),
			tmp.tab[level %in% c("Plant 2", "Plant 3"),],
			data.table(
				Covariate = c('Race'),
				level = c('White')
			),
			tmp.tab[level %in% c("Black", if (use_finrace) {"Unknown"}),],
			if (include_women) {data.table(
				Covariate = c('Sex'),
				level = c('Male')
			)},
			if (include_women) {tmp.tab[level == "Female",]}
		),
		fill = T)

		# jobloss age event counts
		n <- table(dat[get(outcome) == 1,.(
			cut(jobloss.age, c(
				unique(sort(c(
					tmp.tab[grep("Age at job loss", zoo::na.locf(Covariate))]$lower,
					tmp.tab[grep("Age at job loss", zoo::na.locf(Covariate))]$upper
				))),
				max(jobloss.age)),
				right = F, include.lowest = T)
		)]$V1)
		n <- n[length(n):1]
		# Age at hire
		if (sum(grepl("hire", tmp.tab$Covariate)) > 0) {
			n <- c(n, table(dat[get(outcome) == 1, hire.age.cat]))
		}
		# Plant and race (and sex)
		n <- c(n, unlist(sapply(
			dat[get(outcome) == 1,.(
				plant,
				finrace = if (use_finrace) {finrace},
				race = if (!use_finrace) {race},
				Sex = if (include_women) {Sex}
			)], table)))

		tmp.tab$n <- paste0(ifelse(mathmode, "$", ""), n, ifelse(mathmode, "$", ""))

		tmp.tab <- tmp.tab[,.(
			Covariate,
			level,
			`$n$` = n,
			HR,
			`95\\% CI`,
			SE
		)]

		tmp.tab <- rbindlist(list(
			tmp.tab,
			data.table(
				Covariate = splined.covariate,
				level = paste0("splined ",
											 ifelse(mathmode, "$", ""),
											 "(df = ",
											 round(
											 	tmp.coxph$df[
											 		tmp.coxph$pterms == 1],
											 	2),
											 ")",
											 ifelse(mathmode, "$", "")),
				`$n$` = paste0(ifelse(mathmode, "$", ""),
											 summary(tmp.coxph)$nevent,
											 ifelse(mathmode, "$", "")),
				HR = NA,
				`95\\% CI` = NA,
				SE = NA
			)
		))

		names(tmp.tab)[names(tmp.tab) == 'level'] <- "  "

		if (get_finegray) {
			names(tmp.tab)[grep("HR", names(tmp.tab))] <- "sdHR"
		}

		assign(paste0(tolower(outcome), ifelse(
			in_ips,
			ifelse(!recent, "_in_ips", "_recent"),
			ifelse(!recent, "_in_full", "_recent")),
			ifelse(get_finegray, ".fg.", ""), ".tab"),
			tmp.tab, envir = .GlobalEnv, inherits = T)

		saveRDS(tmp.tab,
						file = paste0(
							output.dir, "/",
							paste0(
								tolower(outcome), ifelse(
									in_ips,
									ifelse(!recent, "_in_ips", "_recent"),
									ifelse(!recent, "_in_full", "_recent")),
								ifelse(is.finite(years.max),
											 paste0("_", years.max),
											 ""),
								ifelse(include_women,
											 "_women",
											 ""),
								ifelse(get_finegray, ".fg", ""),
								".tab.rds")))

		print(outcome)
		print(tmp.tab)

	})

}

get.simplecoef <- function(
	in_ips = F,
	years.max = NA,
	get_despair = T,
	get_suicide = T,
	ref_age = ref.age,
	recent = T,
	include_women = F,
	baseline_sinceleavework.max = NA,
	use_finrace = F,
	mathmode = T,
	get_finegray = F,
	model.dir = to_drive_D(here::here("despair/resources")),
	output.dir = here::here("reports/resources/estimates")
) {

	dir.create(output.dir, F, T)

	rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))

	# For case counts
	dat <- as.data.table(as.data.frame(get(
		paste0('cohort_sincehire',
					 ifelse(in_ips, ifelse(!recent, "_in_ips", "_recent"), ifelse(!recent, "_in_full", "_recent"))
		), envir = .GlobalEnv)))

	if (is.finite(years.max)) {
		dat <- dat[time_length(difftime(as.Date(paste0(year, "-01-01")),
																		jobloss.date), 'years') <= years.max]
	}

	if (!include_women) {
		dat <- dat[sex == "M"]
	}

	# Move up job loss date if close to time of death
	suicide.who <- unique(dat[Suicide == 1, studyno])
	overdose.who <- unique(dat[Overdose == 1, studyno])

	if (is.finite(baseline_sinceleavework.max)) {

		dat_suicide <- as.data.table(as.data.frame(dat))
		dat_suicide[
			studyno %in% suicide.who &
				time_length(difftime(yod, jobloss.date), 'days') < baseline_sinceleavework.max &
				yod > jobloss.date, `:=`(
					jobloss.date = jobloss.date + days(baseline_sinceleavework.max),
					changed_jobloss.date = 1)]
		dat_suicide[, `:=`(
			`Job loss` = ifelse(
				year == year(jobloss.date),
				time_length(difftime(
					jobloss.date, as.Date(paste0(year, '-01-01')), 'years')),
				ifelse(year > year(jobloss.date), 1, 0))
		)]
		dat_suicide[yod <= jobloss.date, `Job loss` := 0]

		dat_despair <- as.data.table(as.data.frame(dat))
		dat_despair[
			studyno %in% c(suicide.who, overdose.who) &
				time_length(difftime(yod, jobloss.date), 'days') < baseline_sinceleavework.max &
				yod > jobloss.date, `:=`(
					jobloss.date = jobloss.date + days(baseline_sinceleavework.max),
					changed_jobloss.date = 1)]
		dat_despair[, `:=`(
			`Job loss` = ifelse(
				year == year(jobloss.date),
				time_length(difftime(
					jobloss.date, as.Date(paste0(year, '-01-01')), 'years')),
				ifelse(year > year(jobloss.date), 1, 0))
		)]
		dat_despair[yod <= jobloss.date, `Job loss` := 0]

	} else {
		dat_suicide <- as.data.table(as.data.frame(dat))
		dat_despair <- as.data.table(as.data.frame(dat))
	}

	lapply(c('Despair', 'Suicide')[c(
		ifelse(get_despair, T, F),
		ifelse(get_suicide, T, F)
	)], function(outcome = "Suicide") {

		dat <- as.data.table(as.data.frame(get(paste0('dat_', tolower(outcome)))))

		# For model results
		tmp.name <- paste0('sincehire_', tolower(outcome))
		tmp.coxph <- readRDS(paste0(
			model.dir, "/", paste0(
				tmp.name,
				ifelse(
					in_ips,
					ifelse(!recent, "_in_ips", "_recent"),
					ifelse(!recent, "_in_full", "_recent")),
				ifelse(is.finite(years.max), paste0("_", years.max), ""),
				ifelse(is.finite(baseline_sinceleavework.max),
							 paste0("_baseline", baseline_sinceleavework.max),
							 ""),
				ifelse(include_women, "_women", ""),
				ifelse(get_finegray, ".fg", ".coxph"), '.rds')))

		# Drop splined term
		tmp.tab <- 	summary(tmp.coxph)$coefficients
		tmp.tab <- as.data.frame(cbind(
			rownames(tmp.tab), as.data.frame(tmp.tab)))

		if (sum(grepl('spline\\(year', tmp.coxph$formula))) {
			splined.covariate <- "Calendar year"
			tmp.tab <- tmp.tab[-grep('\\(year', rownames(tmp.tab)), c(1:3)]
		}

		tmp.tab$`rownames(tmp.tab)` <- with(
			tmp.tab,
			levels(`rownames(tmp.tab)`)[as.numeric(`rownames(tmp.tab)`)])
		setDT(tmp.tab)

		n_yincat <- with(tmp.tab,
										 sum(grepl("yin", `rownames(tmp.tab)`))
		)
		yincat.first <- first(which(with(tmp.tab,
																		 grepl("yin", `rownames(tmp.tab)`))))
		yincat.last <- last(which(with(tmp.tab,
																	 grepl("yin", `rownames(tmp.tab)`))))

		tmp.tab <- tmp.tab[, .(
			Covariate = NA,
			level = c(
				"Left job",
				'Plant 2',
				'Plant 3',
				'Black',
				if (use_finrace) {'Unknown'},
				paste0(
					substr(`rownames(tmp.tab)`[yincat.first:yincat.last], 17,
								 unlist(regexpr(",", `rownames(tmp.tab)`[yincat.first:yincat.last])) - 1),
					" to ",
					as.numeric(gsub('\\]|)', '', substring(
						`rownames(tmp.tab)`[yincat.first:yincat.last],
						unlist(regexpr(",", `rownames(tmp.tab)`[yincat.first:yincat.last])) + 1))) - c(rep(1, length(yincat.first:yincat.last) - 1), 0),
					""
				),
				if (include_women) {'Female'}),
			HR = paste0(
				ifelse(mathmode, "$", ""),
				formatC(round(exp(coef), digits = 1),
								format = 'f', digits = 1),
				ifelse(mathmode, "$", "")),
			SE = paste0(
				ifelse(mathmode, "$", ""),
				formatC(round(exp(`se(coef)`), digits = 1),
								format = 'f', digits = 1),
				ifelse(mathmode, "$", "")),
			`95\\% CI` = paste0(
				ifelse(mathmode, "$", ""), "(",
				formatC(round(exp(coef - qnorm(0.975) * `se(coef)`), digits = 1),
								format = 'f', digits = 1),
				",\\,",
				formatC(round(exp(coef + qnorm(0.975) * `se(coef)`), digits = 1),
								format = 'f', digits = 1),
				")", ifelse(mathmode, "$", "")
			),
			lower = c(rep(NA, nrow(tmp.tab) - n_yincat),
								substr(`rownames(tmp.tab)`[yincat.first:yincat.last], 17,
											 unlist(regexpr(",", `rownames(tmp.tab)`[yincat.first:yincat.last])) - 1)),
			upper = c(rep(NA, nrow(tmp.tab) - n_yincat),
								gsub('\\]', '', substring(
									`rownames(tmp.tab)`[yincat.first:yincat.last],
									unlist(regexpr(",", `rownames(tmp.tab)`[yincat.first:yincat.last])) + 1)))
		)]

		tmp.tab <- rbindlist(list(
			data.table(
				Covariate = c('Job status'),
				level = "Employed"
			),
			tmp.tab[1,],
			data.table(
				Covariate = c('Plant'),
				level = c("Plant 1")
			),
			tmp.tab[level %in% c("Plant 2", "Plant 3"),],
			data.table(
				Covariate = c('Race'),
				level = c('White')
			),
			tmp.tab[level %in% c("Black", if (use_finrace) {"Unknown"}),],
			data.table(
				Covariate = c('Year of hire'),
				level = {paste0(
					min(year(dat$yin)),
					" to ",
					min(as.numeric(tmp.tab$lower), na.rm = T) - 1)}
			),
			tmp.tab[yincat.first:yincat.last,],
			if (include_women) {data.table(
				Covariate = c('Sex'),
				level = c('Male')
			)},
			if (include_women) {tmp.tab[level == "Female",]}
		), fill = T)
		# jobloss age event counts
		n <- table(dat[get(outcome) == 1,
									 yod > jobloss.date])
		# Plant and race (and sex)
		n <- c(n, unlist(sapply(
			dat[get(outcome) == 1,.(
				plant,
				finrace = if (use_finrace) {finrace},
				race = if (!use_finrace) {race},
				get(paste0(tolower(outcome), "_yin.cat")),
				Sex = if (include_women) {Sex}
			)], table)))

		tmp.tab$n <- paste0(ifelse(mathmode, "$", ""),
												n,
												ifelse(mathmode, "$", ""))

		tmp.tab <- tmp.tab[,.(
			Covariate,
			level,
			`$n$` = n,
			HR,
			`95\\% CI`,
			SE
		)]

		tmp.tab <- rbindlist(list(
			tmp.tab,
			data.table(
				Covariate = splined.covariate,
				level = paste0("splined ",
											 ifelse(mathmode, "$", ""),
											 "(df = ",
											 round(
											 	tmp.coxph$df[
											 		tmp.coxph$pterms == 1],
											 	2),
											 ")",
											 ifelse(mathmode, "$", "")),
				`$n$` = paste0(ifelse(mathmode, "$", ""),
											 summary(tmp.coxph)$nevent,
											 ifelse(mathmode, "$", "")),
				HR = NA,
				`95\\% CI` = NA,
				SE = NA
			)
		))

		names(tmp.tab)[names(tmp.tab) == 'level'] <- "  "

		if (get_finegray) {
			names(tmp.tab)[grep("HR", names(tmp.tab))] <- "csHR"
		}

		assign(paste0("sincehire_", tolower(outcome), ifelse(
			in_ips,
			ifelse(!recent, "_in_ips", "_recent"),
			ifelse(!recent, "_in_full", "_recent")), ".tab"),
			tmp.tab,
			envir = .GlobalEnv, inherits = T)

		saveRDS(tmp.tab,
						file = paste0(
							output.dir, "/",
							paste0(
								"sincehire_", tolower(outcome), ifelse(
									in_ips,
									ifelse(!recent, "_in_ips", "_recent"),
									ifelse(!recent, "_in_full", "_recent")),
								ifelse(is.finite(years.max),
											 paste0("_", years.max),
											 ""),
								ifelse(is.finite(baseline_sinceleavework.max),
											 paste0("_baseline", baseline_sinceleavework.max),
											 ""),
								ifelse(include_women,
											 "_women",
											 ""),
								ifelse(get_finegray, ".fg", ""),
								".tab.rds")))

		outcome
		tmp.tab


	})

}
