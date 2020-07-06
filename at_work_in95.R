full_censored <- as.data.table(as.data.frame(cohort_analytic[
	year >= 1941 & year(yin) <= 1982 &
		(year <= year(yoc) | is.na(yoc)) &
		immortal == 0 &
		year >= 1970 &
		year(jobloss.date) >= 1970 &
		time_length(difftime(yin, yob), 'years') < 65 &
		year(jobloss.date) >= 1995 & sex == "M"
	]))

full_censored[yod < jobloss.date & (Overdose == 1)]

full_censored[, `:=`(
	yclose = {
	tmp <- rep("2012-12-31", .N)
	tmp[plant == "2"] <- "2010-12-31"
	tmp[plant == "3"] <- "2014-12-31"
	as.Date(tmp)
},
plant = factor(plant, levels = c(1:3),
							 labels = paste('Plant', 1:3)))]

full_censored[, `:=`(
	sinceclosing = {
		tmp <- time_length(
			difftime(paste0(year + 1, "-01-01"), yclose), "year")
		tmp[year == year(yod) & !is.na(yod)] <- time_length(
			difftime(yod[year == year(yod) & !is.na(yod)] + days(1),
							 yclose[year == year(yod) & !is.na(yod)]), "year")
		tmp
		}
)]

# Cumulative incidence?
setorder(full_censored, plant, sinceclosing)
full_censored[,`:=`(
	`Cumulative suicide` = cumsum(Suicide),
	`Cumulative overdose` = cumsum(Overdose),
	`Cumulative despair` = cumsum(Suicide == 1 | Overdose == 1)
), by = .(plant)]
full_censored[,`:=`(
	`At risk` = length(table(studyno))
), by = .(plant, year)]

rect.dat <- data.frame(
	lower = sapply(c("2012-12-31",
									 "2010-12-31",
									 "2014-12-31"), function(x) {
									 	time_length(difftime("1994-12-31", x), 'years')
									 }	),
	upper = sapply(c("2012-12-31",
									 "2010-12-31",
									 "2014-12-31"), function(x) {
									 	time_length(difftime("2015-12-31", x), 'years')
									 }	),
	plant = paste('Plant', 1:3)
)

ggplot() +
	geom_rect(data = rect.dat,
		aes(
			xmin = lower,
			xmax = upper,
			ymin = 0,
			ymax = Inf
		),
		fill = "forestgreen",
		alpha = 0.1) +
	geom_vline(xintercept = 0, color = "salmon") +
	geom_histogram(
		data = full_censored[Suicide == 1 | Overdose == 1],
		aes(x = sinceclosing),
		breaks = seq(-20, 5, 1),
		closed = 'right',
		fill = "grey",
		col = "black") +
	facet_wrap(. ~ plant) +
	scale_y_continuous(breaks = seq(0, 10, 1)) +
	labs(x = "Years to plant closing",
			 y = "Count") + mytheme

ggplot() +
	geom_rect(data = rect.dat,
		aes(
			xmin = lower,
			xmax = upper,
			ymin = 0,
			ymax = Inf
		),
		fill = "forestgreen",
		alpha = 0.1) +
	geom_vline(xintercept = 0, color = "salmon") +
	geom_step(
		data = full_censored[year >= 1995],
		aes(x = sinceclosing,
				y = `Cumulative despair`),
		col = "black") +
	geom_step(
		data = full_censored[year >= 1995],
		aes(x = sinceclosing,
				y = `At risk`/100),
		col = "darkgrey") +
	facet_wrap(. ~ plant) +
	scale_y_continuous(
		breaks = seq(0, 100, 5),
	 sec.axis = sec_axis(~ . * 100,
	 										breaks = seq(0, 5000, 500),
	 										name = "Persons at risk")) +
	labs(x = "Years to plant closing",
			 y = "Cumulative case count") + mytheme
