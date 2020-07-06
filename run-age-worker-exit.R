# Run age at leaving work analyses ####
# February 13, 2020
# Kevin Chen
# rm(list = ls()[-which(grepl('cohort_full|cohort_ips', ls()))])

yout.which <- "employment_end.date"

if (!grepl("gm", getwd(), ignore.case = T)) {
	if ("here" %in% .packages()) {
		detach("package:here", unload = T)
	}
	setwd('gm')
	library(here)
}

source(here::here("despair", "age-worker-exit.R"))

# # Descriptive plots and tables ####
# get.hist(hist.which = c("age", "time" , "suicide", "despair"))
# lualatex(pattern = "hist.*recent.*\\.tex",
# 				 directory = here::here("reports/resources"))
#
# get.cruderisk_bycalendar()
# lualatex(pattern = ".*rate.*recent.*\\.tex",
# 				 directory = here::here("reports/resources"))
#
# # Get analytic datasets ####
# rm(list = grep("cohort_since", ls(), value = T))
# get.coxph(rebuild_cohort_leftwork = T, run_model = F)
#
# # Run models ####
# get.coxph()
# Sys.sleep(10)
#
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.coxph(years.max = 5)
# Sys.sleep(10)
#
# # Get coefficients ####
# get.coef(mathmode = T)
# get.coef(years.max = 5, mathmode = T)
#
# # Get splines ####
# get.spline()
# Sys.sleep(10)
# get.spline(years.max = 5)
# Sys.sleep(10)
# lualatex(".*age.*recent.*\\.tex",
# 				 here::here("reports/resources/splines"),
# 				 break_after = 25)
#
# # Simple Cox PH ####
# if (sum(grepl("cohort_leftwork", ls())) > 0) {
# 	rm(list = grep("cohort_leftwork", ls(), value = T))}
#
# # Get analytic datasets ####
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplecoxph(run_model = F, rebuild_cohort_sincehire = T)
# Sys.sleep(10)
#
# # Run simple models ####
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplecoxph()
# Sys.sleep(10)
#
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplecoxph(years.max = 5)
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# Sys.sleep(10)
#
# # Get coefficients ####
# get.simplecoef(mathmode = T)
# get.simplecoef(years.max = 5, mathmode = T)
#
# # Get splines ####
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplespline()
# lualatex(".*year.*recent\\.tex",
# 				 here::here("reports/resources/splines"))
#
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplespline(years.max = 5)
# lualatex(".*year.*recent_5\\.tex",
# 				 here::here("reports/resources/splines"))
#
# # Fuzz 7 days ####
#
# # Run models ####
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplecoxph(baseline_sinceleavework.max = 7)
# Sys.sleep(10)
#
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplecoxph(years.max = 5, baseline_sinceleavework.max = 7)
# Sys.sleep(10)
#
# # Get coefficients ####
# get.simplecoef(baseline_sinceleavework.max = 7, mathmode = T)
# get.simplecoef(years.max = 5, baseline_sinceleavework.max = 7, mathmode = T)
#
# # Get splines ####
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplespline(baseline_sinceleavework.max = 7)
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# lualatex(".*year.*recent_baseline7\\.tex",
# 				 here::here("reports/resources/splines"))
#
# rm(list = grep("tmp*.coxph|suicide*.coxph|despair*.coxph", ls(), value = T))
# get.simplespline(years.max = 5, baseline_sinceleavework.max = 7)
# lualatex(".*year.*recent_5_baseline7\\.tex",
# 				 here::here("reports/resources/splines"))
#
#
# # Table 1 RECENT sub-cohort ####
# dat <- as.data.table(as.data.frame(cohort_recent[
# 	sex == "M"]))
#
# # All records
# source(here::here("reports", "table1.R"))
# assign("all_recent.tab1",
# 			 get.ips_tab1(df = dat,
# 			 						 table_engine = table.engine,
# 			 						 since_leavework = T,
# 			 						 mathmode = T))
#
# saveRDS(all_recent.tab1,
# 				file = here::here(paste0("reports/resources"),
# 													"all_recent.tab1.rds")
# )
#
# # Known date of leaving work
# assign("known_recent.tab1",
# 			 get.ips_tab1(df = dat[year(jobloss.date) < 1995],
# 			 						 table_engine = table.engine,
# 			 						 since_leavework = T,
# 			 						 mathmode = T))
#
# saveRDS(known_recent.tab1,
# 				file = here::here(paste0("reports/resources"),
# 													"known_recent.tab1.rds")
# )