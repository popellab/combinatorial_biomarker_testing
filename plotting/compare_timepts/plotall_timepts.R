# main R script to generate figures comparing different time points

# folder names/time points
timepts <- c("baseline,d15,d30,relative_d15,relative_d30")
timepts_name <- c("baseline,day15,day30,relchangeday15,relchangeday30")

# plots density ridges comparing different time pts (for single biomarkers)
system(paste("Rscript plot_density_ridges.R", timepts, timepts_name))

# plots perc increase in predictive power at the specified time pt with respect to a ref. timept
# commandline args: timept_new, timept_ref
system(paste("Rscript perc_increase.R", "d15", "baseline"))
system(paste("Rscript perc_increase.R", "d30", "baseline"))
system(paste("Rscript perc_increase.R", "relative_d15", "baseline"))
system(paste("Rscript perc_increase.R", "relative_d30", "baseline"))

#system(paste("Rscript perc_increase_d15_filtered.R", "d15", "baseline"))
system(paste("Rscript perc_increase_filtered.R", "relative_d15", "baseline"))

# plot bootstrapped results
system(paste("Rscript plot_bootstrap_timepts.R", timepts))
