# script to generate figures for each timepoint
timept = "baseline"
consider_sd = "resp" # "resp" for responder; "nonresp" for nonresponder

# generate upset plots comparing single biomarkers with combinations
# dependencies: count_matches.R; iterate_count_matches.R; generate_upset_plots.R
source('multi_biomarkers_compare.R')

# plot density ridges
source('plot_density_ridges.R')

# for on-treatment - to exclude const quantities
#source('plot_density_ridges_v2.R')

# stacked bar plots: fraction of patients with different response statuses
# dependencies: filter_patients_single.R;filter_patients_combo.R; plot_CRPR_SD_PD.R
system(paste('Rscript recist_class_count.R', timept))

# generate grouped bar plots: fraction of responders & non-responders
# dependencies: plot_frac_resp_nonresp.R
source('frac_resp_nonresp.R')

# bootstrapped results for top biomarkers
#dependencies: bootstrap_single.R; bootstrap_combo.R
system(paste('Rscript bootstrap_main.R', timept, consider_sd))
system(paste('Rscript bootstrap_main_compartments.R', timept, consider_sd))

