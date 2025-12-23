pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR"))

params = list(
  start_date = as.Date('2021-04-01'),
  end_date = as.Date('2025-03-31')
)

list(
  tar_target(policies, create_policies()),

  #tar_target(cihi_file, Sys.getenv("CIHI_PATH"), format = "file"),
  #tar_target(cihi, get_cihi_data(cihi_file)),

  tar_target(msp_raw, pull_msp(params$start_date, params$end_date)),
  tar_target(msp, clean_msp(msp_raw)),
  tar_target(vt4_raw, pull_vt4(params$start_date, params$end_date)),
  tar_target(vt4, clean_vt4(vt4_raw)),

  tar_target(fp, get_fp(msp, vt4, policies))
  # tar_target(fp_yearmon, get_fp_yearmon(fp)),
  # tar_target(fp_trim, get_fp_trim(fp, policy_dates)),
  #
  # tar_target(density_plots, create_density_plots(fp_yearmon, save=T)),
  # tar_target(ts_plots, create_ts_plots(data=fp, LFP=LFP, save = T)),
  # tar_target(t_test_plots, create_t_test_plots(data=fp_trim, save = T)),
  #
  # tar_target(unpaired_tests, create_unpaired_tests(fp_trim, group_vars = c('encounters', 'all_source_pd'))),
  # tar_target(paired_tests, create_paired_tests(fp_trim, group_vars = c('encounters', 'all_source_pd')))
)
