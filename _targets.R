pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR"))
ggplot2::theme_set(ggthemes::theme_clean())

list(
  tar_target(cihi_file, Sys.getenv("CIHI_PATH"), format = "file"),
  tar_target(cihi, get_cihi_data(cihi_file)),

  tar_target(policy_dates, create_policy_dates()),
  tar_target(LFP, filter(policy_dates, anything_else == 'Family medicine')),

  tar_target(msp_raw, pull_msp()),
  tar_target(msp, clean_msp(msp_raw)),
  tar_target(vt4_raw, pull_vt4()),
  tar_target(vt4, clean_vt4(vt4_raw)),

  tar_target(fp, get_fps(msp, vt4, policy_dates)),
  tar_target(fp_per_prac, get_fp_per_prac(fp)),

  tar_target(fp_per_prac_density_pd_plot, create_fp_per_prac_density_pd_plot(fp_per_prac)),
  tar_target(fp_per_prac_box_pd_plot, create_fp_per_prac_box_pd_plot(fp_per_prac)),
  tar_target(fp_per_prac_density_enc_plot, create_fp_per_prac_density_enc_plot(fp_per_prac)),
  tar_target(fp_per_prac_box_enc_plot, create_fp_per_prac_box_enc_plot(fp_per_prac)),
  tar_target(fp_per_prac_corr_enc_plot, create_fp_per_prac_corr_enc_plot(fp_per_prac)),

  tar_target(ts_enc_plot, create_ts_enc_plot(fp, LFP)),
  tar_target(ts_pracs_plot, create_ts_pracs_plot(fp, LFP)),
  tar_target(ts_enc_per_prac_plot, create_ts_enc_per_prac_plot(fp, LFP)),
  tar_target(ts_pd_plot, create_ts_pd_plot(fp, LFP)),
  tar_target(ts_pd_per_prac_plot, create_ts_pd_per_prac_plot(fp, LFP))
)
