pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR", "patchwork", "echarts4r", "htmltools", "shiny", "bslib", "shinyWidgets"))

list(
  tar_target(cihi_file, Sys.getenv("CIHI_PATH"), format = "file"),
  tar_target(cihi, get_cihi_data(cihi_file)),
  tar_target(policy_dates, create_policy_dates()),

  tar_target(cihi_data_1, prep_cihi_data_1(cihi)),
  tar_target(ui_1, make_ui_1(cihi_data_1)),
  tar_target(g_1, plot_g_1(cihi_data_1)),
  tar_target(e_1, plot_e_1(cihi_data_1)),

  tar_target(encounters_raw, encounters_fn()),
  tar_target(encounters, clean_encounters(encounters_raw)),
  tar_target(vt4_raw, vt4_fn()),
  tar_target(vt4, clean_vt4(vt4_raw)),

  tar_target(encounters_fps, get_encounters_fps(encounters, vt4, policy_dates))

)


# tar_make in the background?
# px = tar_make(callr_function = callr::r_bg)
# tar_watch()
