pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR", "patchwork", "echarts4r", "htmltools", "shiny", "bslib", "shinyWidgets"))

list(
  tar_target(cihi_file, Sys.getenv("CIHI_PATH"), format = "file"),
  tar_target(cihi, get_cihi_data(cihi_file)),

  tar_target(policy_dates, create_policy_dates()),

  tar_target(encounters_raw, pull_encounters()),
  tar_target(encounters, clean_encounters(encounters_raw)),
  tar_target(vt4_raw, pull_vt4()),
  tar_target(vt4, clean_vt4(vt4_raw)),

  tar_target(encounters_fps, get_encounters_fps(encounters, vt4, policy_dates))

)
