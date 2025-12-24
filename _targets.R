pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR"))

params = list(
  start_date = as.Date('2021-04-01'),
  end_date = as.Date('2025-03-31')
)

list(
  tar_target(policies, create_policies()),

  tar_target(encounters_raw, pull_encounters(params$start_date, params$end_date)),
  tar_target(encounters, clean_encounters(encounters_raw)),
  tar_target(vt4_raw, pull_vt4(params$start_date, params$end_date)),
  tar_target(vt4, clean_vt4(vt4_raw)),

  tar_target(encounters_fp, get_encounters_fp(encounters, vt4, policies))
)
