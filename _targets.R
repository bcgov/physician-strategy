pacman::p_load(targets, tarchetypes, dplyr)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR"))

params = list(
  pull_from_Oracle = T
)


plan = list()

if (params$pull_from_Oracle) {
  plan = append(plan, list(
    tar_target(encounters_raw, pull_encounters(midpoint = filter(policies, policy == "LFP")$start_date)),
    tar_target(vt4_raw, pull_vt4()))
  )
}

plan = append(plan, list(
  tar_target(policies, create_policies()),
  tar_target(encounters, clean_encounters(encounters_raw)),
  tar_target(vt4, clean_vt4(vt4_raw)),
  tar_target(encounters_fp, get_encounters_fp(encounters, vt4, policies)),
  tar_target(cihi_raw, pull_cihi(file = Sys.getenv("CIHI_PATH"))),
  tar_target(cihi, clean_cihi(cihi_raw, policies))
))

plan
