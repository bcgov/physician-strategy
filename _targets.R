pacman::p_load(targets, tarchetypes, dplyr)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR"))

params = list(pull_from_Oracle = T)
plan = list()

if (params$pull_from_Oracle) {
  plan = append(plan, list(
    tar_target(msp_raw, pull_msp()),
    tar_target(vt4_raw, pull_vt4()))
  )
}

plan = append(plan, list(
  tar_target(policies, create_policies()),
  tar_target(fitms, pull_fitms()),
  tar_target(msp, clean_msp(msp_raw)),
  tar_target(vt4, clean_vt4(vt4_raw)),
  tar_target(msp_fp, get_msp_fp(msp, vt4, fitms, policies)),
  tar_target(cihi_raw, pull_cihi(file = Sys.getenv("CIHI_PATH"))),
  tar_target(cihi, clean_cihi(cihi_raw, policies)),

  tar_target(vt4_switches, create_vt4_switches(vt4, policies))
))

plan

# tar_make()
# px = tar_make(callr_function = callr::r_bg); tar_watch(display = 'graph')
