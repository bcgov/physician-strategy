pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR"))

list(
  tar_target(msp_encounters_raw, pull_msp_encounters()),
  tar_target(msp_fitms_raw, pull_msp_fitms()),
  tar_target(vt4_raw, pull_vt4()),
  tar_target(fitms, pull_fitms()),
  tar_target(policies, create_policies()),
  tar_target(msp_encounters, clean_msp(msp_encounters_raw)),
  tar_target(msp_fitms, clean_msp(msp_fitms_raw)),
  tar_target(vt4, clean_vt4(vt4_raw)),
  tar_target(msp_fp_encounters, get_msp_fp(msp_encounters, vt4, policies)),
  tar_target(msp_fp_fitms, get_msp_fp(msp_fitms, vt4, policies)),
  tar_target(cihi_raw, pull_cihi(file = Sys.getenv("CIHI_PATH"))),
  tar_target(hsptlst_raw, pull_hsptlst(file = Sys.getenv("HSPTLST_PATH"))),
  tar_target(cihi, clean_cihi(cihi_raw, policies)),
  tar_target(vt4_switches, create_vt4_switches(vt4, policies))
)


# tar_outdated()
# tar_make()
# px = tar_make(callr_function = callr::r_bg); tar_watch(display = 'graph')
