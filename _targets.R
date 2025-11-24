pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR", "patchwork", "echarts4r", "htmltools"))

targets = list(
  tar_target(file, Sys.getenv("CIHI_PATH"), format = "file"),
  tar_target(cihi, get_cihi_data(file)),
  tar_target(milestones, make_milestones()),
  tar_target(g1, plot1(cihi, 'g')),
  tar_target(e1, plot1(cihi, 'e')),
  tar_target(g2, plot2(cihi, 'g')),
  tar_target(e2, plot2(cihi, 'e')),
  tar_target(g3, plot3(cihi, 'g')),
  tar_target(e3, plot3(cihi, 'e')),
  tar_target(g4, plot4(cihi, 'g')),
  tar_target(e4, plot4(cihi, 'e')),
  tar_target(g5, plot5(cihi, 'g')),
  tar_target(e5, plot5(cihi, 'e'))
)

list(targets)
