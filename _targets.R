library(targets)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR", "patchwork", "echarts4r", "htmltools"))

list(
  tar_target(file, Sys.getenv("CIHI_PATH"), format = "file"),
  tar_target(cihi, get_cihi_data(file)),
  tar_target(gg1, plot_1(cihi, 'gg')),
  tar_target(e1, plot_1(cihi, 'e')),
  tar_target(gg2, plot_2(cihi, 'gg')),
  tar_target(e2, plot_2(cihi, 'e')),
  tar_target(gg3, plot_3(cihi, 'gg')),
  tar_target(e3, plot_3(cihi, 'e')),
  tar_target(gg4, plot_3(cihi, 'gg')),
  tar_target(e4, plot_3(cihi, 'e'))
)
