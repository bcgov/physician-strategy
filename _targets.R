pacman::p_load(targets, tarchetypes)
tar_source()
tar_option_set(packages = c("tidyverse", "hsiaR", "patchwork", "echarts4r", "htmltools", "shiny", "bslib", "shinyWidgets"))

targets = list(
  tar_target(cihi_file, Sys.getenv("CIHI_PATH"), format = "file"),
  tar_target(cihi, get_cihi_data(cihi_file)),
  # tar_target(milestones, make_milestones()),
  tar_target(cihi_data_1, prep_cihi_data_1(cihi)),
  tar_target(ui_1, make_ui_1(cihi_data_1)),
  tar_target(g_1, plot_g_1(cihi_data_1)),
  tar_target(e_1, plot_e_1(cihi_data_1))
  # tar_target(c1, prep_plots1(cihi))
  # tar_target(g1, plot1(cihi, 'g')),
  # tar_target(e1, plot1(cihi, 'e')),
  # tar_target(g2, plot2(cihi, 'g')),
  # tar_target(e2, plot2(cihi, 'e')),
  # tar_target(g3, plot3(cihi, 'g')),
  # tar_target(e3, plot3(cihi, 'e')),
  # tar_target(g4, plot4(cihi, 'g')),
  # tar_target(e4, plot4(cihi, 'e')),
  # tar_target(g5, plot5(cihi, 'g')),
  # tar_target(e5, plot5(cihi, 'e')),
  # tar_target(ui2, print_ui2(cihi)),
  # tar_target(ui5, print_ui5(cihi))
)

list(targets)
