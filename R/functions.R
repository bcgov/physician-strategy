# setup_fn = function() {
#   invisible(extrafont::choose_font("BC Sans", quiet = T))
#   extrafont::loadfonts()
#
#   theme_set(ggthemes::theme_clean(base_size = 14, base_family = "BC Sans"))
#   e_common(font_family = "BC Sans", theme = "Westeros")
# }

ggpad = function(m=.1) scale_x_continuous(expand = expansion(mult = m))

get_cihi_data = function(file = Sys.getenv("CIHI_PATH")) {
  readxl::read_excel(file, sheet="Table 1", range = "A3:BH84800") |>
    rename_with(tolower) |>
    rename_with(~str_remove_all(., "\\r|\\n|:")) |>
    rename_with(~str_replace_all(., "/", " ")) |>
    rename_with(~str_replace_all(., " ", "_")) |>
    rename_with(~str_replace_all(., fixed("\u2013"), "-")) |> # replace en-dashes -- yes, it matters
    rename(phys_pop_ratio = 'physician-to-100,000_population_ratio') |>
    mutate(across(c("year", "specialty_sort", "statistics_canada_population", "net_migration_between_canadian_jurisdictions", starts_with(c("number", "age_group", "place_of", "university_of", "years_since"))), as.integer)) |>
    mutate(across(c("phys_pop_ratio", starts_with(c("average", "median"))), as.double)) |>
    mutate(across(c(jurisdiction, health_region, specialty), fct))
}

plot_1 = function(data, type) {
  df = data |>
    filter(
      jurisdiction == 'B.C.',
      health_region == 'B.C.',
      specialty == "All physicians"
    )

  if (type == 'gg') {
    df |>
      ggplot(aes(x=year, y=number_of_physicians, color=jurisdiction)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      theme(legend.position = 'none') +
      scale_y_continuous(labels = scales::label_comma()) +
      ggtitle("Physicians in B.C.") +
      ggpad()
  } else {
    df |>
      mutate(year = fct(as.character(year))) |>
      e_charts(x = year) |>
      e_line(number_of_physicians) |>
      e_title("# Physicians", "BC") |>
      e_tooltip('axis') |>
      e_y_axis(number_of_physicians)
  }
}

plot_2 = function(data, type) {
  good_provinces = c("Canada", "B.C.", "Alta.", "Ont.", "Man.")

  df = data |>
    filter(
      specialty == "All physicians",
      health_region %in% good_provinces
    )

  if (type == 'gg') {
    df |>
      mutate(is_bc = health_region == 'B.C.') |>
      mutate(is_bc_scale = case_when(is_bc ~ 1.5, T ~ 1)) |>
      ggplot(aes(x=year, y=phys_pop_ratio, color=jurisdiction, linewidth=is_bc_scale)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::label_comma()) +
      guides(linewidth = 'none', size = 'none') +
      ggtitle("Physicians per Pop - across Canada") +
      theme(legend.position = 'bottom') +
      ggpad()
  } else {
    df |>
      mutate(year = fct(as.character(year))) |>
      group_by(jurisdiction) |>
      e_charts(x = year) |>
      e_line(serie = phys_pop_ratio) |>
      e_title("# Physicians", "Canada/Provinces") |>
      e_tooltip('axis') |>
      e_theme("westeros") |>
      e_y_axis(phys_pop_ratio, formatter = e_axis_formatter("decimal"), margin=30)
  }
}

plot_3 = function(data, type) {
  df = data |>
    filter(
      jurisdiction == 'B.C.',
      health_region == 'B.C.',
      specialty == "All physicians"
    )

  df2 = data |>
    filter(
      jurisdiction == 'B.C.',
      health_region == 'B.C.',
      specialty == "All physicians",
      year %in% c(2015, 2020, 2024)
    ) |>
    select(year, starts_with("age_group")) |>
    rename("00-30" = age_group_younger_than_30, "80-100" = age_group_80_and_older) |>
    select(-age_group_unknown) |>
    pivot_longer(cols = 2:last_col()) |>
    mutate(name = str_extract(name, "\\d\\d-\\d\\d")) |>
    rename(age_group = name)

  df3 = df2 |>
    group_by(year) |>
    summarise(total_pop = sum(value)) |>
    inner_join(df2) |>
    mutate(prop_phys = value / total_pop) |>
    mutate(year = fct(as.character(year)))

  if (type == 'gg') {
    g1 = df |>
      ggplot(aes(x=year, y=median_age)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      theme(legend.position = 'none') +
      scale_y_continuous(labels = scales::label_comma()) +
      ggtitle("Median Physician Age") +
      ggpad()

    g2 = df3 |>
      ggplot(aes(x=age_group, y=prop_phys, color=year, group=year)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::label_percent()) +
      ggtitle("Relative Age Distribution by Year") +
      theme(legend.position = 'bottom')

    g1 + g2
  } else {
    e1 = df |>
      mutate(year = fct(as.character(year))) |>
      e_chart(x=year) |>
      e_line(median_age) |>
      e_y_axis(median_age) |>
      e_title("Median Physician Age", "BC") |>
      e_tooltip('axis')

    e2 = df3 |>
      group_by(year) |>
      e_chart(x=age_group) |>
      e_line(prop_phys) |>
      e_title("Relative Age Distribution by Year") |>
      e_tooltip('axis') |>
      e_legend(right=0) |>
      e_y_axis(formatter = e_axis_formatter('percent'))

    browsable(
      div(
        style = "display: flex; gap: 20px;",
        div(style = "flex: 1;", e1),
        div(style = "flex: 1;", e2)
      )
    )
  }
}

plot_4 = function(data, type) {
  df = data |>
    filter(
      jurisdiction == 'B.C.',
      health_region == 'B.C.',
      specialty == "All physicians"
    ) |>
    select(year, number_of_physicians, number_female, number_male) |>
    mutate(across(3:4, ~./number_of_physicians)) |>
    rename_with(~str_replace_all(., "number", "prop"), 3:4) |>
    select(year, starts_with("prop")) |>
    pivot_longer(2:3) |>
    mutate(name = str_remove(name, "prop_"))

  if (type == 'gg') {
    df |>
      ggplot(aes(x=year, y=value, color=name)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      theme(legend.position = 'bottom') +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(color = NULL) +
      ggtitle("Gender Distribution") +
      ggpad()
  } else {
    df |>
      mutate(year = fct(as.character(year))) |>
      group_by(name) |>
      e_chart(x=year) |>
      e_line(value) |>
      e_title("Gender Distribution") |>
      e_y_axis(formatter = e_axis_formatter('percent')) |>
      e_tooltip('axis')
  }
}
