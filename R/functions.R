# NOTES TO SELF:

# - I want FTE, earnings, etc., by month (or week or day?) rather than year
# - can we find out which FPs are on LFP and which aren't?
# - do FPs switch out of hospitalism after LFP?


pacman::p_load(tidyverse, echarts4r, bslib, shinyWidgets, hsiaR, zoo, glue)

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

make_fiscal_year_lookup_table = function(dates) {
  dates_uq = sort(unique(as.Date(dates)))
  tibble(date = dates_uq, fiscal = hsiar::as_fiscal(date))
}

pull_encounters = function(start_date=params$start_date, end_date=params$end_date) {
  con = hiConnect()
  query = "
  SELECT pracnum, servdt, count(clnt_label) as encounters
  FROM (
    SELECT {hiBuildSQL()$select$encounters}
    FROM {hiBuildSQL()$from$msp_join}
    WHERE
      servdt between date '{start_date}' and date '{end_date}'
      and pracnum is not null
      and ({hiBuildSQL()$where$i_dont_know_what_this_does})
      and not ({hiBuildSQL()$where$lab_services})
      and not ({hiBuildSQL()$where$registration_and_15_min_codes})
      and not ({hiBuildSQL()$where$opioid_management})
    )
    GROUP BY pracnum, servdt
    ORDER BY 1,2
  "
  hiQuery(query, con=con)
}

clean_msp = function(msp_raw) {
  stopifnot(sum(is.na(msp_raw)) == 0)

  msp_raw |>
    mutate(encounters = as.integer(encounters)) |>
    mutate(servdt = as.Date(servdt)) |>
    mutate(yearmon = zoo::as.yearmon(servdt)) |>
    mutate(fiscal = as_fiscal(servdt, example_format = "2020/2021"))
}


pull_vt4 = function(start_date=params$start_date, end_date=params$end_date) {
  start_date = hsiaR::as_fiscal(start_date, example_format = "2020/2021")
  end_date = hsiaR::as_fiscal(end_date, example_format = "2020/2021")
  con = hiConnect()
  hiQuery("select * from msea_team_lvl2.vt4 where fiscal between '{start_date}' and '{end_date}'", con=con)
}

clean_vt4 = function(vt4_raw) {
  # maybe make these guys factors cuz that's what they are....
  vt4 = vt4_raw |>
    mutate(across(prac_age, as.integer)) |>
    mutate(across(c(pracnum, ha_cd, hsda_cd, lha_cd, chsa_cd), fct_inseq)) |>
    mutate(across(c(funcspec), ~fct_inseq(as.character(.))))
}

get_fp = function(msp, vt4, policies) {
  fp_specs = policies |>
    filter(policy == "LFP") |>
    pull(specs) |>
    first()

  vt4_fps = vt4 |>
    filter(funcspec %in% fp_specs) |>
    select(pracnum, fiscal, funcspec, ha_cd, prac_age, prac_gender)

  fp = inner_join(msp, vt4_fps, by=join_by(pracnum, fiscal)) |>
    mutate(is_LFP = between(servdt, policies$start_date, policies$end_date))

  stopifnot(
    fp |>
      count(pracnum, servdt) |>
      pull(n) |>
      unique() == 1
  )
  fp
}

get_fp_yearmon = function(fp) fp |>
  group_by(yearmon, is_treated, pracnum) |>
  summarise(
    all_source_pd = sum(all_source_pd),
    encounters = sum(encounters)
  )

get_fp_trim = function(data, policy_dates) {
  n_days = data |>
    group_by(is_treated) |>
    summarise(n_days = n_distinct(servdt))
  plus_minus = n_days[[2,2]]

  df = filter(data, between(servdt, policy_dates$start_date - plus_minus, policy_dates$start_date + plus_minus))

  stopifnot(
    df |>
      group_by(is_treated) |>
      summarise(n_days = n_distinct(servdt)) |>
      pull(n_days) |>
      unique() |>
      length() == 1
  )

  df
}


save_plot = function(filename, plot) ggsave("output/" %,% filename %,% ".png", width = 8, height = 5, dpi = 300)

create_density_plot = function(data, filename, save=F, x=NULL, y=NULL, color=NULL, geoms, labels_x=NULL, labels_y=NULL, legend.position='bottom', title=NULL, ...) {
  aes_list = list()
  dots = list(...)
  if (!is.null(x)) aes_list$x = ensym(x)
  if (!is.null(y)) aes_list$y = ensym(y)
  if (!is.null(color)) aes_list$color = ensym(color)

  g = ggplot(data, aes(!!!aes_list))
  g = g + geoms
  g = g + scale_colour_viridis_d()
  g = g + ggthemes::theme_clean(base_size = 14)
  g = g + theme(legend.position = legend.position)
  if (!is.null(labels_x)) g = g + scale_x_continuous(labels = labels_x)
  if (!is.null(labels_y)) g = g + scale_y_continuous(labels = labels_y)
  if (!is.null(title)) if (length(title) == 2) g = g + ggtitle(title[[1]], title[[2]]) else g = g + ggtitle(title)
  g = g + dots
  if (save) save_plot(filename, g)
  g
}

LFP_plot = function(data, LFP, group_vars = c("yearmon", "is_treated"), value = "encounters", .f = sum, include_geom_smooth = T) {
  group_vars = syms(group_vars)
  x = group_vars[[1]]
  value = sym(value)
  g = data |>
    group_by(!!!group_vars) |>
    summarise(!!value := .f(!!value)) |>
    ungroup() |>
    ggplot() +
    aes(x=!!x, y=!!value, color=is_treated) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = as.yearmon(LFP$start_date), color='red', linetype = 'dashed') +
    ggthemes::theme_clean(base_size = 14) +
    scale_color_viridis_d() +
    theme(legend.position = 'none') +
    labs(x=NULL, y=NULL)
  if (include_geom_smooth) g = g + geom_smooth(method = 'lm', se = F, linetype = 'dashed', linewidth = .5)
  g
}

create_unpaired_tests = function(data, group_vars=c('encounters', 'all_source_pd')) {
  group_vars = syms(group_vars)
  data |>
    select(is_treated, encounters, all_source_pd) |>
    pivot_longer(cols = c(encounters, all_source_pd), names_to = 'variable') |>
    nest(data = c(is_treated, value)) |>
    mutate(t_test = map(data, ~t.test(value ~ is_treated, data = .))) |>
    mutate(wilcox_test = map(data, ~wilcox.test(value ~ is_treated, data = .))) |>
    select(-data) |>
    mutate(across(c(t_test, wilcox_test), ~map(., broom::tidy)))
}


create_paired_tests = function(data, group_vars=c('encounters', 'all_source_pd')) {
  group_vars = syms(group_vars)

  x=data |>
    select(pracnum, is_treated, !!!group_vars) |>
    group_by(pracnum, is_treated) |>
    summarise(across(c(!!!group_vars), mean)) |>
    ungroup() |>
    pivot_wider(names_from = is_treated, values_from = c(!!!group_vars)) |>
    na.omit()

  t_test = map(group_vars, function(var) {
    z = select(x, starts_with(as.character(var)))
    t.test(z[[1]], z[[2]], paired=T) |> broom::tidy()
  }) |>
    bind_rows() |>
    mutate(variable = as.character(group_vars), .before=1) |>
    nest(.by = variable)

  wilcox_test = map(group_vars, function(var) {
    z = select(x, starts_with(as.character(var)))
    wilcox.test(z[[1]], z[[2]], paired=T) |> broom::tidy()
  }) |>
    bind_rows() |>
    mutate(variable = as.character(group_vars), .before=1) |>
    nest(.by = variable)

  tibble(variable = as.character(group_vars), t_test = t_test$data, wilcox_test = wilcox_test$data)
}

