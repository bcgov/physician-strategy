create_policies = function() {
  tribble(~policy, ~start_date, ~end_date, ~specs, ~comments,
    "LFP", "2023-02-01", NA, as.integer(c(0, 50, 76, 77)), "Family Medicine"
  ) |>
    mutate(across(c(start_date, end_date), ymd)) |>
    mutate(end_date = replace_na(end_date, as.Date(Inf)))
}

create_ts_plots = function(data, LFP, save=F) {
  LFP_plot(data, LFP) +
    scale_y_continuous(labels = function(x) x/10^6) +
    labs(y='MSP encounters (M)') +
    ggtitle("Total MSP encounters by Month", "Encounters fall after LFP")
  if (save) save_plot('ts_enc_plot')

  LFP_plot(data, LFP, value = "pracnum", .f = n_distinct) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(y="# distinct FPs") +
    ggtitle("# distinct FP pracs per month", "FPs grow steadily until LFP; decline about 1 year after LFP, possibly accelerating")
  if (save) save_plot('ts_pracs_plot')

  data |>
    group_by(yearmon, is_treated, pracnum) |>
    summarise(encounters = sum(encounters)) |>
    LFP_plot(LFP, .f=mean) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(y="Mean encounters per month") +
    ggtitle("Average # encounters per FP per month", "Encounters per FP has been falling continuously; slightly faster after LFP")
  if (save) save_plot('ts_enc_per_prac_plot')

  LFP_plot(data, LFP, value='all_source_pd') +
    scale_y_continuous(labels = scales::label_dollar(scale = 1/10^9, suffix = "B", accuracy = 1)) +
    labs(y="All source paid") +
    ggtitle("All Source Paid to FPs over time", "Total payments have been rising continuously with a large jump with LFP,\nfollowed by decline")
  if (save) save_plot('ts_pd_plot')

  data |>
    group_by(yearmon, is_treated, pracnum) |>
    summarise(all_source_pd = sum(all_source_pd)) |>
    LFP_plot(LFP, value='all_source_pd', .f=mean) +
    scale_y_continuous(labels = scales::label_dollar()) +
    ggtitle("All Source Paid to FPs over time", "Total payments have been rising continuously with a large jump with LFP,\nfollowed by decline")
  if (save) save_plot('ts_pd_per_prac_plot')
}

create_density_plots = function(data, save=F) {
  create_density_plot(data, 'density_pd', save, x='all_source_pd', color = 'is_treated', geoms = geom_density(), labels_x = scales::label_dollar(scale = 1/10^6, suffix = "M"), title = c("Density of all_source_pd per month", "I doubt someone is actually making $80M/month"))

  create_density_plot(data, 'box_pd', save, y='all_source_pd', color = 'is_treated', geoms = geom_boxplot(), labels_y = scales::label_dollar(scale = 1/10^6, suffix = "M"), title = c("Boxplot of all_source_pd per month", "There are many outliers"))

  create_density_plot(data, 'density_enc', save, x='encounters', color = 'is_treated', geoms = geom_density(), title = c("Density of # encounters per month", "Once again, the skew is very right-tailed"))

  create_density_plot(data, 'box_enc', save, y='encounters', color = 'is_treated', geoms = geom_boxplot(), title = c("Boxplot of # encounters per month", "And again, there are many outliers"))
}

create_t_test_plot = function(data, value) {
  if (is.character(value)) value = sym(value) else value = ensym(value)
  data |>
    group_by(pracnum, is_treated) |>
    summarise(value = sum(!!value)) |>
    ungroup() |>
    select(pracnum, is_treated, value) |>
    pivot_wider(names_from = is_treated, values_from = value) |>
    na.omit() |>
    ggplot(aes(x=`FALSE`, y=`TRUE`)) +
    geom_point(alpha = .3, color = viridis::viridis_pal()(1)) +
    ggthemes::theme_clean() +
    geom_abline(slope = 1) +
    ggtitle(str_to_title(value) %,% " per day, Before and After LFP") +
    labs(x="Before LFP", y="After LPF")
}

create_t_test_plots = function(data, vars = c('encounters', 'all_source_pd'), save = F) {
  map(vars, function(var) {
    g = create_t_test_plot(data, var)
    if (save) save_plot(filename = "t_test_plot_" %,% var)
    g
  })
}
