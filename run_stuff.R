tar_visnetwork()
tar_outdated()

px = tar_make(callr_function = callr::r_bg); tar_watch(display = 'graph')








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



#tar_target(cihi_file, Sys.getenv("CIHI_PATH"), format = "file"),
#tar_target(cihi, get_cihi_data(cihi_file)),
# tar_target(fp_yearmon, get_fp_yearmon(fp)),
# tar_target(fp_trim, get_fp_trim(fp, policy_dates)),
#
# tar_target(density_plots, create_density_plots(fp_yearmon, save=T)),
# tar_target(ts_plots, create_ts_plots(data=fp, LFP=LFP, save = T)),
# tar_target(t_test_plots, create_t_test_plots(data=fp_trim, save = T)),
#
# tar_target(unpaired_tests, create_unpaired_tests(fp_trim, group_vars = c('encounters', 'all_source_pd'))),
# tar_target(paired_tests, create_paired_tests(fp_trim, group_vars = c('encounters', 'all_source_pd')))







df = hiQuery("select
  count(1) as msp_encounters, pracnum, fiscal
from
  (SELECT distinct
     clnt.mrg_clnt_anon_idnt_id as clnt_label, msp.pracnum,
     trunc(servdt) as servdt, srv_fisc_yr fiscal
   FROM ahip.AR_MSPCLM_CORE_CAN msp
     left join AHIP.CB_DTL_DM_CLNT_VW clnt on
       clnt.label = msp.clnt_label /*client dimention*/
     left join MSEA_TEAM_LVL2.LAB_PROVIDER lab on
       msp.payenum=LPAD(lab.payenum, 5, '0')
     left join ahip.cb_dtl_dm_srv_date_vw dt on
       msp.servdt = dt.srv_date
     left join ahip.fitmds fitm on msp.fitm=fitm.fitm
   where
     pracnum is not null
     AND to_number(substr(period_id(servdt, 'F'),1,4)) >= 2010
     and (clmtp in ('MM','MA','MB','MH','MN','MS','PM') or (servcd=13 and clmtp = 'PB'))
     AND msp.fitm not in (15501,15601)
     and (( PAYESTAT IN ('Y','F')  AND (ENCTR_CLM_MSPD IS NOT NULL  OR
        (ENCTR_CLM_MSPD IS NULL AND PAIDSERV >0) ))
        OR     (PAYESTAT NOT IN ('Y','F')  AND PAIDSERV >0))
     /* Remove lab services of lab providers*/
     AND not (
      lab.payenum is not null and ((servcd = 93 and payestat in ('C', 'H', 'L')) or (servcd = 94 and msp.fitm <> 90665) or (servcd = 94 and msp.fitm = 90665 and payestat in ('C', 'H', 'L')) or (servcd = 98 and msp.fitm in (00012,90000) and payestat in ('C', 'H', 'L')))
      )
     /* Remove registration and 15min LFP codes*/
     AND   NOT(
      (fitmdesc like '%15 MIN%' and fitmdesc like '%LFP%') or
      fitmdesc like '%INCENTIVE%' or
      fitmdesc like '%PCN PANEL%' or
      fitmdesc like '%PRIMARY CARE PANE%' or
      fitmdesc like '%PCN ATTACH%' or
      fitmdesc like '%PCN DETACH%' or
      fitmdesc like '%PCN PANEL%' or
      fitmdesc like '%MANAGEMENT FEE%' or
      fitmdesc like '%CARE TIME%' or
      fitmdesc like '%ENROLMENT CODE%' or
      fitmdesc like '%REGISTRATION CODE%' OR
      fitmdesc like '%TRANSITION CODE%'  OR
      fitmdesc like '%PREMIUM%' OR
      fitmdesc like '%SURCHARGE%'
     )
     /* Remove opioid management*/
     AND msp.fitm<>39
     AND NOT servcd in (12,17,9,19,29,49,71)
     AND msp.fitm not in (98111,98112)
  )
group by
  pracnum, fiscal
", con=hiConnect())


hiBuildSQL = function (where = "'servdt > date '2009-01-01'")
{
  glue::glue("\n  select {hiBuildSQL_select()$encounters}\n  from {hiBuildSQL_from()$msp_join}\n  where\n    {where}\n    and {hiBuildSQL_where()$encounters}\n  order by 1,2,3\n  ")
}

pull_encounters = function(start_date=params$start_date, end_date=params$end_date) {
  inner_query = hsiaR::hiBuildSQL('query$encounters')("to_number(substr(period_id(servdt, 'F'),1,4)) >= 2010")
  query = glue::glue("
  SELECT pracnum, servdt, count(clnt_label) as encounters
  FROM (\n{inner_query}\n)
  GROUP BY pracnum, servdt
  ORDER BY 1,2
  ")
  hiQuery(query, con=hiConnect(), run_query = F)
}

df3=pull_encounters(NULL, NULL)
