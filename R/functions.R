# NOTES TO SELF:

# - I want FTE, earnings, etc., by month (or week or day?) rather than year
# - can we find out which FPs are on LFP and which aren't?
# - do FPs switch out of hospitalism after LFP?


pacman::p_load(tidyverse, echarts4r, bslib, shinyWidgets, hsiaR)

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

prep_cihi_data_1 = function(data) {
  df = data |>
    filter(
      specialty == "All physicians",
      as.character(jurisdiction) == as.character(health_region),
      jurisdiction != 'Canada'
    )
}

make_ui_1 = function(data) {
  all_provinces = unique(data$jurisdiction)
  bslib::page_fluid(
    virtualSelectInput("prov", "Prov?", choices = all_provinces, multiple = T, selected = "B.C."),
    card(plotOutput("p1"), full_screen = T)
  )
}

# don't need this guy as a target
g_1_fn = function(data) {
  all_provinces = unique(data$jurisdiction)
  prov_colors = viridis::magma(length(all_provinces))
  names(prov_colors) = all_provinces
  prov_colors[["B.C."]] = HA_colors()[[1]]

  data |>
    ggplot() +
    aes(x=year, y=number_of_physicians, color=jurisdiction) +
    scale_y_continuous(labels = scales::label_comma()) +
    ggtitle("Physicians") +
    ggpad() +
    geom_line(aes(linetype = jurisdiction != "B.C.")) +
    geom_point(aes(size = if_else(jurisdiction == 'B.C.', 2, 1))) +
    scale_color_manual(values = prov_colors) +
    scale_size_identity() +
    guides(size = 'none', linetype = 'none')
}


plot_g_1 = function(data) {
  default_provinces = c("B.C.", "Alta.", "Ont.", "Man.")
  data |>
    filter(jurisdiction %in% default_provinces) |>
    g_1_fn()
}

plot_e_1 = function(data) {
  default_provinces = c("B.C.", "Alta.", "Ont.", "Man.")
  data |>
    filter(jurisdiction %in% default_provinces) |>
    mutate(year = fct(as.character(year))) |>
    group_by(jurisdiction) |>
    e_charts(x = year) |>
    e_line(number_of_physicians) |>
    e_title("# Physicians") |>
    e_tooltip('axis')
}


# prep_plots1 = function(data) {
#   default_provinces = c("B.C.", "Alta.", "Ont.", "Man.")

  # df = data |>
  #   filter(
  #     specialty == "All physicians",
  #     as.character(jurisdiction) == as.character(health_region),
  #     jurisdiction != 'Canada'
  #   )

  # all_provinces = unique(df$jurisdiction)

  # ui = bslib::page_fluid(
  #   virtualSelectInput("prov", "Prov?", choices = all_provinces, multiple = T, selected = "B.C."),
  #   card(plotOutput("p1"), full_screen = T)
  # )

  # g = function(data) data |>
  #   ggplot() +
  #   aes(x=year, y=number_of_physicians, color=jurisdiction) +
  #   scale_color_viridis_d() +
  #   scale_y_continuous(labels = scales::label_comma()) +
  #   ggtitle("Physicians") +
  #   ggpad() +
  #   geom_line() +
  #   geom_point()

  #g1 = g(filter(df, jurisdiction %in% default_provinces))

#   e1 = filter(df, jurisdiction %in% default_provinces) |>
#     mutate(year = fct(as.character(year))) |>
#     group_by(jurisdiction) |>
#     e_charts(x = year) |>
#     e_line(number_of_physicians) |>
#     e_title("# Physicians") |>
#     e_tooltip('axis')
#
#   list(df=df, ui=ui, g=g, g1=g1, e1=e1)
# }

# plot1 = function(data, type) {
#   if (type == 'g') {
#     ggplot() +
#       scale_color_viridis_d() +
#       scale_y_continuous(labels = scales::label_comma()) +
#       ggtitle("Physicians in B.C.") +
#       ggpad()
#   } else {
#     df |>
#       mutate(year = fct(as.character(year))) |>
#       e_charts(x = year) |>
#       e_line(number_of_physicians) |>
#       e_title("# Physicians", "BC") |>
#       e_tooltip('axis') |>
#       e_y_axis(number_of_physicians)
#   }
# }
#
# print_server_2 = function(data) {
#   print(data)
#   data |>
#     mutate(is_bc = health_region == 'B.C.') |>
#     mutate(is_bc_scale = case_when(is_bc ~ 1.5, T ~ 1)) |>
#     ggplot(aes(x=year, y=phys_pop_ratio, color=jurisdiction, linewidth=is_bc_scale)) +
#     geom_line() +
#     geom_point() +
#     scale_color_viridis_d() +
#     scale_y_continuous(labels = scales::label_comma()) +
#     guides(linewidth = 'none', size = 'none') +
#     ggtitle("Physicians per Pop - across Canada") +
#     theme(legend.position = 'bottom') +
#     ggpad()
# }
#
# print_ui5 = function(data) {
#   specialties = data |>
#     select(year, jurisdiction, health_region, specialty, number_of_physicians) |>
#     filter(health_region == 'B.C.') |>
#     #filter(str_detect(specialty, "^[_|All]", negate = T))
#     pull(specialty) |>
#     unique() |>
#     sort()
#
#   page_sidebar(
#     sidebar = sidebar(
#       virtualSelectInput("specs", "Spec?", choices = specialties, multiple = T, selected = "Family medicine")
#     ),
#     card(
#       plotOutput("p5"), full_screen = T
#     )
#   )
# }
#
# # print_server_5 = function(data) {
# #   data |>
# #     ggplot(aes(x=year, y=number_of_physicians, color=specialty)) +
# #     geom_point() +
# #     geom_line() +
# #     scale_y_continuous(labels = scales::label_comma())
# # }
#
#
#
# plot2 = function(data, type) {
#   good_provinces = c("Canada", "B.C.", "Alta.", "Ont.", "Man.")
#
#   df = data |>
#     filter(
#       specialty == "All physicians",
#       health_region %in% good_provinces
#     )
#
#   if (type == 'g') {
#     df |>
#       mutate(is_bc = health_region == 'B.C.') |>
#       mutate(is_bc_scale = case_when(is_bc ~ 1.5, T ~ 1)) |>
#       ggplot(aes(x=year, y=phys_pop_ratio, color=jurisdiction, linewidth=is_bc_scale)) +
#       geom_line() +
#       geom_point() +
#       scale_color_viridis_d() +
#       scale_y_continuous(labels = scales::label_comma()) +
#       guides(linewidth = 'none', size = 'none') +
#       ggtitle("Physicians per Pop - across Canada") +
#       theme(legend.position = 'bottom') +
#       ggpad()
#   } else {
#     df |>
#       mutate(year = fct(as.character(year))) |>
#       group_by(jurisdiction) |>
#       e_charts(x = year) |>
#       e_line(serie = phys_pop_ratio) |>
#       e_title("# Physicians", "Canada/Provinces") |>
#       e_tooltip('axis') |>
#       e_theme("westeros") |>
#       e_y_axis(phys_pop_ratio, formatter = e_axis_formatter("decimal"), margin=30)
#   }
# }
#
# plot3 = function(data, type) {
#   df = data |>
#     filter(
#       jurisdiction == 'B.C.',
#       health_region == 'B.C.',
#       specialty == "All physicians"
#     )
#
#   df2 = data |>
#     filter(
#       jurisdiction == 'B.C.',
#       health_region == 'B.C.',
#       specialty == "All physicians",
#       year %in% c(2015, 2020, 2024)
#     ) |>
#     select(year, starts_with("age_group")) |>
#     rename("00-30" = age_group_younger_than_30, "80-100" = age_group_80_and_older) |>
#     select(-age_group_unknown) |>
#     pivot_longer(cols = 2:last_col()) |>
#     mutate(name = str_extract(name, "\\d\\d-\\d\\d")) |>
#     rename(age_group = name)
#
#   df3 = df2 |>
#     group_by(year) |>
#     summarise(total_pop = sum(value)) |>
#     inner_join(df2) |>
#     mutate(prop_phys = value / total_pop) |>
#     mutate(year = fct(as.character(year)))
#
#   if (type == 'g') {
#     g1 = df |>
#       ggplot(aes(x=year, y=median_age)) +
#       geom_line() +
#       geom_point() +
#       scale_color_viridis_d() +
#       theme(legend.position = 'none') +
#       scale_y_continuous(labels = scales::label_comma()) +
#       ggtitle("Median Physician Age") +
#       ggpad()
#
#     g2 = df3 |>
#       ggplot(aes(x=age_group, y=prop_phys, color=year, group=year)) +
#       geom_line() +
#       geom_point() +
#       scale_color_viridis_d() +
#       scale_y_continuous(labels = scales::label_percent()) +
#       ggtitle("Relative Age Distribution by Year") +
#       theme(legend.position = 'bottom')
#
#     g1 + g2
#   } else {
#     e1 = df |>
#       mutate(year = fct(as.character(year))) |>
#       e_chart(x=year) |>
#       e_line(median_age) |>
#       e_y_axis(median_age) |>
#       e_title("Median Physician Age", "BC") |>
#       e_tooltip('axis')
#
#     e2 = df3 |>
#       group_by(year) |>
#       e_chart(x=age_group) |>
#       e_line(prop_phys) |>
#       e_title("Relative Age Distribution by Year") |>
#       e_tooltip('axis') |>
#       e_legend(right=0) |>
#       e_y_axis(formatter = e_axis_formatter('percent'))
#
#     browsable(
#       div(
#         style = "display: flex; gap: 20px;",
#         div(style = "flex: 1;", e1),
#         div(style = "flex: 1;", e2)
#       )
#     )
#   }
# }
#
# plot4 = function(data, type) {
#   df = data |>
#     filter(
#       jurisdiction == 'B.C.',
#       health_region == 'B.C.',
#       specialty == "All physicians"
#     ) |>
#     select(year, number_of_physicians, number_female, number_male) |>
#     mutate(across(3:4, ~./number_of_physicians)) |>
#     rename_with(~str_replace_all(., "number", "prop"), 3:4) |>
#     select(year, starts_with("prop")) |>
#     pivot_longer(2:3) |>
#     mutate(name = str_remove(name, "prop_"))
#
#   if (type == 'g') {
#     df |>
#       ggplot(aes(x=year, y=value, color=name)) +
#       geom_line() +
#       geom_point() +
#       scale_color_viridis_d() +
#       theme(legend.position = 'bottom') +
#       scale_y_continuous(labels = scales::label_percent()) +
#       labs(color = NULL) +
#       ggtitle("Gender Distribution") +
#       ggpad()
#   } else {
#     df |>
#       mutate(year = fct(as.character(year))) |>
#       group_by(name) |>
#       e_chart(x=year) |>
#       e_line(value) |>
#       e_title("Gender Distribution") |>
#       e_y_axis(formatter = e_axis_formatter('percent')) |>
#       e_tooltip('axis')
#   }
# }
#
# plot5 = function(data, type) {
#   df = data |>
#     select(year, jurisdiction, health_region, specialty, number_of_physicians) |>
#     filter(health_region == 'B.C.') |>
#     filter(str_detect(specialty, "^[_|All]", negate = T))
#   if (type == 'g') {
#     df |>
#       ggplot(aes(x=year, y=number_of_physicians, color=specialty)) +
#       geom_point() +
#       geom_line() +
#       scale_y_continuous(labels = scales::label_comma())
#
#   } else {
#     df |>
#       mutate(year = fct(as.character(year))) |>
#       group_by(specialty) |>
#       e_chart(x=year) |>
#       e_line(number_of_physicians) |>
#       e_title("number_of_physicians") |>
#       e_tooltip('axis') |>
#       e_legend_scroll()
#   }
#
# }
#
# make_milestones = function() {
#   tribble(~date, ~name, ~specialty, ~other,
#     "2023-02-01", "LFP", "Family medicine", NA_character_
# ) |>
#     mutate(date = ymd(date))
# }




encounters_fn = function() {
  con = hiConnect()
  query = sql("
  SELECT pracnum, servdt, count(clnt_label) as msp_encounters
  FROM (
    SELECT distinct
      clnt.mrg_clnt_anon_idnt_id as clnt_label,
      msp.pracnum,
      trunc(servdt) as servdt
    FROM
      ahip.AR_MSPCLM_CORE_CAN msp
      left join AHIP.CB_DTL_DM_CLNT_VW clnt ON clnt.label = msp.clnt_label
      left join MSEA_TEAM_LVL2.LAB_PROVIDER lab ON msp.payenum=LPAD(lab.payenum, 5, '0')
      left join ahip.cb_dtl_dm_srv_date_vw dt ON msp.servdt = dt.srv_date
      left join ahip.fitmds fitm on msp.fitm=fitm.fitm
    WHERE
      pracnum is not null and
      (clmtp in ('MM','MA','MB','MH','MN','MS','PM') or (servcd=13 and clmtp = 'PB')) AND
      msp.fitm not in (15501,15601) and
      (
        (PAYESTAT IN ('Y','F')  AND (ENCTR_CLM_MSPD IS NOT NULL OR (ENCTR_CLM_MSPD IS NULL AND PAIDSERV >0) ))
        OR (PAYESTAT NOT IN ('Y','F')  AND PAIDSERV >0)
      ) AND
      to_number(to_char(servdt,'YYYY'))>2009 and

      /* Remove lab services of lab providers*/
      not (
        lab.payenum is not null and ((servcd = 93 and payestat in ('C', 'H', 'L')) or (servcd = 94 and msp.fitm <> 90665) or (servcd = 94 and msp.fitm = 90665 and payestat in ('C', 'H', 'L')) or (servcd = 98 and msp.fitm in (00012,90000) and payestat in ('C', 'H', 'L')))
      ) and
      /* Remove registration and 15min codes*/
      NOT(
        fitmdesc like '%15 MIN%' or
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
      ) and
      /* Remove opioid management*/
      msp.fitm<>39 and
      NOT servcd in (12,17,9,19,29,49,71) and
      msp.fitm not in (98111,98112)
  )
    GROUP BY pracnum, servdt
    ORDER BY 1,2
  ")
  hiQuery(query, con=con)
}

vt4_fn = function() {
  con = hiConnect()
  hiQuery("select * from msea_team_lvl2.vt4", con=con)
}

get_fiscal_year <- function(date, fiscal_start_month = 4) {
  date <- as.Date(date)
  year <- lubridate::year(date)
  month <- lubridate::month(date)

  fiscal_year <- year - (month < fiscal_start_month)
  paste0(fiscal_year, "/", fiscal_year + 1)
}

make_fiscal_year_lookup_table = function(dates) {
  dates_uq = sort(unique(as.Date(dates)))
  tibble(date = dates_uq, fiscal = get_fiscal_year(date))
}

clean_encounters = function(encounters_raw) {
  x = make_fiscal_year_lookup_table(encounters_raw$servdt)
  encounters_raw |>
    mutate(msp_encounters = as.integer(msp_encounters)) |>
    mutate(servdt = as.Date(servdt)) |>
    inner_join(x, by=join_by(servdt == date))
}

clean_vt4 = function(vt4_raw) {
  vt4_raw |>
    mutate(across(c(funcspec, prac_age, ha_cd, hsda_cd, lha_cd, chsa_cd), as.integer))
}

get_encounters_fps = function(encounters, vt4, policy_dates) {
  fp_specs = policy_dates |>
    filter(anything_else == "Family medicine") |>
    pull(specs) |>
    first()

  vt4_fps = vt4 |>
    filter(funcspec %in% fp_specs) |>
    select(pracnum, fiscal, funcspec, ha_cd, prac_age, prac_gender, all_source_pd)

  inner_join(encounters, vt4_fps, by=join_by(pracnum, fiscal)) |>
    mutate(is_treated = between(servdt, policy_dates$start_date, policy_dates$end_date))
}

