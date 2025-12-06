# NOTES TO SELF:

# - I want FTE, earnings, etc., by month (or week or day?) rather than year
# - can we find out which FPs are on LFP and which aren't?
# - do FPs switch out of hospitalism after LFP?


pacman::p_load(tidyverse, echarts4r, bslib, shinyWidgets, hsiaR, zoo)

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

pull_msp = function() {
  con = hiConnect()
  query = sql("
  SELECT pracnum, servdt, count(clnt_label) as encounters
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

clean_msp = function(msp_raw) {
  df = msp_raw |>
    mutate(encounters = as.integer(encounters)) |>
    mutate(servdt = as.Date(servdt)) |>
    mutate(yearmon = as.yearmon(servdt)) |>
    mutate(fiscal = hsiaR::as_fiscal(servdt, example_format = "2020/2021"))

  p = df |>
    group_by(yearmon) |>
    summarise(encounters = sum(encounters)) |>
    ungroup() |>
    mutate(z = (encounters - mean(encounters)) / sd(encounters)) # z-score

  # We can see that the final month is incomplete so we'll dump it
  #ggplot(p, aes(yearmon, z)) + geom_line()
  #ggplot(p, aes(x=z)) + geom_histogram()

  r = p |>
    filter(abs(z) < 3) |>
    select(yearmon)

  # check for no missing dates
  stopifnot(diff(r$yearmon) |> round(digits = 12) |> unique() |> length() == 1)

  inner_join(df, r) |>
    select(pracnum, servdt, yearmon, fiscal, encounters)
}


pull_vt4 = function() {
  con = hiConnect()
  hiQuery("select * from msea_team_lvl2.vt4", con=con)
}

clean_vt4 = function(vt4_raw) {
  # maybe make these guys factors cuz that's what they are....
  vt4 = vt4_raw |>
    mutate(across(prac_age, as.integer)) |>
    mutate(across(c(pracnum, ha_cd, hsda_cd, lha_cd, chsa_cd), fct_inseq)) |>
    mutate(across(c(funcspec), ~fct_inseq(as.character(.))))

  # FOR NOW!!!!!
  bind_rows(
    vt4,

    vt4 |>
      filter(fiscal == max(vt4$fiscal)) |>
      mutate(fiscal = "2024/2025") |>
      mutate(prac_age = prac_age + 1)
    ,

    vt4 |>
      filter(fiscal == max(vt4$fiscal)) |>
      mutate(fiscal = "2025/2026") |>
      mutate(prac_age = prac_age + 2)
  )
}

get_fps = function(msp, vt4, policy_dates) {
  fp_specs = policy_dates |>
    filter(anything_else == "Family medicine") |>
    pull(specs) |>
    first()

  vt4_fps = vt4 |>
    filter(funcspec %in% fp_specs) |>
    select(pracnum, fiscal, funcspec, ha_cd, prac_age, prac_gender, all_source_pd)

  inner_join(msp, vt4_fps, by=join_by(pracnum, fiscal)) |>
    mutate(is_treated = between(servdt, policy_dates$start_date, policy_dates$end_date))
}

get_fp_per_prac = function(fp) fp |>
  group_by(yearmon, is_treated, pracnum) |>
  summarise(
    all_source_pd = sum(all_source_pd),
    encounters = sum(encounters)
  )

LFP_plot = function(df=fp, lfp=LFP, group_vars = c("yearmon", "is_treated"), value = "encounters", .f = sum, include_geom_smooth = T) {

  group_vars = syms(group_vars)
  x = group_vars[[1]]
  value = sym(value)

  g = df |>
    group_by(!!!group_vars) |>
    summarise(!!value := .f(!!value)) |>
    ungroup() |>
    ggplot() +
    aes(x=!!x, y=!!value, color=is_treated) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = as.yearmon(lfp$start_date), color='red', linetype = 'dashed') +
    ggthemes::theme_clean() +
    scale_color_viridis_d() +
    theme(legend.position = 'none') +
    labs(x=NULL, y=NULL)

  if (include_geom_smooth) g + geom_smooth(method = 'lm', se = F, linetype = 'dashed', linewidth = .5) else g
}

create_fp_per_prac_density_pd_plot = function(fp_per_prac) ggplot(fp_per_prac) +
  aes(x=all_source_pd) +
  geom_density(aes(color=is_treated)) +
  scale_colour_viridis_d() +
  theme(legend.position = 'bottom') +
  scale_x_continuous(labels = scales::label_dollar(scale = 1/10^6, suffix = "M")) +
  ggthemes::theme_clean()

create_fp_per_prac_box_pd_plot = function(fp_per_prac) ggplot(fp_per_prac) +
  aes(y=all_source_pd) +
  geom_boxplot(aes(color=is_treated)) +
  scale_colour_viridis_d() +
  theme(legend.position = 'bottom') +
  scale_x_continuous(labels = scales::label_dollar(scale = 1/10^6, suffix = "M")) +
  ggthemes::theme_clean()

create_fp_per_prac_density_enc_plot = function(fp_per_prac) ggplot(fp_per_prac) +
  aes(x=encounters) +
  geom_density(aes(color=is_treated)) +
  scale_colour_viridis_d() +
  theme(legend.position = 'bottom') +
  scale_x_continuous() +
  ggthemes::theme_clean()

create_fp_per_prac_box_enc_plot = function(fp_per_prac) ggplot(fp_per_prac) +
  aes(y=encounters) +
  geom_boxplot(aes(color=is_treated)) +
  scale_colour_viridis_d() +
  theme(legend.position = 'bottom') +
  scale_x_continuous() +
  ggthemes::theme_clean()

create_fp_per_prac_corr_enc_plot = function(fp_per_prac) ggplot(fp_per_prac) +
  aes(x=encounters, y=all_source_pd, color=is_treated) +
  geom_point(alpha = .3) +
  scale_color_viridis_d() +
  theme(legend.position = 'bottom') +
  ggthemes::theme_clean()

create_ts_enc_plot = function(fp, LFP) {
  LFP_plot(fp, LFP) +
    scale_y_continuous(labels = function(x) x/10^6) +
    labs(y='MSP encounters (M)')
}

create_ts_pracs_plot = function(fp, LFP) {
  LFP_plot(fp, LFP, value = "pracnum", .f = n_distinct) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(y="# distinct FPs")
}

create_ts_enc_per_prac_plot = function(fp, LFP) {
  fp |>
    group_by(yearmon, is_treated, pracnum) |>
    summarise(encounters = sum(encounters)) |>
    LFP_plot(lfp = LFP, .f=mean) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(y="Mean encounters per month")
}

create_ts_pd_plot = function(fp, LFP) {
  LFP_plot(fp, LFP, value='all_source_pd') +
    scale_y_continuous(labels = scales::label_dollar(scale = 1/10^9, suffix = "B", accuracy = 1)) +
    labs(y="All source paid")
}

create_ts_pd_per_prac_plot = function(fp, LFP) {
  fp |>
    group_by(yearmon, is_treated, pracnum) |>
    summarise(all_source_pd = sum(all_source_pd)) |>
    LFP_plot(lfp=LFP, value='all_source_pd', .f=mean) +
    scale_y_continuous(labels = scales::label_dollar())
}
