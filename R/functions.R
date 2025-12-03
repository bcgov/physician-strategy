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

pull_encounters = function() {
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

clean_encounters = function(encounters_raw) {
  x = make_fiscal_year_lookup_table(encounters_raw$servdt)

  df = encounters_raw |>
    mutate(msp_encounters = as.integer(msp_encounters)) |>
    mutate(servdt = as.Date(servdt)) |>
    mutate(yearmon = as.yearmon(servdt)) |>
    inner_join(x, by=join_by(servdt == date))

  p = df |>
    group_by(yearmon) |>
    summarise(msp_encounters = sum(msp_encounters)) |>
    ungroup() |>
    mutate(z = (msp_encounters - mean(msp_encounters)) / sd(msp_encounters)) # z-score

  # We can see that the final month is incomplete so we'll dump it
  #ggplot(p, aes(yearmon, z)) + geom_line()
  #ggplot(p, aes(x=z)) + geom_histogram()

  r = p |>
    filter(abs(z) < 3) |>
    select(yearmon)

  # check for no missing dates
  stopifnot(diff(r$yearmon) |> round(digits = 12) |> unique() |> length() == 1)

  inner_join(df, r)
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

plot_LFP_encounters = function(df = encounters_LFP, group_vars = c("yearmon", "is_treated"), value = "msp_encounters", .f = sum, include_geom_smooth = T) {

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
    geom_vline(xintercept = as.yearmon(LFP$start_date), color='red', linetype = 'dashed') +
    ggthemes::theme_clean() +
    scale_color_viridis_d() +
    theme(legend.position = 'none') +
    labs(x='month')

  if (include_geom_smooth) g + geom_smooth(method = 'lm', se = F, linetype = 'dashed', linewidth = .5) else g
}
