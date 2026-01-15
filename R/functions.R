# NOTES TO SELF:

# - I want FTE, earnings, etc., by month (or week or day?) rather than year
# - can we find out which FPs are on LFP and which aren't?
# - do FPs switch out of hospitalism after LFP?

pacman::p_load(tidyverse, hsiaR, zoo, glue, targets)

create_policies = function() {
  tribble(~policy, ~start_date, ~end_date, ~specs, ~comments,
          "LFP", "2023-02-01", NA, as.integer(c(0, 50, 76, 77)), "Family Medicine"
  ) |>
    mutate(across(c(start_date, end_date), ymd)) |>
    mutate(end_date = replace_na(end_date, as.Date(Inf)))
}

pull_msp = function(months_back = 3, years = 5, min_start_date = as.Date("2020-09-01")) {

  # - months_back: how many months since today() do you want to go back? Default is 3 cuz that's what I believe it takes for the data to settle
  # - min_start_date: A Covid adjustment; will become irrelevant soon

  end = floor_date(today()  - months(months_back), unit = "month") - 1
  start = floor_date(end - years(years), unit = "month")

  # quick covid adjustment
  start = max(start, min_start_date)

  query = "select
    pracnum,
    servdt,
    clnt.mrg_clnt_anon_idnt_id as clnt_label,
    fitm.fitm,
    servloc,
    sum(expdamt) as expdamt
  from
    {hiBuildSQL$from$msp_encounters}
  where
    {hiBuildSQL$where$msp_encounters(start, end)}
  group by
    pracnum,
    servdt,
    clnt.mrg_clnt_anon_idnt_id,
    fitm.fitm,
    servloc
  "

  # query = dplyr::sql(glue::glue("
  # SELECT
  #   pracnum,
  #   servdt,
  #   fitm,
  #   servloc,
  #   count(clnt_label) as encounters,
  #   sum(expdamt) as expdamt
  # FROM ({inner_query})
  # GROUP BY pracnum, servdt, fitm, servloc
  # ORDER BY 1,2,3,4
  # "))

  hiQuery(query, run_query = T, con=hiConnect())
}

pull_fitms = function() {
  hiQuery("select * from ahip.fitmds", con=hiConnect())
}

pull_vt4 = function() {
  hiQuery("select * from msea_team_lvl2.vt4 where fiscal > '2019/202'", con=hiConnect())
}


clean_msp = function(msp_raw) {
  stopifnot(sum(is.na(msp_raw)) == 0)
  msp_raw |>
    mutate(encounters = as.integer(encounters)) |>
    mutate(servdt = as.Date(servdt)) |>
    mutate(yearmon = zoo::as.yearmon(servdt)) |>
    mutate(fiscal = as_fiscal(servdt, example_format = "2020/2021"))
}


clean_vt4 = function(vt4_raw) {
  # maybe make these guys factors cuz that's what they are....
  vt4 = vt4_raw |>
    mutate(across(prac_age, as.integer)) |>
    mutate(across(c(pracnum, ha_cd, hsda_cd, lha_cd, chsa_cd), fct_inseq)) |>
    mutate(across(c(funcspec), ~fct_inseq(as.character(.)))) |>
    arrange(pracnum, fiscal)
}

get_msp_fp = function(msp, vt4, fitms, policies) {

  # midpoint: the start of LFP; so we want roughly equal number of days before and after
  midpoint = filter(policies, policy == "LFP")$start_date

  end = max(msp$servdt)
  elapsed = end - midpoint
  start = floor_date(midpoint - elapsed, "month")
  start = max(start, min(msp$servdt))

  fp_specs = policies |>
    filter(policy == "LFP") |>
    pull(specs) |>
    first()

  vt4_fps = vt4 |>
    filter(funcspec %in% fp_specs) |>
    select(pracnum, fiscal, funcspec, ha_cd, prac_age, prac_gender)

  msp_fp = msp |>
    filter(between(servdt, start, end)) |>
    inner_join(vt4_fps, by=join_by(pracnum, fiscal)) |>
    left_join(fitms) |>
    mutate(is_LFP = between(servdt, policies$start_date, policies$end_date))

  stopifnot(
    msp_fp |>
      count(pracnum, servdt) |>
      pull(n) |>
      unique() == 1
  )
  msp_fp
}

pull_cihi = function(file = Sys.getenv("CIHI_PATH")) {
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

clean_cihi = function(cihi_raw, policies) {
  cihi_raw |>
  filter(
    jurisdiction == "B.C.",
    health_region == "B.C."
  ) |>
  select(year, specialty, number_of_physicians, number_of_physicians_who_returned_from_abroad, number_of_physicians_who_moved_abroad, net_migration_between_canadian_jurisdictions)
}

create_vt4_switches = function(vt4, policies) {
  vt4 |>
    mutate(is_LFP = between(as.integer(substr(fiscal, 1, 4)), year(policies$start_date), ifelse(is.infinite(policies$end_date), Inf, year(policies$end_date))), .after=1) |>
    mutate(is_fp = funcspec %in% policies$specs[[1]]) |>
    select(is_fp, is_LFP, pracnum, fiscal, funcspec) |>
    arrange(pracnum, fiscal) |>
    group_by(pracnum) |>
    mutate(lag_funcspec = lag(funcspec)) |>
    mutate(lag_is_fp = lag(is_fp)) |>
    mutate(change = case_when(
      is.na(lag_funcspec) ~ NA,
      funcspec == lag_funcspec ~ 'no_change',
      !is_fp & !lag_is_fp ~ 'no_change',
      is_fp & !lag_is_fp ~ 'new_fp',
      !is_fp & lag_is_fp ~ 'new_non_fp',
      is_fp & (funcspec != lag_funcspec) ~ 'fp_switch',
      T ~ 'other'
    )) |>
    ungroup()
}
