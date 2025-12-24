# NOTES TO SELF:

# - I want FTE, earnings, etc., by month (or week or day?) rather than year
# - can we find out which FPs are on LFP and which aren't?
# - do FPs switch out of hospitalism after LFP?

pacman::p_load(tidyverse, echarts4r, bslib, shinyWidgets, hsiaR, zoo, glue)

create_policies = function() {
  tribble(~policy, ~start_date, ~end_date, ~specs, ~comments,
          "LFP", "2023-02-01", NA, as.integer(c(0, 50, 76, 77)), "Family Medicine"
  ) |>
    mutate(across(c(start_date, end_date), ymd)) |>
    mutate(end_date = replace_na(end_date, as.Date(Inf)))
}

pull_encounters = function(start_date=params$start_date, end_date=params$end_date) {
  inner_query = glue::glue("
  select
    {hiBuildSQL('select$encounters')}
  from
    {hsiaR::hiBuildSQL('from$msp_join')}
  where
    servdt between date '{start_date}' and date '{end_date}'
    and (
      {hsiaR::hiBuildSQL('where$encounters')}
    )
  ")
  query = glue::glue("
  SELECT pracnum, servdt, count(clnt_label) as encounters
  FROM (\n{inner_query}\n)
  GROUP BY pracnum, servdt
  ORDER BY 1,2
  ")
  hiQuery(query, con=hiConnect())
}

clean_encounters = function(encounters_raw) {
  stopifnot(sum(is.na(encounters_raw)) == 0)

  encounters_raw |>
    mutate(encounters = as.integer(encounters)) |>
    mutate(servdt = as.Date(servdt)) |>
    mutate(yearmon = zoo::as.yearmon(servdt)) |>
    mutate(fiscal = as_fiscal(servdt, example_format = "2020/2021"))
}

pull_vt4 = function(start_date=params$start_date, end_date=params$end_date) {
  start_date = hsiaR::as_fiscal(start_date, example_format = "2020/2021")
  end_date = hsiaR::as_fiscal(end_date, example_format = "2020/2021")
  hiQuery("select * from msea_team_lvl2.vt4 where fiscal between '{start_date}' and '{end_date}'", con=hiConnect())
}

clean_vt4 = function(vt4_raw) {
  # maybe make these guys factors cuz that's what they are....
  vt4 = vt4_raw |>
    mutate(across(prac_age, as.integer)) |>
    mutate(across(c(pracnum, ha_cd, hsda_cd, lha_cd, chsa_cd), fct_inseq)) |>
    mutate(across(c(funcspec), ~fct_inseq(as.character(.))))
}

get_encounters_fp = function(encounters, vt4, policies) {
  fp_specs = policies |>
    filter(policy == "LFP") |>
    pull(specs) |>
    first()

  vt4_fps = vt4 |>
    filter(funcspec %in% fp_specs) |>
    select(pracnum, fiscal, funcspec, ha_cd, prac_age, prac_gender)

  encounters_fp = inner_join(encounters, vt4_fps, by=join_by(pracnum, fiscal)) |>
    mutate(is_LFP = between(servdt, policies$start_date, policies$end_date))

  stopifnot(
    encounters_fp |>
      count(pracnum, servdt) |>
      pull(n) |>
      unique() == 1
  )
  encounters_fp
}



