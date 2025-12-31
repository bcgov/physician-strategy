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

pull_encounters = function(midpoint = filter(targets::tar_read(policies), policy == "LFP")$start_date) {

  end = floor_date(today(), unit = "month") - 1
  elapsed = end - midpoint
  start = floor_date(midpoint - elapsed, "month")
  # quick covid adjustment
  start = max(start, as.Date("2020-09-01"))

  inner_query = hiBuildSQL$query$msp_encounters(dates = glue::glue("servdt between date '{start}' and date '{end}'"))
  query = dplyr::sql(glue::glue("
  SELECT pracnum, servdt, count(clnt_label) as encounters
  FROM (\n{inner_query}\n)
  GROUP BY pracnum, servdt
  ORDER BY 1,2
  "))
  hiQuery(query, con=hiConnect())
}

pull_vt4 = function() {
  hiQuery("select * from msea_team_lvl2.vt4 where fiscal > '2019/202'", con=hiConnect())
}


clean_encounters = function(encounters_raw) {
  stopifnot(sum(is.na(encounters_raw)) == 0)

  encounters_raw |>
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
    health_region == "B.C.",
    specialty == "Family medicine"
  ) |>
  select(year, number_of_physicians_who_returned_from_abroad, number_of_physicians_who_moved_abroad, net_migration_between_canadian_jurisdictions) |>
  mutate(is_LFP = between(year, year(policies$start_date), ifelse(is.infinite(policies$end_date), Inf, year(policies$end_date))), .after=1)
}
