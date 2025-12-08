create_policy_dates = function() {
  tribble(~policy, ~start_date, ~end_date, ~specs, ~anything_else,
    "LFP", "2023-02-01", NA, as.integer(c(0, 50, 76)), "Family medicine"
  ) |>
    mutate(across(c(start_date, end_date), ymd)) |>
    mutate(end_date = replace_na(end_date, as.Date(Inf)))
}


create_unpaired_tests = function(data) data |>
  select(is_treated, encounters, all_source_pd) |>
  pivot_longer(cols = c(encounters, all_source_pd), names_to = 'variable') |>
  nest(data = c(is_treated, value)) |>
  mutate(t_test = map(data, ~t.test(value ~ is_treated, data = .))) |>
  mutate(wilcox_test = map(data, ~wilcox.test(value ~ is_treated, data = .))) |>
  mutate(t_test = map(t_test, broom::tidy)) |>
  mutate(wilcox_test = map(wilcox_test, broom::tidy))
