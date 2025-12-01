create_policy_dates = function() {
  tribble(~policy, ~start_date, ~end_date, ~specs, ~anything_else,
    "LFP", "2023-02-01", NA, as.integer(c(0, 50, 76)), "Family medicine"
  ) |>
    mutate(across(c(start_date, end_date), ymd)) |>
    mutate(end_date = replace_na(end_date, as.Date(Inf)))
}
