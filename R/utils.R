n_digits <- 3

var_label_fns <- function(x) {
  switch(x,
    "Tot_Prscrbrs" = "Total Prescribers",
    "Tot_Clms" = "Total Claims",
    "Tot_Drug_Cst" = "Total Drug Cost ($)",
    "Tot_Benes" = "Total Beneficiaries",
    "Brnd_Name" = "Brand Name",
    "Gnrc_Name" = "Generic Name",
    "Opioid_Drug_Flag" = "Opioid",
    "Opioid_LA_Drug_Flag" = "Long-acting Opioid",
    "state" = "State",
    "year" = "Year",
    x
  )
}

var_label_fns_v <- function(x) {
  map_chr(x, var_label_fns)
}

choice_masker <- function(x) {
  purrr::set_names(x, var_label_fns_v(x))
}

var_for_agg <-
  c(
    "Tot_Prscrbrs",
    "Tot_Clms",
    "Tot_Drug_Cst",
    "Tot_Benes"
  )

extra_filter_choices <-
  c(
    var_for_agg,
    "Opioid_Drug_Flag",
    "Opioid_LA_Drug_Flag"
  )

var_for_disp <-
  c("Brnd_Name", "Gnrc_Name", extra_filter_choices, "state", "year")

plotly_formula <- function(x) {
  as.formula(paste0("~`", var_label_fns(x), "`"))
}
