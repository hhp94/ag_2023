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
    "year" = "Year"
  )
}

var_label_fns_v <- function(x) {
  map_chr(x, var_label_fns)
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
    "Tot_Prscrbrs",
    "Tot_Clms",
    "Tot_Drug_Cst",
    "Tot_Benes",
    "Opioid_Drug_Flag",
    "Opioid_LA_Drug_Flag"
  )


var_for_display <-
  c("Brnd_Name", "Gnrc_Name", extra_filter_choices, "state", "year")