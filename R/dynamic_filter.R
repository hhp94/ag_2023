make_ui <- function(x, var, label_fns) {
  UseMethod("make_ui")
}

make_ui.numeric <- function(x, var, label_fns) {
  rng <- range(x, na.rm = TRUE)
  numericRangeInput(var, label_fns(var), min = rng[1], max = rng[2], value = rng)
}
 
make_ui.factor <- function(x, var, label_fns) {
  levs <- levels(x)
  awesomeCheckboxGroup(
    inputId = var,
    label = label_fns(var),
    choices = levs,
    selected = levs,
    inline = TRUE,
    status = "primary"
  )
}

filter_var <- function(x, val) {
  UseMethod("filter_var")
}

filter_var.numeric <- function(x, val) {
  !is.na(x) & x >= val[1] & x <= val[2]
}

filter_var.factor <- function(x, val) {
  x %in% val
}
