get_default <- function(func){
  x <- as.list(formals(func))
  purrr::keep(x, purrr::map_lgl(x, ~ !is.name(.x)))
}

overwrite_default <- function(func, args){
  default_args <- get_default(func)
  result <- purrr::map(names(default_args), ~ if(is.element(.x, args)){args[[.x]]}else{default_args[[.x]]})
  stats::setNames(result, names(default_args))
}
