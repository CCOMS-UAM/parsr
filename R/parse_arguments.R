#' Title
#'
#' @param arg
#' @param default
#'
#' @return
#' @import assertive.extra::assert_is_a_bool
#' @export
#'
#' @examples
parse_bool <- function(arg, default = "warning") {

  ## TODO: severities
  assertive.extra::assert_is_a_bool(arg, severity = default)
  as.logical(arg[1])
}

#' Title
#'
#' @param arg
#' @param default
#'
#' @return
#' @import assertive.extra::assert_is_a_number
#' @export
#'
#' @examples
parse_scalar <- function(arg, default = "warning") {

  ## TODO: severities
  assertive.extra::assert_is_a_number(arg, severity = default)
  as.double(arg[1])
}

#' Title
#'
#' @param arg
#' @param default
#'
#' @return
#' @import assertive.extra::assert_is_a_whole_number
#' @export
#'
#' @examples
parse_whole_number <- function(arg, default = "warning") {

  ## TODO: severities
  assertive.extra::assert_is_a_whole_number(arg, severity = default)
  as.integer(arg[1])
}

#' Title
#'
#' @param arg
#' @param default
#'
#' @return
#' @import assertive.extra::assert_is_a_natural_number
#'         assertive.numbers::is_negative
#' @export
#'
#' @examples
parse_natural_number <- function(arg, default = "warning") {

  ## TODO: severities
  assertive.extra::assert_is_a_natural_number(arg, severity = default)
  arg <- as.integer(arg[1])

  ## TODO: error message
  if (assertive.numbers::is_negative(arg)) {

    stop ("Must be a natural (positive integer) number")
  }

  arg
}

#' Title
#'
#' @param arg
#' @param default
#' @param ...
#'
#' @return
#' @import assertive.extra::assert_is_a_string
#' @export
#'
#' @examples
parse_string <- function(arg, default = "warning", ...) {

  ## TODO: severities
  assertive.extra::assert_is_a_string(arg, severity = default)
  as.character(arg[1])
}

#' Title
#'
#' @param arg
#' @param default
#' @param ...
#'
#' @return
#' @import assertive.types::assert_is_character
#' @export
#'
#' @examples
parse_char <- function(arg, default = "warning", ...) {

  ## TODO: severities
  assertive.types::assert_is_character(arg, severity = default)
  as.character(arg)
}

#' Title
#'
#' @param arg
#' @param default
#' @param ...
#'
#' @return
#' @import assertive.extra::assert_class_is_one_of
#'         assertive.extra::class_is_one_of
#'         assertive.types::is_list
#'         assertive.strings::assert_all_are_matching_fixed
#' @export
#'
#' @examples
parse_char_or_symbols <- function(arg, default = "stop", ...) {

  ## Constants: ----
  VALID_CLASSES <- c("character", "list", "name")
  SYMBOL_TYPE   <- "symbol"


  ## Main: ----

  assertive.extra::assert_class_is_one_of(
    arg,
    VALID_CLASSES,
    severity = default
  )

  if (assertive.types::is_list(arg)) {

    assertive.strings::assert_all_are_matching_fixed(
      # map_chr(arg, typeof),
      sapply(arg, typeof), # TODO: Check that works properly
      SYMBOL_TYPE,
      severity = default
    )
  }

  if (!assertive.extra::class_is_one_of(arg, VALID_CLASSES)) {

    arg <- as.character(arg)
  }

  arg
}

#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
vars_as_symbols <- function(vars) {

  ## Argument checking and formatting: ----
  parse_char_or_symbols(vars)

  ## Main: ----
  if (length(vars) > 1) vars %>% syms() else vars %>% sym()
}

#' Title
#'
#' @param arg
#' @param dataset
#' @param default
#' @param sym
#'
#' @return
#' @import assertive.types::assert_is_data.frame rlang::as_name
#'         assertive.sets::assert_is_subset
#' @export
#'
#' @examples
parse_varnames <- function(arg,
                           dataset,
                           default = "stop",
                           sym     = c("asis", "no", "yes")) {
  ## Argument checking and formatting: ----

  assertive.types::assert_is_data.frame(dataset)
  parse_char_or_symbols(arg)

  default <- parse_string(default)
  sym     <- match.arg(sym)


  ## Main: ----

  arg_check <- if (is.symbol(arg)) rlang::as_name(arg)
               # else                map_chr(arg, rlang::as_name)
               else                sapply(arg, rlang::as_name) # TODO: Check that works properly

  ## TODO: severities
  assertive.sets::assert_is_subset(
    arg_check,
    colnames(dataset),
    severity = default
  )

  if (sym == "asis") return(arg)
  if (sym == "no")   return(arg_check)
  if (sym == "yes")  return(vars_as_symbols(arg))
}
