# Obtain the AST of an R expression (or something like that).
get_ast = function(arg) purrr::map_if(as.list(arg), is.call, get_ast)

# If exists, get the object. Otherwise, return NULL.
get2 = function(x) {
  if (exists(x)) return(get(x))
  return(NULL)
}

is_function2 = function(x) {
  greek_letters = c(
    "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
    "gamma", "varpi", "phi", "delta", "kappa", "rho",
    "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
    "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
    "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
    "Upsilon", "Omega", "Theta", "Pi", "Phi"
  )
  x_chr = as.character(x)
  if (tolower(x_chr) %in% letters) return(FALSE)
  if (tolower(x_chr) %in% greek_letters) return(FALSE)
  is.function(get2(x_chr))
}

# Heuristic to determine the arguments of a function.
get_args = function(expr, sort = TRUE) {
  expr = parse(text = expr)
  ast = unlist(get_ast(expr))
  result = sapply(
    ast,
    function(x) if (is.symbol(x) && !is_function2(x)) as.character(x) else NA
  )
  result = setdiff(result[!is.na(result)], "pi")
  if (sort) {
    return(sort(unique(result)))
  } else {
    return(unique(result))
  }
}

# Create a function of given args and given body (both are strings).
new_function = function(args, body, envir = parent.frame()) {
  f = function(){}
  f_args = rep(list(bquote()), length(args))
  names(f_args) = args

  formals(f) = f_args
  body(f) = parse(text = body)
  environment(f) = envir
  return(f)
}

#' Construct R function from LaTeX expression.
#'
#' @param latex_string string
#' @param envir environment
#'
#' @return function
#' @export
#'
#' @examples
#' x = seq(-2 * pi, 2 * pi, length.out = 500)
#' f = latex2fun("\\sin{x * a}")
#' f_x = f(x = x, a = 2)
#' plot(x, f_x, type = "l")
latex2fun = function(latex_string, envir = parent.frame()) {
  fun_body = latex2r(latex_string)
  if (grepl("=", fun_body)) {
    stop_custom("latex2r.error", "Expression contains assignment.")
  }
  fun_args = get_args(fun_body)
  new_function(fun_args, fun_body, envir)
}
