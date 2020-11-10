Expr = R6::R6Class("Expr",
  public = list(
    initialize = function() {
      stop("I pretend to be an abstract class, you can't instantiate me.", call. = FALSE)
    },
    accept = function(){}
  )
)

Grouping = R6::R6Class("Grouping",
  inherit = Expr,
  public = list(
    expression = NULL,
    initialize = function(expression) {
      self$expression = expression
    },
    accept = function(visitor) {
      visitor$visitGroupingExpr(self)
    }
  )
)

Binary = R6::R6Class("Binary",
  inherit = Expr,
  public = list(
    left = NULL,
    operator = NULL,
    right = NULL,
    initialize = function(left, operator, right) {
      self$left = left
      self$operator = operator
      self$right = right
    },
    accept = function(visitor) {
      visitor$visitBinaryExpr(self)
    }
  )
)

Unary = R6::R6Class("Unary",
  inherit = Expr,
  public = list(
    operator = NULL,
    right = NULL,
    initialize = function(operator, right) {
      self$operator = operator
      self$right = right
    },
    accept = function(visitor) {
      visitor$visitUnaryExpr(self)
    }
  )
)

UnaryFun = R6::R6Class("UnaryFun",
  inherit = Expr,
  public = list(
    operator = NULL,
    arg = NULL,
    initialize = function(operator, arg) {
      self$operator = operator
      self$arg = arg
    },
    accept = function(visitor) {
      visitor$visitUnaryFunExpr(self)
    }
  )
)

LogFun = R6::R6Class("LogFun",
  inherit = Expr,
  public = list(
    base = NULL,
    arg = NULL,
    initialize = function(base, arg) {
      self$base = base
      self$arg = arg
    },
    accept = function(visitor) {
      visitor$visitLogFunExpr(self)
    }
  )
)

ExpFun = R6::R6Class("ExpFun",
  inherit = Literal,
  public = list(
    arg = 1,
    initialize = function(arg) {
      self$arg = arg
    },
    accept = function(visitor) {
      visitor$visitExpFunExpr(self)
    }
  )
)

Supsubscript = R6::R6Class("Supsubscript",
  inherit = Expr,
  public = list(
    left = NULL,
    sub = NULL,
    sup = NULL,
    initialize = function(left, sub, sup) {
      if (!(inherits(left, 'Variable') || is.null(sub))) {
        stop("Only variable names can have subscripts")
      }
      self$left = left
      self$sub = sub
      self$sup = sup
    },
    accept = function(visitor) {
      visitor$visitSupsubscriptExpr(self)
    }
  )
)

Variable = R6::R6Class("Variable",
  inherit = Expr,
  public = list(
    name = NULL,
    initialize = function(name) {
      self$name = name
    },
    accept = function(visitor) {
      visitor$visitVariableExpr(self)
    }
  )
)

Literal = R6::R6Class("Literal",
  inherit = Expr,
  public = list(
    value = NULL,
    initialize = function(value) {
      self$value = value
    },
    accept = function(visitor) {
      visitor$visitLiteralExpr(self)
    }
  )
)


