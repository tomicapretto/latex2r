AstPrinter = R6::R6Class(
  classname = "AstPrinter",
  public = list(
    initialize = function() {},

    print = function(expr) {
      expr$accept(self)
    },

    parenthesize = function(name, ...) {
      exprs = list(...)
      string = paste0("(", name)
      for (expr in exprs) {
        string = paste(string, expr$accept(self))
      }
      string = paste0(string, ")")
      string
    },

    visitUnaryExpr = function(expr) {
      self$parenthesize(expr$operator$lexeme, expr$right)
    },

    visitBinaryExpr = function(expr) {
      self$parenthesize(expr$operator$lexeme, expr$left, expr$right)
    },

    visitGroupingExpr = function(expr) {
      self$parenthesize("group", expr$expression)
    },

    visitLiteralExpr = function(expr) {
      expr$value
    },

    visitVariableExpr = function(expr) {
      expr$name
    }
  )
)

RPrinter = R6::R6Class(
  classname = "RPrinter",
  public = list(
    initialize = function() {},

    print = function(expr) {
      expr$accept(self)
    },

    format = function(mode = "unary", ...) {
      exprs = list(...)
      string = ""
      for (expr in exprs) {
        string = paste0(string, switch(mode,
          "supsupbscript" = self$print_subsubscript(expr),
          "unary" = paste(expr$operator$lexeme, expr$right$accept(self)),
          "binary" = paste(expr$left$accept(self), expr$operator$lexeme, expr$right$accept(self)),
          "group" = paste0("(", expr$expression$accept(self), ")")
        ))
      }
      return(string)
    },

    visitBinaryExpr = function(expr) {
      self$format(mode = "binary", expr)
    },

    visitUnaryExpr = function(expr) {
      self$format(mode = "unary", expr)
    },

    visitUnaryFunExpr = function(expr) {
      paste0(expr$operator, "(", expr$arg$accept(self), ")")
    },

    visitLogFunExpr = function(expr) {
      paste0("log(", expr$arg$accept(self), ", base = ", expr$base$accept(self), ")")
    },

    visitExpFunExpr = function(expr) {
      paste0("exp(", expr$arg$accept(self), ")")
    },

    visitSupsubscriptExpr = function(expr) {
      if (is.null(expr$sup)) {
        output = paste0(expr$left$accept(self), "_", expr$sub$accept(self))
        return(output)
      }
      if (is.null(expr$sub)) {
        output = paste0(expr$left$accept(self), "^", expr$sup$accept(self))
        return(output)
      }
      paste0(
        expr$left$accept(self), "_", expr$sub$accept(self), "^", expr$sup$accept(self)
      )
    },

    visitGroupingExpr = function(expr) {
      self$format(mode = "group", expr)
    },

    visitLiteralExpr = function(expr) {
      expr$value
    },

    visitVariableExpr = function(expr) {
      expr$name
    }
  )
)


