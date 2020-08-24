# Like the scanner, it consumes a sequence.
# However, now we're working at the level of entire tokens.
Parser = R6::R6Class("Parser",
  inherit = Reina,
  public = list(

    tokens = list(),
    current = 1,
    unary_fns = NULL,

    initialize = function(tokens) {
      self$tokens = tokens
      self$unary_fns = get_pkg_data('UNARY_FNS')
    },

    parse = function() {
      tryCatch({
        self$expression()
      },
      parse_error = function(cnd) {
        super$shared_env$had_error = TRUE
        super$error(self$peek(), cnd$message)
      },
      error = function(cnd) {
        super$shared_env$had_error = TRUE
        super$error(self$peek(), paste("Unexpected error when parsing:", cnd$message))
      })
    },

    is_at_end = function() {
      self$peek()$type == 'EOF'
    },

    advance = function() {
      if (!self$is_at_end()) {
        self$current = self$current + 1
        self$previous()
      }
    },

    # Returns the token we are about to consume
    peek = function() {
      self$tokens[[self$current]]
    },

    # Returns the last token consumed
    previous = function() {
      self$tokens[[self$current - 1]]
    },

    check = function(types) {
      # Supports multiple check (it uses `%in%`` and not `==`)
      if (self$is_at_end()) return(FALSE)
      self$peek()$type %in% types
    },

    match = function(types) {
      if (self$check(types)) {
        self$advance()
        return(TRUE)
      }
      return(FALSE)
    },

    # `consume()` checks the next token if of the expected type.
    # If TRUE, `advance()` is called it's all good man.
    # Otherwise, we've found an error and `error()` is called.
    consume = function(type, message) {
      if (self$check(type)) return(self$advance())
      self$error(message)
    },

    # Throws a custom error class that is appropiately handled.
    error = function(message) {
      stop_custom("parse_error", message)
    },

    # Here we start walking through the grammar.
    expression = function() {
      self$assignment()
    },

    assignment = function() {
      expr = self$addition()
      while(self$match('EQUAL')) {
        operator = self$previous()
        right = self$addition()
        expr = Binary$new(expr, operator, right)
      }
      return(expr)
    },

    addition = function() {
      expr = self$multiplication()
      while(self$match(c('MINUS', 'PLUS'))) {
        operator = self$previous()
        right = self$multiplication()
        expr = Binary$new(expr, operator, right)
      }
      return(expr)
    },

    multiplication = function() {
      while(self$match('FRAC')) {
        # Kind of a hack. I should use more elegant ways if possible.
        self$consume('LEFT_BRACE', "Expect '{' after FRAC.")
        expr1 = self$expression()
        self$consume('RIGHT_BRACE', "Expect '}' after expression")
        self$consume('LEFT_BRACE', "Expect '{' after FRAC.")
        expr2 = self$expression()
        self$consume('RIGHT_BRACE', "Expect '}' after expression")

        if (!inherits(expr1, c("Unary", "Literal", "Variable"))) {
          expr1 = Grouping$new(expr1)
        }
        if (!inherits(expr2, c("Unary", "Literal", "Variable"))) {
          expr2 = Grouping$new(expr2)
        }
        expr = Binary$new(expr1, Token$new('FRAC', '/', NULL), expr2)
        return(expr)
      }

      expr = self$unary()

      while(self$match(c('STAR', 'SLASH', 'UNDERSCORE', 'CARET'))) {
        if (self$previous()$type == 'CARET') {
          sub = NULL
          sup = self$unary()
          if (self$match('UNDERSCORE')) sub = self$multiplication()
          expr = tryCatch(
            Supsubscript$new(expr, sub = sub, sup = sup),
            error = function(cnd) {
            self$error(cnd$message)
          })
        } else if (self$previous()$type == 'UNDERSCORE') {
          sup = NULL
          sub = self$unary()
          if (self$match('CARET')) sup = self$multiplication()
          expr = tryCatch(
            Supsubscript$new(expr, sub = sub, sup = sup),
            error = function(cnd) {
              self$error(cnd$message)
            })
        } else {
          operator = self$previous()
          right = self$unary()
          expr = Binary$new(expr, operator, right)
        }
      }
      return(expr)
    },

    unary = function() {
      if (self$match(c('PLUS', 'MINUS', self$unary_fns))) {
        if (self$previous()$type %in% self$unary_fns) {
          return(self$unary_fn())
        } else {
          operator = self$previous()
          right = self$addition()
          return(Unary$new(operator, right))
        }
      }
      return(self$primary())
    },

    # This is a helper to represent unary functions that require
    # special care when translating from LaTeX to R.
    unary_fn_arg = function() {
      if (self$check(c('LEFT_BRACE', 'LEFT_PAREN'))) {
        if (self$peek()$type == 'LEFT_BRACE') {
          # This first `consume()` is not strictly necessary, `advance()`` would suffice.
          self$consume('LEFT_BRACE', paste0("Expect '{' after '", operator, "'"))
          arg = self$expression()
          self$consume('RIGHT_BRACE', "Expect '}' after expression")
        } else {
          self$consume('LEFT_PAREN', paste0("Expect '(' after '", operator, "'"))
          arg = self$expression()
          self$consume('RIGHT_PAREN', "Expect ')' after expression")
        }
      } else {
        self$error(
          paste0("Expect '{' or '(' after '", operator,
                 "' to avoid ambiguity in the function argument.")
        )
      }
      return(arg)
    },

    unary_fn = function() {
      operator = tolower(self$previous()$type)
      if (operator == 'log' && self$match('UNDERSCORE')) {
        base = self$primary()
        if (!inherits(base, 'Literal')) {
          self$error("Expect a number, and only a number, as the logarithm base.")
        }
        arg = self$unary_fn_arg()
        return(LogFun$new(base, arg))
      }
      arg = self$unary_fn_arg()
      return(UnaryFun$new(operator, arg))
    },

    primary = function() {
      if (self$match(c('NUMBER'))) {
        return(Literal$new(self$previous()$literal))
      }

      if (self$match('PI_NUMBER')) {
        return(Literal$new('pi'))
      }

      if (self$match('E_NUMBER')) {
        arg = Literal$new('1')
        if (self$match('CARET')) {
          if (self$match('LEFT_BRACE')) {
            arg = self$expression()
            self$consume('RIGHT_BRACE', "Expect '}' after expression")
          } else {
            arg = self$primary()
          }
        }
        return(ExpFun$new(arg))
      }

      if (self$match(c('IDENTIFIER', 'GREEK_IDENTIFIER'))) {
        return(Variable$new(self$previous()$lexeme))
      }

      if (self$match('LEFT_PAREN')) {
        expr = self$expression()
        self$consume('RIGHT_PAREN', "Expect ')' after expression.")
        return(Grouping$new(expr))
      }

      if (self$match('LEFT_BRACE')) {
        expr = self$expression()
        self$consume('RIGHT_BRACE', "Expect '}' after expression.")
        return(Grouping$new(expr))
      }
      self$error("Expect expression.")
    }

  )
)
