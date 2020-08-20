# Like the scanner, it consumes a sequence, only now
# we're working at the level of entire tokens.
Parser = R6::R6Class("Parser",
  inherit = Reina,
  public = list(
    tokens = list(),
    current = 1,

    initialize = function(tokens) {
      self$tokens = tokens
    },

    parse = function() {
      tryCatch({
        self$expression()
      },
      parse_error = function(cnd) {
        super$shared_env$had_error = TRUE
        super$error(self$peek(), cnd$message)
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

    # Devuelve el token que estamos por consumir
    peek = function() {
      self$tokens[[self$current]]
    },

    # Devuelve el ultimo token consumido
    previous = function() {
      self$tokens[[self$current - 1]]
    },

    check = function(types) {
      # Supports multiple check
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

    # Este metodo se parece a `check()` en que chequea si el siguiente token
    # es del tipo esperado. Si es asi, lo consume y todo va bien.
    # Caso contrario, nos hemos encontramos con un error.
    # En ese caso, lo reportamos llamando a `ParseError()`.
    consume = function(type, message) {
      if (self$check(type)) return(self$advance())
      self$error(message)
    },

    error = function(message) {
      stop_custom("parse_error", message)
    },

    # Aca empezamos a caminar por la gramatica
    expression = function() {
      self$addition()
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
      if (self$match(c('PLUS', 'MINUS', 'SQRT', 'SIN', 'COS', 'TAN'))) {
        if (self$previous()$type %in% c('SQRT', 'SIN', 'COS', 'TAN')) {

          operator = tolower(self$previous()$type)
          self$consume('LEFT_BRACE', paste0("Expect '{' after '", operator, "'"))
          arg = self$expression()
          self$consume('RIGHT_BRACE', "Expect '}' after expression")
          return(TexUnary$new(operator, arg))

        } else {
          operator = self$previous()
          right = self$unary()
          return(Unary$new(operator, right))
        }
      }
      return(self$primary())
    },



    primary = function() {
      if (self$match(c('NUMBER'))) {
        return(Literal$new(self$previous()$literal))
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
