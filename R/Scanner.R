Scanner = R6::R6Class("Scanner",
  inherit = Reina,
  public = list(
    source = NULL,
    tokens = list(),

    start = 1,
    current = 1,

    greek_keywords = NULL,
    keywords = NULL,
    keywords_lexemes = NULL,

    initialize = function(source = NULL) {
      if (!is.null(source)) {
        self$source = source
        self$greek_keywords = get_pkg_data('GREEK_KEYWORDS')
        self$keywords = get_pkg_data('KEYWORDS')
        self$keywords_lexemes = get_pkg_data('KEYWORDS_LEXEMES')
      } else {
        self$error("scan_error", "Source can't be NULL.")
      }
    },

    is_at_end = function() {
      self$current >= nchar(self$source) + 1
    },

    error = function(message) {
      stop_custom("scan_error", message)
    },

    scan_tokens = function() {
      tryCatch({
        while (!self$is_at_end()) {
          self$start = self$current
          self$scan_token()
        }
        self$tokens = append(self$tokens, Token$new('EOF', '', ''))
        return(self$tokens)
      },
      scan_error = function(cnd) {
        super$shared_env$had_error = TRUE
        super$report(cnd$message, '')
        return(NULL)
      })
    },

    scan_token = function() {
      char = self$advance()
      switch(char,
        '(' = self$add_token('LEFT_PAREN'),
        ')' = self$add_token('RIGHT_PAREN'),
        '{' = self$add_token('LEFT_BRACE'),
        '}' = self$add_token('RIGHT_BRACE'),
        '+' = self$add_token('PLUS'),
        '-' = self$add_token('MINUS'),
        '/' = self$add_token('SLASH'),
        '*' = self$add_token('STAR'),
        "_" = self$add_token('UNDERSCORE'),
        "^" = self$add_token('CARET'),
        "e" = self$add_token('E_NUMBER'),
        "=" = self$add_token('EQUAL'),
        " " = NULL,

        # Comportamiento default
        if (is_digit(char)) {
          self$number()
        } else if (is_alpha(char)) {
          self$identifier()
        } else if (char == "\\") {
          self$latex()
        } else {
          self$error("Unexpected character.")
        }
      )
    },

    # Consume el siguiente caracter desde el input y lo devuelve
    advance = function() {
      self$current = self$current + 1
      return(char_at(self$source, self$current - 1))
    },

    peek = function() {
      if (self$is_at_end()) return('')
      char_at(self$source, self$current)
    },

    peek_next = function() {
      if (self$current + 1 >= nchar(self$source) + 1) return('')
      char_at(self$source, self$current + 1)
    },

    match = function(expected) {
      if (self$is_at_end()) return(FALSE)
      if (char_at(self$source, self$current) != expected) return(FALSE)
      self$current = self$current + 1
      return(TRUE)
    },

    number = function() {
      while(is_digit(self$peek())) self$advance()
      # Buscamos parte fraccional, si es que hay
      if (self$peek() == "." && is_digit(self$peek_next())) {
        # Consumimos el '.'
        self$advance()
        while(is_digit(self$peek())) self$advance()
      }
      self$add_token(
        'NUMBER',
        as.numeric(substr(self$source, self$start, self$current - 1))
      )
    },

    identifier = function() {
      self$add_token('IDENTIFIER')
    },

    latex = function() {
      if (self$peek() == ' ') {
        return(NULL)
      }
      while (is_alpha(self$peek())) {
        self$advance()
      }

      text = substr(self$source, self$start, self$current - 1)
      if (text %in% c("\\left", "\\right")) {
        self$latex_delimiters(text)
        return(NULL)
      }

      if (text %in% self$greek_keywords) {
        # Delete backslashes
        text = gsub("\\\\", "", text)
        if (text == "pi") {
          self$add_token_latex("PI_NUMBER", text)
          return(NULL)
        }
        self$add_token_latex("GREEK_IDENTIFIER", text)
        return(NULL)
      }

      type = self$keywords[[text]]
      if (!is.null(type)) {
        lexeme = self$keywords_lexemes[[type]]
        self$add_token_latex(type, lexeme)
        return(NULL)
      }
      self$error("Unrecognized latex character.")
    },

    latex_delimiters = function(text) {
      if (text == '\\left') {
        if (self$peek() == "\\") self$advance()
        if (self$peek() == '(') {
          self$advance()
          self$add_token('LEFT_PAREN')
        } else if (self$peek() == '{') {
          self$advance()
          self$add_token('LEFT_BRACE')
        } else {
          self$error("Unrecognized latex character.")
        }
      }

      if (text == '\\right') {
        if (self$peek() == "\\") self$advance()
        if (self$peek() == ')') {
          self$advance()
          self$add_token('RIGHT_PAREN')
        } else if (self$peek() == '}') {
          self$advance()
          self$add_token('RIGHT_BRACE')
        } else {
          self$error("Unrecognized latex character.")
        }
      }
    },

    # Only literals have "literal != NULL"
    add_token = function(type, literal = NULL) {
      text = substr(self$source, self$start, self$current - 1)
      self$tokens = append(self$tokens, Token$new(type, text, literal))
    },

    add_token_latex = function(type, lexeme) {
      self$tokens = append(self$tokens, Token$new(type, lexeme, literal = NULL))
    }
  )
)

# sc = Scanner$new("\\alpha+\\beta")
# sc$scan_tokens()
#
#
# sc = Scanner$new("\\left(a\\ +\\ b\\right)")
# sc$scan_tokens()
#
#
# sc = Scanner$new("x_1^{n\\ +\\ 2}")
# sc$scan_tokens()
#
#
# sc = Scanner$new("2*x+3^y")
# sc$scan_tokens()
#
# sc = Scanner$new("\\sqrt{\\frac{3}{4}}")
# sc$scan_tokens()

# sc = Scanner$new("3/4")
# sc$scan_tokens()

# sc = Scanner$new("3.125/4.1115")
# sc$scan_tokens()
#
# sc = Scanner$new("3.5*2.2")
# sc$scan_tokens()

# sc = Scanner$new("\\cos(x + 1)")
# sc$scan_tokens()


