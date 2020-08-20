# That’s a token: a bundle containing the raw lexeme along with
# the other things the scanner learned about it.
Token = R6::R6Class("Token",
  public = list(
    type = NULL,
    lexeme = NULL,
    literal = NULL,
    initialize = function(type = NULL, lexeme = NULL, literal = NULL) {
      self$type = type
      self$lexeme = lexeme
      self$literal = literal
    },
    to_string = function() {
      paste(self$type, self$lexeme, self$literal)
    }
  )
)
