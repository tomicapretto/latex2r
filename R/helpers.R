# Helpers
is_digit = function(char) {
  grepl("[[:digit:]]", char)
}

is_alpha = function(char) {
  grepl("[a-zA-Z]", char)
}

is_alphanumeric = function(char) {
  is_alpha(char) || is_digit(char)
}

char_at = function(char, at) {
  substr(char, at, at)
}

stop_custom = function(.subclass, message, call = NULL, ...) {
  err = structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}


# https://github.com/KaTeX/KaTeX/blob/master/src/Parser.js
# Esta bueno la nota que hace. El parser mio no tiene modo texto aun.