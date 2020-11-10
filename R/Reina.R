# About the shared environment:
# https://stackoverflow.com/questions/44961211/using-r6-how-do-i-find-classes-and-objects-that-inherit-from-some-superclass
Reina = R6::R6Class("Reina",
  public = list(
    shared_env = new.env(),
    initialize = function() {
      self$shared_env$had_error = FALSE
    },

    main = function(...) {
      args = list(...)
      if (length(args) == 1) {
        self$run_line(args[[1]])
      } else {
        self$run_prompt()
      }
    },

    run_line = function(line) {
      result = self$run(line)
      if (self$shared_env$had_error) {
        self$shared_env$had_error = FALSE
        return(invisible(NULL))
      }
      return(result)
    },

    run_prompt = function() {
      while(TRUE) {
        line = readline(prompt = "R> ")
        if (nchar(line) == 0) break
        print(self$run(line))
        self$shared_env$had_error = FALSE
      }
    },

    run = function(source) {
      scanner = Scanner$new(source)
      tokens = scanner$scan_tokens()
      if (!is.null(tokens)) {
        parser = Parser$new(tokens)
        expr = parser$parse()
        if (self$shared_env$had_error) return(invisible(NULL))
        RPrinter$new()$print(expr)
      }
    },

    error = function(token, message) {
      if (token$type == 'EOF') {
        self$report(message, "At end")
      } else {
        self$report(message, paste0("At '", token$lexeme, "'"))
      }
    },
    report = function(message, where) {
      self$shared_env$had_error = TRUE
      if (nchar(where) == 0) {
        err = message
      } else {
        err = paste0(where, ": ", message)
      }
      stop_custom("latex2r.error", err)
    }
  )
)
