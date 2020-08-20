# source("Reina.R")

# r = Reina$new()
#
# r$run_line("x_1^2")
# r$run_line("x^2_1")
# r$run_line("x^{2 + 2}_1")
# r$run_line("x_1^{2 + 2}")
#
# r$run_line("2 * 4 + 1 * (5 - 2)")
# r$run_line("x_1 ^ 2 + x_2 ^ 2")
# r$run_line("\\frac{x^2 + 2 \\cdot x + 3}{y^2 - 1 \\cdot x + 3}")
# r$run_line("x\\ \\cdot\\ x")
# r$run_line("x\\ \\times\\ x")
# r$run_line("\\frac{5}{5 + 1 * 2 }")
# r$run_line("\\frac{\\alpha_1 + \\alpha_2}{(\\alpha_1 + \\alpha_2) ^ 2}")
# r$run_line("x^{\\alpha + \\beta}")
# r$run_line("\\sqrt{x+1}")
# r$run_line("\\cos{\\frac{x}{2}} + \\sin{x^2_2 - 1} + \\tan{\\pi}")
# r$run_line("\\tan{\\pi}")

# Los siguientes tiran errores esperados.
# r$run_line("\\frec")
# r$run_line("y^2 - \\cdot x + 3")


# Count your lines of R code
# files = list.files(path = "./", recursive = TRUE, full.names = TRUE)
# subset = stringr::str_subset(files, "[.][R]$")
# code_length = sum(sapply(subset, function(x) length(readLines(x))))
# cat("The project has", code_length, "rows of code.")

