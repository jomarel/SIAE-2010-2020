required_packages <- c(
  "dplyr", "tidyr", "readr", "purrr", "stringr", "tibble",
  "janitor", "fs", "here", "cli", "glue", "rlang", "jsonlite"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  } else {
    message("All required packages are already installed.")
  }
}

install_if_missing(required_packages)

invisible(lapply(required_packages, library, character.only = TRUE))
message("Package setup completed.")
