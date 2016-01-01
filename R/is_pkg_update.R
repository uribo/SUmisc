#' @title Check package update.
#' @export is_pkg_update
#' @param pkg character. package name
is_pkg_update <- function(pkg = NULL, ...)
{
  # set variables
  inst.ver <- utils::packageVersion(pkg)
  author   <- utils::packageDescription(pkg)$GithubUsername
  repo     <- utils::packageDescription(pkg)$GithubRepo

  if (is.null(repo))
  {
    message('This package is not installed from GitHub repository.')
  } else {
    gh.url <- base::paste("https://raw.githubusercontent.com", author, repo, "master/DESCRIPTION", sep = "/")

    dev.ver <- base::readLines(con = gh.url) %>%
      base::grep(pattern = "Version: ", x = ., value = TRUE) %>%
      base::sub(pattern = "Version: ", replacement = "", .)
    if (magrittr::is_less_than(inst.ver, dev.ver))
    {
      message('Update available to version ', dev.ver,
              '.\n You can run follow comand `devtools::install_github("',
              author,
              '/',
              repo,
              '")`')
    } else {
      message("No update available.")
    }
  }
}
#
# pkg <- "dplyr"
# browseURL(gh.url)
# is_pkg_update("pipeR")
# if (magrittr::is_less_than(2.0, 2.1))
# {
#   message("Update available to version ")
# } else {
#   message("No update available.")
# }
