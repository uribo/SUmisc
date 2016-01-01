#' @title Retest user's example codes
#' @name retest
#' @param url test urls.
#' @param rank execute code.
#' @export retest
## ----  --------------------------------------------------------------------
retest <- function(url = NULL, rank = 1, lang = "R")
{
  site <- xml2::url_parse(url)$server

  if (site == "rpubs.com")
  {
    url = paste0('http:', rvest::html_attr(rvest::html_node(xml2::read_html(x = url), css = 'iframe'), name = 'src'))
    path = 'pre.r code'
  } else (site != "rpubs.com")
  {
    if (site == "qiita.com")
      {
        path = 'div .code-frame .highlight pre'
    } else if (site == "github.com")
    {
      if (lang == "R")
      {
        path = 'div .highlight.highlight-R'
      } else {
        path = 'pre code'
      }
    } else if (site == "stackoverflow.com")
    {
      path = 'pre code'
    }
  }

  cat(rvest::html_text(
    rvest::html_nodes(xml2::read_html(x = url),
                      css = path)[rank]),
    file = tempfile(pattern = paste(rank, "retest", sep = "_"), fileext = ".r"))

  tmp.file.path <<- paste(tempdir(), grep(pattern = paste(rank, "retest", sep = "_"), ".r$",
                                         x = list.files(tempdir()),
                                         value = TRUE), sep = "/")
  tryCatch(
    {
      source(file = tmp.file.path, echo = TRUE)
    }, error = function()
    {
      file.remove(tmp.file.path)
    },
    finally = file.remove(tmp.file.path)
  )
}
# url <- "http://rpubs.com/uri-sy/jp_post_preparation"
# rank <- 1
# retest(url, rank)
# url <- "http://stackoverflow.com/questions/2061897/parse-json-with-r"
# url <- "https://github.com/hadley/dplyr/issues/1335#issuecomment-134390497"
# retest(url, 1, lang = "R")
# url <- "http://qiita.com/uri/items/98b20d928a9d8c0645d2"

retest_qt <- function(url, rank)
{
  # 1. qiita.com でのコードまでのCSSパス
  path <- 'div .code-frame .highlight pre'

  # 2. コード部分を指定して.Rファイルを一時フォルダ内に作る
  cat(rvest::html_text(
    rvest::html_nodes(xml2::read_html(x = url),
               css = path)[rank]),
    file = tempfile(pattern = paste(rank, "retest", sep = "_"), fileext = ".r"))

  # 3. 2.で作成した一時ファイルまでのパス
  tmp.file.path <- paste(tempdir(), grep(pattern = paste(rank, "retest", sep = "_"), ".r$",
                                    x = list.files(tempdir()),
                                    value = TRUE), sep = "/")
  # 4. Rファイルをsourceとして実行。実行後は削除する
    tryCatch(
    {
      source(file = tmp.file.path, echo = TRUE)
    }, error = function()
      {
      file.remove(tmp.file.path)
      },
    finally = file.remove(tmp.file.path)
    )
}

retest_gh <- function(url, rank, lang = c("R", "plain"), execute = TRUE)
{
  path = 'div .highlight.highlight-R'

  cat(rvest::html_text(
      rvest::html_nodes(xml2::read_html(x = url),
                 css = path)[rank]),
      file = tempfile(pattern = paste(rank, "retest", sep = "_"), fileext = ".r"))
  tmp.file.path <- paste(tempdir(), grep(pattern = paste(rank, "retest", sep = "_"), ".r$",
                                         x = list.files(tempdir()),
                                         value = TRUE), sep = "/")
  tryCatch(
    {
      source(file = tmp.file.path, echo = TRUE)
    }, error = function()
    {
      file.remove(tmp.file.path)
    },
    finally = file.remove(tmp.file.path)
  )
}

retest_so <- function(url, rank)
{
  path = 'pre code'
  cat(rvest::html_text(
    rvest::html_nodes(xml2::read_html(x = url), css = path)[rank]),
    file = tempfile(pattern = paste(rank, "retest", sep = "_"), fileext = ".r"))
  tmp.file.path <- paste(tempdir(), grep(pattern = paste(rank, "retest", sep = "_"), ".r$",
                                         x = list.files(tempdir()),
                                         value = TRUE), sep = "/")
  tryCatch(
    {
      source(file = tmp.file.path, echo = TRUE)
    }, error = function()
    {
      file.remove(tmp.file.path)
    },
    finally = file.remove(tmp.file.path)
  )
}
