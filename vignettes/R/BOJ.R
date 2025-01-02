if (getRversion() >= "2.15.1") utils::globalVariables(c("obs_value"))

# Standardise column names
.clean_names <- function(x) {
  x <- make.unique(tolower(trimws(gsub("[[:space:]]", "_", x))))

  return(x)
}

# Download a file from a given URL
.download_file <- function(file_url, ...) {
  # Save user options
  old_options <- options()

  # Restore user options on function exit
  on.exit(options(old_options))

  # Force minimum timeout of 300 for file download
  options(timeout = max(300, getOption("timeout")))

  file_path <- tryCatch({
    # Prepare temp file
    file_ext <- tools::file_ext(file_url)
    file_ext <- ifelse(file_ext == "", "", paste0(".", file_ext))
    tmp_file <- tempfile(fileext = file_ext)

    # Download data and store in temp file
    utils::download.file(file_url, tmp_file, mode = "wb")

    # Return path to temp tile
    file_path <- tmp_file

    file_path
  },
  error = function(x) {
    message(paste("Unable to download file:", file_url))
    message("The resource is unavailable or has changed.")
    message("To download large files, try increasing the download timeout:")
    message("options(timeout = 600)")
    message("Original error message:")
    message(x)
    return(NA)
  },
  warning = function(x) {
    message(paste("Unable to download file:", file_url))
    message("The resource is unavailable or has changed.")
    message("To download large files, try increasing the download timeout:")
    message("options(timeout = 600)")
    message("Original warning message:")
    message(x)
    return(NA)
  }
  )

  return(file_path)
}

# Extract the contents of a zip file
.unzip_file <- function(archive_path) {
  # Prepare temp dir
  tmp_dir <- tempdir()

  # Unpack zip file
  file_name <- utils::unzip(archive_path, list = TRUE)
  utils::unzip(archive_path, exdir = tmp_dir)

  # Get path(s) to csv file(s)
  file_path <- file.path(tmp_dir, file_name$Name)

  return(file_path)
}

#' Convert a BOJ data set to long format
#'
#' @param tbl Tibble. A tibble data frame containing a BOJ data set (usually
#' obtained via \code{get_boj(item_url, auto_pivot = FALSE)}).
#'
#' @return A tibble data frame in long format.
#' @export
#'
#' @examples
#' \donttest{
#' ds   <- get_boj_datasets()
#' sppi <- get_boj(ds$url[(ds$name == "sppi_q_en")], auto_pivot = FALSE)
#' sppi <- subset(sppi, code == "PRCS15_52S0000000_CQ")
#' sppi <- pivot_longer_boj(sppi)
#' }
pivot_longer_boj <- function(tbl) {
  excl_cols <- names(tbl)[is.element(names(tbl),
                                     c("code", "desc", "struc", "unit"))]
  tbl <- tidyr::pivot_longer(data = tbl, cols = -tidyselect::all_of(excl_cols),
                             names_to = "date", values_to = "obs_value")
  tbl <- dplyr::mutate(tbl, obs_value = as.numeric(obs_value))

  return(tbl)
}

# Parse a BOJ data set
.parse_boj <- function(file_path, item_url, auto_pivot) {
  # Get file name
  file_name <- tools::file_path_sans_ext(basename(item_url))

  # Read data into a list of tibble data frames
  tbl <- list()
  i   <- 0

  # One tibble data frame per file
  while (i < length(file_path)) {
    i <- i + 1

    # Read data into tibble data frame
    tbl[[i]] <- readr::read_csv(file_path[[i]], col_names = FALSE,
                                show_col_types = FALSE,
                                na = c("", "NA", "ND"))

    # Distinguish between wide and long data sets
    if (!is.element(file_name, c("fof", "co", "colease"))) {
      # Wide data (horizontal), dates in columns
      nms <- as.character(tbl[[i]][1, ])

      if (!is.element(file_name, c("bp_m_en", "regbp_q_en", "qiip_q_en",
                                   "iip_cy_en"))) {
        # Three column data set
        if (is.element(file_name, c("bis1-1_q_en", "bis1-2_q_en",
                                    "bis2-1_q_en", "bis2-2_q_en"))) {
          nms[1:3] <- c("code", "struc", "unit")
        } else {
          nms[1:3] <- c("code", "desc", "struc")
        }
      } else {
        # Four column data set
        nms[1:4] <- c("code", "desc", "struc", "unit")
      }

      # Set column names
      names(tbl[[i]]) <- .clean_names(nms)
      tbl[[i]]        <- tbl[[i]][-1, ]

      # Pivot data from wide to long format
      if (auto_pivot) {
        tbl[[i]]        <- pivot_longer_boj(tbl[[i]])
      }

    } else {
      # Long data (vertical), dates in rows
      names(tbl[[i]]) <- c("code", "freq", "date", "obs_value")
    }

    # Add name to list item
    names(tbl)[[i]] <- tools::file_path_sans_ext(basename(file_path))[[i]]

    # Check for successful parsing
    if (nrow(tbl[[i]]) == 0) {
      message(paste("Unable to parse file:", file_name))
    }
  }

  # If there is only one tibble data frame, return as single object
  if (length(tbl) < 2) {
    tbl <- tbl[[1]]
  }

  return(tbl)
}

#' Retrieve a list of available BOJ data sets
#'
#' @param base_url Character. URL of the BOJ's Time-Series Data portal flat
#' files page (optional).
#'
#' @return A tibble data frame.
#' @export
#'
#' @examples
#' \donttest{
#' ds <- get_boj_datasets()
#' }
get_boj_datasets <- function(
    base_url = "https://www.stat-search.boj.or.jp/info/dload_en.html") {
  tbl <- tryCatch({
    # Download webpage
    page  <- xml2::read_html(base_url)
    nodes <- rvest::html_nodes(page, xpath = "//a[contains(@href, 'zip')]")

    # Parse homepage: Get file descriptions, names, and URLs
    item_desc <- rvest::html_text(nodes)
    item_name <- tools::file_path_sans_ext(rvest::html_attr(nodes, "href"))
    item_url  <- xml2::url_absolute(rvest::html_attr(nodes, "href"), base_url)

    # Return tibble data frame
    tbl <- dplyr::tibble(desc = item_desc,
                         name = item_name,
                         url  = item_url)

    if (nrow(tbl) == 0) {
      message(paste("Unable to download and parse homepage:", base_url))
      message("The resource is unavailable or has changed.")
    }

    tbl
  },
  error = function(x) {
    message(paste("Unable to download and parse homepage:", base_url))
    message("The resource is unavailable or has changed.")
    message("Original error message:")
    message(x)
    return(NA)
  },
  warning = function(x) {
    message(paste("Unable to download and parse homepage:", base_url))
    message("The resource is unavailable or has changed.")
    message("Original warning message:")
    message(x)
    return(NA)
  }
  )

  return(tbl)
}

#' Download and parse a BOJ data set
#'
#' @param item_url Character. URL of the data set to be imported (usually
#' obtained via \code{get_boj_datasets()}).
#' @param auto_pivot Logical. Controls whether source data set is converted to
#' long format. Set this to \code{FALSE} to disable conversion (default: TRUE).
#' @param ... Arguments passed to \code{download.file()} (e.g.
#' \code{quiet = TRUE}).
#'
#' @return A tibble data frame, or a list of tibble data frames in cases where
#' the source zip file contains multiple csv files.
#' @export
#'
#' @examples
#' \donttest{
#' ds <- get_boj_datasets()
#' df <- get_boj(ds$url[(ds$name == "sppi_q_en")])
#' }
get_boj <- function(item_url, auto_pivot = TRUE, ...) {
  try(zip_file_path <- .download_file(item_url, ...), TRUE)
  try(csv_file_path <- .unzip_file(zip_file_path), TRUE)
  try(return(.parse_boj(csv_file_path, item_url, auto_pivot)), TRUE)
}
