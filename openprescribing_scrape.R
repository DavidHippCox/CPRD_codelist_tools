library(rvest)
library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# -----------------------------
# Settings
# -----------------------------

base_url <- "https://openprescribing.net"

parent_urls <- list(
  vmp  = "https://openprescribing.net/dmd/?q=0401&obj_types=vmp",
  vmpp = "https://openprescribing.net/dmd/?q=0401&obj_types=vmpp",
  amp  = "https://openprescribing.net/dmd/?q=0401&obj_types=amp",
  ampp = "https://openprescribing.net/dmd/?q=0401&obj_types=ampp"
)

output_files <- list(
  vmp  = "dmd_vmp_0401.csv",
  vmpp = "dmd_vmpp_0401.csv",
  amp  = "dmd_amp_0401.csv",
  ampp = "dmd_ampp_0401.csv"
)

output_dir <- "openprescribing_dmd_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

sleep_seconds <- 0.5
max_tries <- 3

errors <- list()

# -----------------------------
# Helper functions
# -----------------------------

add_error <- function(object_type, url, stage, message) {
  errors[[length(errors) + 1]] <<- tibble(
    object_type = object_type,
    url = url,
    stage = stage,
    error_message = message,
    timestamp = as.character(Sys.time())
  )
}

fetch_html <- function(url, max_tries = 3, sleep_seconds = 0.5) {
  last_error <- NULL

  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(
      {
        req <- request(url) |>
          req_user_agent(
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
          ) |>
          req_headers(
            Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            `Accept-Language` = "en-GB,en;q=0.9"
          ) |>
          req_timeout(60)

        resp <- req_perform(req)

        if (resp_status(resp) >= 400) {
          stop("HTTP ", resp_status(resp), " returned for ", url)
        }

        read_html(resp_body_string(resp))
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    Sys.sleep(sleep_seconds * attempt)
  }

  stop(last_error)
}

clean_cell_text <- function(node) {
  txt <- html_text2(node)

  pieces <- str_split(txt, "\\r?\\n")[[1]] |>
    str_squish()

  pieces <- pieces[nzchar(pieces)]

  paste(unique(pieces), collapse = "; ")
}

make_unique_names <- function(x) {
  x <- str_squish(x)
  x[x == "" | is.na(x)] <- "unnamed_field"
  make.unique(x, sep = "_")
}

extract_dmd_id <- function(url) {
  clean_url <- str_remove(url, "[?#].*$")
  str_extract(clean_url, "[0-9]+(?=/?$)")
}

get_detail_links <- function(parent_url, object_type) {
  page <- fetch_html(parent_url, max_tries = max_tries, sleep_seconds = sleep_seconds)

  a_nodes <- html_elements(page, "a[href]")

  hrefs <- html_attr(a_nodes, "href")
  link_text <- html_text2(a_nodes)

  full_urls <- xml2::url_absolute(hrefs, parent_url)
  clean_urls <- str_remove(full_urls, "[?#].*$")

  detail_pattern <- paste0("/dmd/", object_type, "/[0-9]+/?$")

  tibble(
    object_type = object_type,
    result_name = link_text,
    detail_url = clean_urls
  ) |>
    filter(str_detect(detail_url, detail_pattern)) |>
    distinct(detail_url, .keep_all = TRUE)
}

parse_detail_page <- function(detail_url, parent_url, object_type, result_name) {
  page <- fetch_html(detail_url, max_tries = max_tries, sleep_seconds = sleep_seconds)

  heading <- page |>
    html_element("h1, h2") |>
    html_text2()

  if (length(heading) == 0 || is.na(heading)) {
    heading <- NA_character_
  }

  rows <- html_elements(page, "table tr")

  field_data <- map_dfr(rows, function(row) {
    cells <- html_elements(row, "th, td")

    if (length(cells) < 2) {
      return(NULL)
    }

    field <- clean_cell_text(cells[[1]])
    value <- clean_cell_text(cells[[2]])

    if (field == "" || is.na(field)) {
      return(NULL)
    }

    tibble(field = field, value = value)
  })

  metadata <- tibble(
    object_type = object_type,
    dmd_id = extract_dmd_id(detail_url),
    detail_url = detail_url,
    source_url = parent_url,
    search_result_name = result_name,
    page_heading = heading
  )

  if (nrow(field_data) == 0) {
    return(metadata)
  }

  names_vec <- make_unique_names(field_data$field)
  values <- as.list(field_data$value)
  names(values) <- names_vec

  bind_cols(metadata, as_tibble(values))
}

scrape_object_type <- function(object_type, parent_url) {
  message("Finding links for ", object_type, " ...")

  links <- tryCatch(
    get_detail_links(parent_url, object_type),
    error = function(e) {
      add_error(object_type, parent_url, "parent_page", conditionMessage(e))
      tibble()
    }
  )

  if (nrow(links) == 0) {
    message("No detail links found for ", object_type)
    return(tibble())
  }

  message("Found ", nrow(links), " links for ", object_type)

  results <- vector("list", nrow(links))

  for (i in seq_len(nrow(links))) {
    detail_url <- links$detail_url[i]
    result_name <- links$result_name[i]

    message("[", object_type, "] ", i, "/", nrow(links), ": ", result_name)

    one_result <- tryCatch(
      parse_detail_page(
        detail_url = detail_url,
        parent_url = parent_url,
        object_type = object_type,
        result_name = result_name
      ),
      error = function(e) {
        add_error(object_type, detail_url, "detail_page", conditionMessage(e))
        NULL
      }
    )

    results[[i]] <- one_result

    Sys.sleep(sleep_seconds)
  }

  bind_rows(results)
}

# -----------------------------
# Run scrape
# -----------------------------

for (object_type in names(parent_urls)) {
  message("\n==============================")
  message("Scraping object type: ", object_type)
  message("==============================")

  data <- scrape_object_type(
    object_type = object_type,
    parent_url = parent_urls[[object_type]]
  )

  out_path <- file.path(output_dir, output_files[[object_type]])

  write_csv(data, out_path, na = "")

  message("Saved: ", out_path)
}

# -----------------------------
# Save errors
# -----------------------------

errors_df <- bind_rows(errors)

error_path <- file.path(output_dir, "scrape_errors.csv")
write_csv(errors_df, error_path, na = "")

message("\nDone.")
message("CSV files saved in: ", normalizePath(output_dir))
message("Errors saved to: ", error_path)