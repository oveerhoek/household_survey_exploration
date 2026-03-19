library(tidyverse)
library(haven)

PATH_LISS_DATA <- "data/liss/"
PATH_LISS_TMP_DATA <- "tmp/liss/"

#' Load and stack LISS .dta files matching a pattern
#'
#' Reads all Stata (.dta) files in the \code{"LISS Panel"} directory whose
#' filenames match \code{pat}, and row-binds them into a single tibble.
#' Prints a progress message after each file is appended.
#'
#' @param pat A regular expression used to match filenames (passed to
#'   \code{list.files(pattern = ...)}), e.g. \code{"cw.*dta"}.
#'
#' @return A tibble containing the row-bound contents of all matching files.
#'
#' @details
#' This function loads full datasets from disk and can be memory-intensive
#' for wide/large waves. It assumes files share compatible column names.
#'
#' @examples
#' \dontrun{
#' core <- load_files("avars.*dta")
#' cw   <- load_files("cw.*dta")
#' }
#'
#' @export
load_files <- function(pat) {
  files <- list.files(PATH_LISS_DATA, pattern = pat, full.names = TRUE)
  
  df <- tibble()
  
  i <- 1
  n <- length(files)
  
  for (file in files) {
    df <- bind_rows(df, haven::read_dta(file))
    print(paste0(i, " / ", n, ": ", round(object.size(df) / (1000 ** 2), 0), "MB"))
    i <- i + 1
  }
  
  return(df)
}

#' Collect column names for LISS .dta files by wave/year
#'
#' Reads the column names from each Stata (.dta) file in the \code{"LISS Panel"}
#' directory whose filenames match \code{pat}, and returns them as a named list
#' keyed by year.
#'
#' @param pat A regular expression used to match filenames (passed to
#'   \code{list.files(pattern = ...)}), e.g. \code{"cw.*dta"}.
#'
#' @return A named list. Each element is a character vector of column names for
#'   one year/wave; list names are four-digit years as strings (e.g. \code{"2018"}).
#'
#' @details
#' The function extracts the year from the filename using fixed character
#' positions (\code{substr(file, 14, 15)}), so it assumes a specific directory/name
#' layout. If that layout changes, year extraction should be rewritten using
#' \code{basename()} plus a regex.
#'
#' For variables starting with \code{"cd"} (housing module), the function strips
#' the first 5 characters so that \code{"cd18a038"} becomes \code{"038"} etc.
#'
#' @examples
#' \dontrun{
#' cw_cols <- load_colnames("cw.*dta")
#' cd_cols <- load_colnames("cd.*dta")
#' }
#'
#' @export
load_colnames <- function(pat) {
  files <- paste0(PATH_LISS_DATA, list.files(PATH_LISS_DATA, pattern = pat))
  prefix <- substr(pat, 1, 2) # currently unused; kept if you later branch by module
  
  dl <- list()
  
  for (file in files) {
    year <- paste0("20", substr(file, 14, 15))
    cols <- colnames(haven::read_dta(file))
    
    cols <- ifelse(
      grepl("^cd", cols),
      substr(cols, 6, 99),
      cols
    )
    
    dl[[year]] <- cols
  }
  
  return(dl)
}

#' Extract the three-digit LISS variable code from a variable name
#'
#' Given a LISS variable name of the form \code{"cw18a134"}, returns the
#' three-digit code portion (e.g. \code{"134"}).
#'
#' @param variable_name A character scalar containing a LISS variable name.
#'
#' @return A character scalar with the three-digit variable code.
#'
#' @details
#' This implementation assumes the code begins at character 6 and ends at 8.
#' If naming conventions change (e.g., underscores), use a regex instead.
#'
#' @examples
#' extract_variable_code("cw18a134")  # "134"
#'
#' @export
extract_variable_code <- function(variable_name) {
  return(substr(variable_name, 6, 8))
}

#' Extract the survey year from a LISS variable name
#'
#' Given a LISS variable name containing a two-digit year at positions 3--4
#' (e.g. \code{"cw18a134"}), returns the four-digit year (e.g. \code{2018}).
#'
#' @param variable_name A character scalar containing a LISS variable name.
#'
#' @return An integer year (e.g. \code{2018}).
#'
#' @details
#' This implementation assumes the year is stored as \code{YY} at positions 3--4.
#' If naming conventions change, use a regex extraction.
#'
#' @examples
#' extract_variable_year("cw18a134")  # 2018
#'
#' @export
extract_variable_year <- function(variable_name) {
  return(2000 + as.integer(substr(variable_name, 3, 4)))
}

# ---- Data creation pipelines (not exported functions) ----

write_rds(load_files("avars.*dta"), paste0(PATH_LISS_TMP_DATA, "liss_core.Rds"))
write_rds(load_files("cd.*dta"), paste0(PATH_LISS_TMP_DATA, "liss_housing.Rds"))

housing <- load_files("cd.*dta") %>%
  select(
    nomem_encr,
    matches("^cd\\d{2}[a-z]003$"),
    matches("^cd\\d{2}[a-z]005$"),
    matches("^cd\\d{2}[a-z]086$")
  ) %>%
  rename_with(
    ~ paste0(
      case_when(
        extract_variable_code(.x) == "003" ~ "renter",
        extract_variable_code(.x) == "005" ~ "pays_rent",
        extract_variable_code(.x) == "086" ~ "receives_rent_subsidy"
      ),
      extract_variable_year(.x)
    ),
    -nomem_encr
  ) %>% 
  group_by(nomem_encr) %>%
  summarise(
    across(matches("^(renter|pays_rent|receives_rent_subsidy)\\d{4}$"),
           ~ dplyr::first(na.omit(.x)))
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = matches("^(renter|pays_rent|receives_rent_subsidy)\\d{4}$"),
    names_to = c(".value", "year"),
    names_pattern = "^(renter|pays_rent|receives_rent_subsidy)(\\d{4})$"
  ) %>%
  transmute(
    nomem_encr,
    year = as.numeric(year),
    renter = case_when(
      renter %in% c(1, 2) ~ 1,
      renter %in% c(3, 4) ~ 0,
      .default = NA_real_
    ),
    pays_rent = case_when(
      pays_rent == 1 ~ 1,
      pays_rent == 0 ~ 0,
      .default = NA_real_
    ),
    receives_rent_subsidy = case_when(
      receives_rent_subsidy %in% c(1, 2) ~ 1,
      receives_rent_subsidy == 3 ~ 0,
      receives_rent_subsidy == 98 ~ NA_real_,
      .default = NA_real_
    )
  )

write_rds(housing, paste0(PATH_LISS_TMP_DATA, "liss_housing.Rds"))

coresidence <- load_files("cf.*dta") %>%
  select(
    nomem_encr,
    matches("^cf\\d{2}[a-z]017$"),
    matches("^cf\\d{2}[a-z]451$")
  ) %>%
  rename_with(
    ~ paste0("coresidence", 2000 + as.integer(substr(.x, 3, 4))),
    -nomem_encr
  ) %>%
  group_by(nomem_encr) %>%
  summarise(
    across(starts_with("coresidence"),
           ~ dplyr::first(na.omit(.x)))
  ) %>%
  ungroup() %>%
  pivot_longer(
    names_to = "year",
    values_to = "coresidence",
    cols = starts_with("coresidence"),
    names_prefix = "coresidence"
  ) %>%
  transmute(
    nomem_encr,
    year = as.numeric(year),
    coresidence = case_when(
      coresidence == 4 ~ 1,
      .default = 0
    )
  )

write_rds(coresidence, paste0(PATH_LISS_TMP_DATA, "liss_coresidence.Rds"))

work_schooling <- load_files("cw.*dta") %>%
  select(
    nomem_encr,
    matches("^cw\\d{2}[a-z]134$"),
    matches("^cw\\d{2}[a-z]135$"),
    matches("^cw\\d{2}[a-z]319$"),
    matches("^cw\\d{2}[a-z]320$")
  ) %>%
  rename_with(
    ~ paste0(
      case_when(
        extract_variable_code(.x) == "134" ~ "job_new_start_year",
        extract_variable_code(.x) == "135" ~ "job_new_start_month",
        extract_variable_code(.x) == "319" ~ "job_old_start_year",
        extract_variable_code(.x) == "320" ~ "job_old_start_month"
      ),
      extract_variable_year(.x)
    ),
    -nomem_encr
  ) %>%
  group_by(nomem_encr) %>%
  summarise(
    across(starts_with("job"),
           ~ dplyr::first(na.omit(.x)))
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("job"),
    names_to = c(".value", "year"),
    names_pattern = "^(job_.*?)(\\d{4})$"
  ) %>%
  transmute(
    nomem_encr,
    year = as.numeric(year),
    job_new_start_year = as.numeric(job_new_start_year),
    job_new_start_month = as.numeric(job_new_start_month),
    job_new_start_month = case_when(
      job_new_start_month == 999 ~ NA,
      .default = job_new_start_month
    ),
    job_old_start_year = as.numeric(job_old_start_year),
    job_old_start_month = as.numeric(job_old_start_month),
    job_old_start_month = case_when(
      job_old_start_month == 999 ~ NA,
      .default = job_old_start_month
    )
  )

write_rds(work_schooling, paste0(PATH_LISS_TMP_DATA, "liss_work_schooling.Rds"))

highest_educ <- load_files("cw.*dta") %>%
  select(
    nomem_encr,
    matches("^cw\\d{2}[a-z]005$")
  ) %>%
  rename_with(
    ~ paste0(
      case_when(
        extract_variable_code(.x) == "005" ~ "highest_educ"
      ),
      extract_variable_year(.x)
    ),
    -nomem_encr
  ) %>%
  group_by(nomem_encr) %>%
  summarise(
    across(starts_with("highest_educ"),
           ~ dplyr::first(na.omit(.x)))
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("highest_educ"),
    names_to = c(".value", "year"),
    names_pattern = "^(highest_educ.*?)(\\d{4})$"
  ) %>%
  transmute(
    nomem_encr,
    year = as.numeric(year),
    highest_educ
  )

write_rds(highest_educ, paste0(PATH_LISS_TMP_DATA, "liss_highest_educ.Rds"))
