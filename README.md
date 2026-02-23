==============================================================================
# IHE Recipients of USAID Grants and Contracts (USAspending.gov API and R)
==============================================================================

 DESCRIPTION
-------
Script uses keyword searches and manual category assignment (Institution of 
higher education, or related to IHE - Y/N) to identify active grants and contracts 
potentially at risk of termination due to USAID shuttering. 



## MECHANICS

This script runs in two cycles ... or "rounds," controlled by the ROUND variable:

## -- ROUND 1

- Downloads all USAID award transactions from the bulk download API.

- Creates a pivot table of one record/row per award that were active as of 
ACTIVE_DATE (March 11, 2025), harvests results, and saves them in a cache.

- Two CSVs are produced for review, 
... table1.csv: Catalogs positive "hits" on IHE-related keyword searches ("universit", "college", etc.).
Manual IHE assignment/confirmation undertaken in blank "IHE" column/field, 
eliminating false positives (UNIVERSal Tire, COLLEGE Movers) by classifying with "N" notation.

... table2.csv: Catalogs False negatives "hits" on keyword searches. 
Manual IHE assignment/confirmation undertaken in blank "IHE" column/field,
re-classifying IHE-related "false negative" recipients (Texas A&M AgriLife Research", "The Administrators
of Tulane Educational Fund) with a "Y" notation in the blank "IHE" column/field.

## -- ROUND 2

- Incorporates notation logic from Round 1's Table 1 and Table 2
- Produces a final publishable product in Round 2's Table 1




# !!! IMPORTANT !!!
#
Regarding FORCE_REFRESH ...
- Set FORCE_REFRESH to TRUE > Re-download from the API
- Set FORCE_REFRESH to FALSE > After successful download, use/reuse cached data

- USAspending.gov's bulk download API limits pulls to max oof 365 per request.

- Rather than a single request, the script undertakes 11 requests pulling 365-days of data. 
 
- To reduce API runs, successfully harvested 365-day chunks saved as 
"partial caches" (_partial_*.rds). When/if script re-reun, script will skip already-downloaded
"partial caches."

- If script runs successfully during 1st "ROUND" and "Y/N" classifiers are table1.csv and table2.csv
input into the "IHE" field/column of both CSVs, then ...

... change "ROUND" in "Round-Setting and Re-Downloader" Section below to ROUND to "2" and re-run the script. 
If you don't want to redownload bulk downloads (and instead use a cache file), set FORCE_REFRESH to FALSE.

- May need to run code during off-peak hours, or close out of R Studio completely if problem 
persists with API pulls






library(httr)
library(jsonlite)
library(tidyverse, warn.conflicts = FALSE)
library(stringr)
library(readxl)




# -- Round-Setting and Redownloader

ROUND <- 1

FORCE_REFRESH  <- TRUE  # TRUE = re-download from API // FALSE = use cache




# -- Algebra

BULK_DL_URL    <- "https://api.usaspending.gov/api/v2/bulk_download/awards/"
DL_STATUS_URL  <- "https://api.usaspending.gov/api/v2/download/status"

ACTIVE_DATE    <- as.Date("2025-03-11")  # Will limit analysis to awards active on date

CACHE_FILE     <- "awards_active_cache.rds"
FORCE_REFRESH  <- TRUE                     # TRUE = re-download from API; FALSE = use cache

working_directory <- "~/GitHub/r-usaid-ihe-funding"

setwd(working_directory)

ASST_TYPES     <- c("02", "03", "04", "05", "06", "10", "11")  # Grants & financial assistance
CONT_TYPES     <- c("A", "B", "C", "D")                        # Contracts





# -- Keyword-Setting and "Number-Writing" to CSV

IHE_KEYWORDS <- regex(
  str_c(
    "univ",
    "school",
    "colleg",
    "institut",
    "acad",
    "regent",
    "higher ed",
    "post-secondary",
    "post secondary",
    "postsecondary",
    "seminar",
    "suny",
    "cuny",
    "conservator",
    "board of trustee",
    "board of governor",
    "board of visitor",
    "board of curator",
    "polytech",
    sep = "|"
  ),
  ignore_case = TRUE
)



options(scipen = 999)






# -- Bulk Downloader


make_year_chunks <- function(start_date = "2015-01-01",
                             end_date   = "2025-03-11") {
  
  
  
  start <- as.Date(start_date)
  end   <- as.Date(end_date)
  chunks <- list()
  chunk_start <- start
  
  
  while (chunk_start < end) {
    chunk_end <- min(chunk_start + 364, end)
    chunks <- c(chunks, list(list(
      start_date = as.character(chunk_start),
      end_date   = as.character(chunk_end)
    )))
    
    
    chunk_start <- chunk_end + 1
  }
  chunks
}








fetch_bulk_chunk <- function(start_date, end_date, award_types, temp_dir) {
  
  
  
  body <- list(
    filters = list(
      agencies = list(list(
        type = "awarding",
        tier = "toptier",
        name = "Agency for International Development"
      )),
      
      
      prime_award_types = as.list(award_types),
      date_type  = "action_date",
      date_range = list(
        start_date = start_date,
        end_date   = end_date
      )
    ),
    file_format = "csv"
  )

  

  resp <- NULL
  for (post_attempt in 1:3) {
    tryCatch({
      resp <- POST(BULK_DL_URL,
                   body = toJSON(body, auto_unbox = TRUE),
                   content_type_json(),
                   timeout(120))
      if (status_code(resp) < 500) break
      cat(sprintf("    !!! POST attempt %d got HTTP %d, retrying...\n",
                  post_attempt, status_code(resp)))
    }, error = function(e) {
      cat(sprintf("    !!! POST attempt %d error: %s\n", post_attempt, e$message))
    })
    Sys.sleep(15)
  }

  if (is.null(resp) || status_code(resp) >= 400) {
    cat(sprintf("    !!! POST FAILED: %s to %s (award types: %s)\n",
                start_date, end_date, paste(award_types, collapse = ",")))
    return(list())
  }
  
  
  
  
  
  
  dl_info <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  
  
  
  
  
  
# -- Wait (max: 10 min)
  
  
  file_name <- dl_info$file_name
  status <- list(status = "running")
  
  for (i in 1:60) {
    Sys.sleep(10)
    status_resp <- GET(paste0(DL_STATUS_URL, "?file_name=",
                              URLencode(file_name, reserved = TRUE)))
    
    
    status <- fromJSON(content(status_resp, "text", encoding = "UTF-8"))
    
    
    
    
    if (identical(status$status, "finished")) {
      
      break
    }
    
    
    
    if (identical(status$status, "failed")) {
      cat(sprintf("    !!! FAILED: %s to %s\n", start_date, end_date))
      return(list())
    }
    
    flush.console()
  }
  
  if (!identical(status$status, "finished")) {
    cat(sprintf("    !!! TIMED OUT: %s to %s\n", start_date, end_date))
    return(list())
  }
  
  
  
  
  
# -- Bulk Download ZIP
# -- Delay overheating CDN

  Sys.sleep(20)
  
  zip_path <- file.path(temp_dir, file_name)
  
  dl_ok <- FALSE
  for (attempt in 1:3) {
    tryCatch({
      resp_dl <- GET(dl_info$file_url,
                     write_disk(zip_path, overwrite = TRUE),
                     timeout(300))
      
      
      
      if (status_code(resp_dl) == 200 && file.exists(zip_path) &&
          file.size(zip_path) > 0) {
        dl_ok <- TRUE
        break
      }
    }, error = function(e) {
      cat(sprintf("    !!! Download attempt %d failed: %s\n", attempt, e$message))
    })
    Sys.sleep(10)
  }
  
  if (!dl_ok) {
    cat(sprintf("    !!! DOWNLOAD FAILED after 3 attempts: %s to %s\n",
                start_date, end_date))
    if (file.exists(zip_path)) file.remove(zip_path)
    return(list())
  }
  
  
  
# -- Unzip, Target, Extract CSVs
  
  csv_files <- unzip(zip_path, exdir = temp_dir)
  
  
  
  
  
  result <- list()
  
  
  for (f in csv_files) {
    df <- read_csv(f, show_col_types = FALSE,
                   col_types = cols(.default = col_character()))
    
    
    if (grepl("Assistance", basename(f))) {
      result$assistance <- bind_rows(result$assistance, df)
      
      
    } else if (grepl("Contracts", basename(f))) {
      result$contracts <- bind_rows(result$contracts, df)
    }
    file.remove(f)
  }
  file.remove(zip_path)
  
  result
}





# -- CSV Column Standardization

# Script to eventually combine assistance and contract records into one file. 

# The assistance and contract bulk downloads rely on different field schemas.



# ... Assistance

standardize_assistance <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  
  df |>
    transmute(
      generated_unique_award_id = assistance_award_unique_key,
      award_id       = award_id_fain,
      recipient_uei  = recipient_uei,
      recipient_name = recipient_name,
      recipient_name_raw     = recipient_name_raw,
      recipient_parent_name  = recipient_parent_name,
      recipient_parent_uei   = recipient_parent_uei,
      period_of_performance_start_date     = period_of_performance_start_date,
      period_of_performance_end_date       = period_of_performance_current_end_date,
      period_of_performance_potential_end_date = NA_character_,
      total_obligated_amount   = as.numeric(total_obligated_amount),
      award_type     = assistance_type_description,
      prime_award_base_transaction_description      = prime_award_base_transaction_description,
      cfda_number    = cfda_number,
      recipient_city_name         = recipient_city_name,
      recipient_state_code        = recipient_state_code,
      primary_place_of_performance_country_name      = primary_place_of_performance_country_name,
      naics_code          = NA_character_,
      naics_description   = NA_character_
    )
}





# ... Contracts

standardize_contracts <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  df |>
    transmute(
      generated_unique_award_id = contract_award_unique_key,
      award_id       = award_id_piid,
      recipient_uei  = recipient_uei,
      recipient_name = recipient_name,
      recipient_name_raw     = recipient_name_raw,
      recipient_parent_name  = recipient_parent_name,
      recipient_parent_uei   = recipient_parent_uei,
      period_of_performance_start_date     = period_of_performance_start_date,
      period_of_performance_end_date       = pmax(
        as.Date(period_of_performance_current_end_date),
        as.Date(period_of_performance_potential_end_date),
        na.rm = TRUE
      ) |> as.character(),
      period_of_performance_potential_end_date = period_of_performance_potential_end_date,
      total_obligated_amount   = as.numeric(potential_total_value_of_award),
      award_type     = award_type,
      prime_award_base_transaction_description      = prime_award_base_transaction_description,
      cfda_number    = NA_character_,
      recipient_city_name         = recipient_city_name,
      recipient_state_code        = recipient_state_code,
      primary_place_of_performance_country_name      = primary_place_of_performance_country_name,
      naics_code          = naics_code,
      naics_description   = naics_description
    )
}







# -- Encoding Prep (Fix garbled unicode)

clean_text <- function(x) {
  x |>
    str_replace_all("\u201C|\u201D", '"') |>
    str_replace_all("\u2018|\u2019", "'") |>
    str_replace_all("\u2013",        "-") |>
    str_replace_all("\u2014",       "--") |>
    str_replace_all("\u2026",      "...") |>
    str_replace_all("\u00A0",        " ")
}








# WebFetch and Bulk Downloads (If FORCE_REFRESH=TRUE and No Cache) 

if (!FORCE_REFRESH && file.exists(CACHE_FILE)) {
  
  awards_active <- readRDS(CACHE_FILE)
  
} else {
  
  t0 <- Sys.time()
  temp_dir <- tempdir()
  chunks   <- make_year_chunks("2015-01-01", "2025-03-11")
  
  all_assistance <- tibble()
  all_contracts  <- tibble()
  
  
  
  

  
# -- Fetch assistance awards (in 365-day "chunks")

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    partial_file <- sprintf("_partial_asst_%02d.rds", i)

    if (file.exists(partial_file)) {
      cat(sprintf("  ASST chunk %d/%d: %s to %s  [cached]\n",
                  i, length(chunks), chunk$start_date, chunk$end_date))
      result <- readRDS(partial_file)
    } else {
      cat(sprintf("  ASST chunk %d/%d: %s to %s\n",
                  i, length(chunks), chunk$start_date, chunk$end_date))
      flush.console()
      result <- fetch_bulk_chunk(chunk$start_date, chunk$end_date,
                                 ASST_TYPES, temp_dir)
      if (length(result) > 0) saveRDS(result, partial_file)
      Sys.sleep(10)
    }

    if (!is.null(result$assistance))
      all_assistance <- bind_rows(all_assistance, result$assistance)
  }






# -- Fetch contract awards


  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    partial_file <- sprintf("_partial_cont_%02d.rds", i)

    if (file.exists(partial_file)) {
      cat(sprintf("  CONT chunk %d/%d: %s to %s  [cached]\n",
                  i, length(chunks), chunk$start_date, chunk$end_date))
      result <- readRDS(partial_file)
    } else {
      cat(sprintf("  CONT chunk %d/%d: %s to %s\n",
                  i, length(chunks), chunk$start_date, chunk$end_date))
      flush.console()
      result <- fetch_bulk_chunk(chunk$start_date, chunk$end_date,
                                 CONT_TYPES, temp_dir)
      if (length(result) > 0) saveRDS(result, partial_file)
      Sys.sleep(10)
    }

    if (!is.null(result$contracts))
      all_contracts <- bind_rows(all_contracts, result$contracts)
  }
  
  elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
  cat(sprintf("Bulk download completed in %s minutes.\n\n", elapsed))






# -- Standardize columns
  
  grants    <- standardize_assistance(all_assistance)
  
  contracts <- standardize_contracts(all_contracts)
  
  awards_all <- bind_rows(grants, contracts)
  
  
  



# -- Collapse transactions to one record/row per award, ignore transaction modification/action records





  awards_all <- awards_all |>
    group_by(generated_unique_award_id) |>
    summarise(
      award_id       = first(na.omit(award_id)),
      recipient_uei  = first(na.omit(recipient_uei)),
      recipient_name = first(na.omit(recipient_name)),
      recipient_name_raw     = first(na.omit(recipient_name_raw)),
      recipient_parent_name  = first(na.omit(recipient_parent_name)),
      recipient_parent_uei   = first(na.omit(recipient_parent_uei)),
      period_of_performance_start_date     = min(period_of_performance_start_date, na.rm = TRUE),
      period_of_performance_end_date       = max(period_of_performance_end_date, na.rm = TRUE),
      period_of_performance_potential_end_date = ifelse(all(is.na(period_of_performance_potential_end_date)), NA_character_,
                                   max(period_of_performance_potential_end_date, na.rm = TRUE)),
      total_obligated_amount   = ifelse(all(is.na(total_obligated_amount)), NA_real_,
                                max(total_obligated_amount, na.rm = TRUE)),
      award_type     = first(na.omit(award_type)),
      prime_award_base_transaction_description      = first(na.omit(prime_award_base_transaction_description)),
      cfda_number    = first(na.omit(cfda_number)),
      recipient_city_name         = first(na.omit(recipient_city_name)),
      recipient_state_code        = first(na.omit(recipient_state_code)),
      primary_place_of_performance_country_name      = first(na.omit(primary_place_of_performance_country_name)),
      naics_code          = first(na.omit(naics_code)),
      naics_description   = first(na.omit(naics_description)),
      .groups          = "drop"
    )
  
  
  
  
  
# -- Testing Whether Award was "Active" On "ACTIVE_DATE"

  awards_active <- awards_all |>
    mutate(start_dt = as.Date(period_of_performance_start_date),
           end_dt   = as.Date(period_of_performance_end_date)) |>
    filter(!is.na(start_dt), !is.na(end_dt),
           start_dt <= ACTIVE_DATE,
           end_dt   >= ACTIVE_DATE) |>
    mutate(
      



# -- Fix ASST issue >> API returns _072 (toptier) vs. reference uses _7200 (subtier)
      
      generated_unique_award_id = str_replace(generated_unique_award_id, "_072$", "_7200")
    )
  
  
  
  saveRDS(awards_active, CACHE_FILE)






  # -- Merge and zip together partial caches ... 

  partial_files <- list.files(pattern = "^_partial_(asst|cont)_\\d+\\.rds$")
  if (length(partial_files) > 0) {
    file.remove(partial_files)
  }
}






# -- Ensure suffix normalization is applied even when loading from cache

awards_active <- awards_active |>
  mutate(
    generated_unique_award_id = str_replace(generated_unique_award_id, "_072$", "_7200")
  )





# -- Keyword Matching and Summarizing

recipients <- awards_active |>
  group_by(recipient_uei, recipient_name) |>
  summarise(
    recipient_city_name  = first(na.omit(recipient_city_name)),
    recipient_state_code = first(na.omit(recipient_state_code)),
    recipient_name_raw    = first(na.omit(recipient_name_raw)),
    recipient_parent_name = first(na.omit(recipient_parent_name)),
    recipient_parent_uei  = first(na.omit(recipient_parent_uei)),
    naics_codes     = paste(unique(na.omit(naics_code)), collapse = "; "),
    naics_descriptions = paste(unique(na.omit(naics_description)), collapse = "; "),
    .groups         = "drop"
  ) |>
  mutate(
    keyword_match = str_detect(recipient_name, IHE_KEYWORDS) |
                    str_detect(replace_na(recipient_name_raw, ""), IHE_KEYWORDS) |
                    str_detect(replace_na(recipient_parent_name, ""), IHE_KEYWORDS),
    naics_6113    = str_detect(naics_codes, "^6113|; 6113")
  )





# -- Collect both recipient UEIs and parent UEIs of keyword-matched recipients

keyword_ueis <- recipients |>
  filter(keyword_match) |>
  { \(d) unique(c(d$recipient_uei, na.omit(d$recipient_parent_uei))) }()






# -- ROUND 1

if (ROUND == 1) {

  dir.create("round_1", showWarnings = FALSE)






# -- Table 1: Keyword-matched awards (review for false positives)

  table1 <- awards_active |>
    filter(recipient_uei %in% keyword_ueis |
           recipient_parent_uei %in% keyword_ueis) |>
    transmute(
      recipient_uei              = recipient_uei,
      IHE                        = "",
      recipient_name             = clean_text(recipient_name),
      recipient_name_raw         = clean_text(recipient_name_raw),
      recipient_parent_name      = clean_text(recipient_parent_name),
      recipient_parent_uei       = recipient_parent_uei,
      recipient_city_name        = recipient_city_name,
      recipient_state_code       = recipient_state_code,
      prime_award_base_transaction_description = clean_text(prime_award_base_transaction_description),
      total_obligated_amount     = total_obligated_amount,
      period_of_performance_start_date         = period_of_performance_start_date,
      period_of_performance_end_date           = period_of_performance_end_date,
      period_of_performance_potential_end_date = period_of_performance_potential_end_date,
      primary_place_of_performance_country_name = primary_place_of_performance_country_name,
      generated_unique_award_id  = generated_unique_award_id,
      award_type                 = award_type
    )





# -- Table 2: Non-keyword recipients (review for missed IHEs)

  table2 <- recipients |>
    filter(!keyword_match) |>
    transmute(
      recipient_uei          = recipient_uei,
      IHE                    = "",
      recipient_name         = clean_text(recipient_name),
      recipient_name_raw     = clean_text(recipient_name_raw),
      recipient_parent_name  = clean_text(recipient_parent_name),
      recipient_parent_uei   = recipient_parent_uei,
      recipient_city_name,
      recipient_state_code,
      naics_codes,
      naics_descriptions,
      naics_6113
    ) |>
    arrange(desc(naics_6113), recipient_name)
  
  write_excel_csv(table1, "round_1/table1.csv")
  write_excel_csv(table2, "round_1/table2.csv")
  
}







# -- ROUND 2

if (ROUND == 2) {

  dir.create("round_2", showWarnings = FALSE)

  r1_table1 <- read_csv("round_1/table1.csv", show_col_types = FALSE)
  r1_table2 <- read_csv("round_1/table2.csv", show_col_types = FALSE)




# -- False positives: keyword-matched recipients the reviewer flagged as non-IHE

  exclude_ueis <- r1_table1 |>
    filter(toupper(IHE) == "N") |>
    { \(d) unique(c(d$recipient_uei, na.omit(d$recipient_parent_uei))) }()





# -- Manually added IHEs: non-keyword recipients the reviewer flagged as IHE

  manual_ueis <- r1_table2 |>
    filter(toupper(IHE) == "Y") |>
    { \(d) unique(c(d$recipient_uei, na.omit(d$recipient_parent_uei))) }()





# -- Combine: all keyword + manual IHEs, minus false positives

  final_ueis <- setdiff(unique(c(keyword_ueis, manual_ueis)), exclude_ueis)





# -- Build the final award-level table for manually confirmed IHE recipients

  table1 <- awards_active |>
    filter(recipient_uei %in% final_ueis |
           recipient_parent_uei %in% final_ueis) |>
    transmute(
      recipient_uei              = recipient_uei,
      IHE                        = "",
      recipient_name             = clean_text(recipient_name),
      recipient_name_raw         = clean_text(recipient_name_raw),
      recipient_parent_name      = clean_text(recipient_parent_name),
      recipient_parent_uei       = recipient_parent_uei,
      recipient_city_name        = recipient_city_name,
      recipient_state_code       = recipient_state_code,
      prime_award_base_transaction_description = clean_text(prime_award_base_transaction_description),
      total_obligated_amount     = total_obligated_amount,
      period_of_performance_start_date         = period_of_performance_start_date,
      period_of_performance_end_date           = period_of_performance_end_date,
      period_of_performance_potential_end_date = period_of_performance_potential_end_date,
      primary_place_of_performance_country_name = primary_place_of_performance_country_name,
      generated_unique_award_id  = generated_unique_award_id,
      award_type                 = award_type
    )



  write_excel_csv(table1, "round_2/table1.csv")
  
}
