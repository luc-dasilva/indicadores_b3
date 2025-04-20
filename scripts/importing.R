
# Load Packages ----------------------------------------------------------

library(tidyverse)
library(httr2)

# Setup ------------------------------------------------------------------

doc_types <- c("DFP", "ITR", "FCA") 
base_url <- "https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/"
files_years <- 2020:2024
files_basenames <- str_c(str_to_lower(doc_types), "_cia_aberta_")

urls_tibble <- crossing(
    base = files_basenames,
    year = files_years
  ) |> 
  mutate(
    file = str_c(base, year, ".zip"),
    doc = case_when(
      str_detect(base, "dfp") ~ "DFP",
      str_detect(base, "itr") ~ "ITR",
      str_detect(base, "fca") ~ "FCA"
    ),
    urls = str_c(base_url, doc, "/DADOS/", file)
  ) |> 
  filter(
    !(doc == "ITR" & !(year %in% c(2023, 2024))),
    !(doc == "FCA" & !year == 2024)
  ) 

urls <- urls_tibble |> 
  pull(urls)

# Requests ---------------------------------------------------------------

requests <- map(urls, request)
responses <- map(requests, req_perform)

# Exporting --------------------------------------------------------------

output_dir <- "data/raw_data/"

if (dir.exists(output_dir) == FALSE) {
  map(unique(output_dir), dir.create, recursive = TRUE)
}

output_paths <- urls_tibble |>
  mutate(
    output_dir = output_dir,
    output_dir_path = str_c(output_dir, "zip_files/", file)
  ) |> 
  pull(output_dir_path)

responses_bins <- map(responses, pluck, "body")
walk2(responses_bins, output_paths, ~ writeBin(object = .x, con = .y))

# Extracting -------------------------------------------------------------

output_docs <- urls_tibble|>
  mutate(
    output_docs = str_c(
      "data/raw_data/",
      str_to_lower(doc),
      "/"
    )
) |> 
  pull(output_docs)

if (any(dir.exists(unique(output_docs))) == FALSE) {
  map(unique(output_docs), dir.create, recursive = TRUE)
}

walk2(output_paths, output_docs, ~ unzip(zipfile = .x, exdir = .y))

# Selecting --------------------------------------------------------------

all_files <- map(unique(output_docs), list.files)

unused_files <- tibble(files = unlist(all_files)) |> 
  mutate(
    doc = case_when(
      str_detect(files, "^dfp_") ~ "dfp",
      str_detect(files, "^itr_") ~ "itr",
      str_detect(files, "^fca_") ~ "fca"
    )
  ) |> 
  filter(doc != "fca") |> 
  filter(str_detect(files, "_ind_"))

unused_files_dfp <- unused_files |> 
  filter(doc == "dfp") |> 
  mutate(dfp_files = str_c(unique(output_docs)[1], files)) |> 
  pull(dfp_files)

if (any(file.exists(unused_files_dfp) == TRUE)) {
  file.remove(unused_files_dfp)
}

unused_files_itr <- unused_files |> 
  filter(doc == "itr") |> 
  mutate(itr_files = str_c(unique(output_docs)[3], files)) |> 
  pull(itr_files)

if (any(file.exists(unused_files_itr) == TRUE)) {
  file.remove(unused_files_itr)
}
