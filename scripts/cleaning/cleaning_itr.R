
# Load Packages ----------------------------------------------------------

library(tidyverse)

# Importing --------------------------------------------------------------

import_dir <- "data/raw_data/"
files_dirs <- c("dfp/", "itr/", "fca/")
fca <- read_rds("data/processed_data/clean/fca_clean.rds")

import_tibble <- tibble(
  files = unlist(map(str_c(import_dir, files_dirs), list.files))
  ) |> 
  mutate(doc = str_extract(files, ".{3}")) |> 
  filter(
    doc != "fca",
    str_detect(files, "_(BPA|BPP|DRE)_")
  ) |> 
  mutate(
    dir = if_else(
      doc == "dfp",
      str_c(import_dir, "dfp/"),
      str_c(import_dir, "itr/")
    ),
    path = str_c(dir, files)
  ) 

# ITR - DRE

import_dre <- import_tibble |> 
  filter(doc == "itr", str_detect(files, "_DRE")) |> 
  pull(path)

dre <- map(
  import_dre,
  read_csv2,
  show_col_types = FALSE,
  locale = locale(encoding = "latin1")
)

dre_clean <- dre |> 
  bind_rows() |> 
  filter(ST_CONTA_FIXA == "S", ORDEM_EXERC == "ÚLTIMO") |> 
  mutate(
    int = ceiling(
      time_length(interval(DT_INI_EXERC, DT_FIM_EXERC), "month")
    )
  ) |>
  filter(int == 3) |> 
  janitor::remove_constant() |> 
  distinct() |> 
  mutate(
    VL_CONTA = parse_number(VL_CONTA),
    VL_CONTA = if_else(ESCALA_MOEDA == "MIL", VL_CONTA * 1000, VL_CONTA),
    tri = case_when(
      month(DT_FIM_EXERC) %in% 3:5 ~ "x1T",
      month(DT_FIM_EXERC) %in% 6:8 ~ "x2T",
      month(DT_FIM_EXERC) %in% 9:11 ~ "x3T",
      TRUE ~ NA_character_
    ),
    tri = str_c(tri, str_sub(year(DT_FIM_EXERC), 3, 4))
  ) |> 
  select(!c(ESCALA_MOEDA, VERSAO, CNPJ_CIA)) |> 
  janitor::clean_names() |> 
  left_join(
    fca, 
    by = join_by(cd_cvm)
  ) |> 
  select(!denom_cia) |>
  filter(
    !setor %in% c("Bancos", "Serv. Financeiros", "Seguros"),
    !is.na(setor),
    !is.na(tri),
    !cd_cvm %in% c("016438", "027707", "020125"),
    cd_conta < 999
  )

dre_cleaner <- dre_clean |> 
  select(cd_cvm, nome_sa, setor, ends_with("_conta"), tri) |> 
  pivot_wider(
    names_from = tri,
    values_from = vl_conta
  ) |>   
  filter(if_all(starts_with("x"), ~ !is.na(.x))) |> 
  filter(!is.na(nome_sa))

# Exporting --------------------------------------------------------------

write_rds(dre_cleaner, "data/processed_data/clean/dre_itr_clean.rds")