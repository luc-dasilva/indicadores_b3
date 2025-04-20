
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

# DFPs - BPA
import_bpa <- import_tibble |> 
  filter(doc == "dfp", str_detect(files, "_BPA")) |> 
  pull(path)

bpa <- map(
  import_bpa,
  read_csv2,
  show_col_types = FALSE,
  locale = locale(encoding = "latin1")
)

bpa_clean <- bpa |> 
  bind_rows() |> 
  filter(ST_CONTA_FIXA == "S", ORDEM_EXERC == "ÚLTIMO") |> 
  janitor::remove_constant() |> 
  distinct() |> 
  mutate(
    VL_CONTA = parse_number(VL_CONTA),
    VL_CONTA = if_else(ESCALA_MOEDA == "MIL", VL_CONTA * 1000, VL_CONTA),
    year = year(DT_REFER)
  ) |> 
  select(!c(ESCALA_MOEDA, VERSAO, CNPJ_CIA, DENOM_CIA, DT_FIM_EXERC, DT_REFER)) |> 
  janitor::clean_names() |> 
  left_join(
    fca, 
    by = join_by(cd_cvm)
  ) |> 
  filter(
    !setor %in% c("Bancos", "Serv. Financeiros", "Seguros"),
    !is.na(setor),
    !cd_cvm %in% c("016438", "026328", "026077", "021148"),
    cd_conta < 99999
  )

bpa_cleaner <- bpa_clean |> 
  pivot_wider(
    names_from = year,
    values_from = vl_conta
  ) |> 
  janitor::clean_names()

# DFPs - BPP
import_bpp <- import_tibble |> 
  filter(doc == "dfp", str_detect(files, "_BPP")) |> 
  pull(path)

bpp <- map(
  import_bpp,
  read_csv2,
  show_col_types = FALSE,
  locale = locale(encoding = "latin1")
)

bpp_clean <- bpp |> 
  bind_rows() |>
  filter(ST_CONTA_FIXA == "S", ORDEM_EXERC == "ÚLTIMO") |> 
  janitor::remove_constant() |> 
  distinct() |> 
  mutate(
    VL_CONTA = parse_number(VL_CONTA),
    VL_CONTA = if_else(ESCALA_MOEDA == "MIL", VL_CONTA * 1000, VL_CONTA),
    year = year(DT_REFER)
  ) |> 
  select(!c(ESCALA_MOEDA, VERSAO, CNPJ_CIA, DENOM_CIA, DT_FIM_EXERC, DT_REFER)) |> 
  janitor::clean_names() |> 
  left_join(
    fca, 
    by = join_by(cd_cvm)
  ) |> 
  filter(
    !setor %in% c("Bancos", "Serv. Financeiros", "Seguros"),
    !is.na(setor),
    !cd_cvm %in% c("016438", "026328", "026077", "021148"),
    cd_conta < 99999
  ) |> 
  mutate(
    ds_conta = case_when(
      cd_conta == 20201 ~ "Empréstimos e Financiamentos de LP",
      cd_conta == 20202 ~ "Outras Obrigações de LP",
      cd_conta == 20204 ~ "Provisões de LP",
      cd_conta == 20205 ~ "Passivos sobre Ativos Não-Correntes a Venda e Descontinuados de LP",
      .default = ds_conta
    )
  )

bpp_cleaner <- bpp_clean |> 
  pivot_wider(
    names_from = year,
    values_from = vl_conta
  ) |> 
  janitor::clean_names()

# DFPs - DRE
import_dre <- import_tibble |> 
  filter(doc == "dfp", str_detect(files, "_DRE")) |> 
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
  janitor::remove_constant() |> 
  distinct() |> 
  mutate(
    VL_CONTA = parse_number(VL_CONTA),
    VL_CONTA = if_else(ESCALA_MOEDA == "MIL", VL_CONTA * 1000, VL_CONTA),
    year = year(DT_REFER)
  ) |> 
  select(
    !c(
      ESCALA_MOEDA, VERSAO, CNPJ_CIA, DENOM_CIA,
      DT_INI_EXERC, DT_FIM_EXERC, DT_REFER
    )
  ) |> 
  janitor::clean_names() |> 
  left_join(
    fca, 
    by = join_by(cd_cvm)
  ) |> 
  filter(
    !setor %in% c("Bancos", "Serv. Financeiros", "Seguros"),
    !is.na(setor),
    !cd_cvm %in% c("016438", "027707", "020125", "026328", "026077", "021148"),
    cd_conta < 999
  )

dre_cleaner <- dre_clean |> 
  select(!cd_conta) |> 
  pivot_wider(
    names_from = year,
    values_from = vl_conta
  ) |> 
  janitor::clean_names()

# Exporting --------------------------------------------------------------

if (dir.exists("data/processed_data/clean/") ==  FALSE) {
  dir.create("data/processed_data/clean/")
}

write_rds(dre_cleaner, "data/processed_data/clean/dre_clean.rds")
write_rds(bpp_cleaner, "data/processed_data/clean/bpp_clean.rds")
write_rds(bpa_cleaner, "data/processed_data/clean/bpa_clean.rds")