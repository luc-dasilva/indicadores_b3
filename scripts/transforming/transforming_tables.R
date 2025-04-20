
# Load Packages ----------------------------------------------------------

library(tidyverse)

# Importing --------------------------------------------------------------

dre <- read_rds("data/processed_data/dre.rds")
bpa <- read_rds("data/processed_data/clean/bpa_clean.rds")
bpp <- read_rds("data/processed_data/clean/bpp_clean.rds")

# Tranforming ------------------------------------------------------------

bpa_tib <- bpa |> 
  select(!cd_cvm) |> 
  mutate(
    var_20_21 = ifelse(x2020 == 0, 0, (x2021 - x2020) / abs(x2020) * 100),
    var_21_22 = ifelse(x2021 == 0, 0, (x2022 - x2021) / abs(x2021) * 100),
    var_22_23 = ifelse(x2022 == 0, 0, (x2023 - x2022) / abs(x2022) * 100),
    var_23_24 = ifelse(x2023 == 0, 0, (x2024 - x2023) / abs(x2023) * 100),
    indent_level = str_length(as.character(cd_conta)) * 2
  ) |> 
  select(
    nome_sa, cd_conta, ds_conta, x2020, x2021, var_20_21, x2022, var_21_22,
    x2023, var_22_23, x2024, var_23_24, indent_level
  )

bpp_tib <- bpp |>  
  select(!cd_cvm) |> 
  mutate(
    var_20_21 = ifelse(x2020 == 0, 0, (x2021 - x2020) / abs(x2020) * 100),
    var_21_22 = ifelse(x2021 == 0, 0, (x2022 - x2021) / abs(x2021) * 100),
    var_22_23 = ifelse(x2022 == 0, 0, (x2023 - x2022) / abs(x2022) * 100),
    var_23_24 = ifelse(x2023 == 0, 0, (x2024 - x2023) / abs(x2023) * 100),
    indent_level = str_length(as.character(cd_conta)) * 2
  ) |> 
  select(
    nome_sa, cd_conta, ds_conta, x2020, x2021, var_20_21, x2022, var_21_22,
    x2023, var_22_23, x2024, var_23_24, indent_level
  )

dre_tib <- dre |> 
  select(!cd_cvm) |> 
  mutate(
    var_20_21 = ifelse(x2020 == 0, 0, (x2021 - x2020) / abs(x2020) * 100),
    var_21_22 = ifelse(x2021 == 0, 0, (x2022 - x2021) / abs(x2021) * 100),
    var_22_23 = ifelse(x2022 == 0, 0, (x2023 - x2022) / abs(x2022) * 100),
    var_23_24 = ifelse(x2023 == 0, 0, (x2024 - x2023) / abs(x2023) * 100)
  ) |> 
  select(
    nome_sa, ds_conta, x2020, x2021, var_20_21, x2022, var_21_22,
    x2023, var_22_23, x2024, var_23_24
  )

# Exporting --------------------------------------------------------------

write_rds(bpa_tib, "data/processed_data/tables/bpa_tib.rds")
write_rds(bpp_tib, "data/processed_data/tables/bpp_tib.rds")
write_rds(dre_tib, "data/processed_data/tables/dre_tib.rds")
