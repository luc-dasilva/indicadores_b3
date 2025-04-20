
# Load Packages ----------------------------------------------------------

library(tidyverse)

# Importing --------------------------------------------------------------

dre_itr <- read_rds("data/processed_data/clean/dre_itr_clean.rds")
dre_dfp <- read_rds("data/processed_data/clean/dre_clean.rds")

# Tranforming ------------------------------------------------------------

dre <- dre_itr |> 
  left_join(
    dre_dfp,
    by = join_by(cd_cvm, ds_conta, nome_sa, setor)
  ) |> 
  filter(if_all(starts_with("x"), ~ !is.na(.x))) |> 
  mutate(
    x4T23 = (x1T23 + x2T23 + x3T23) * -1 + x2023,
    x4T24 = (x1T24 + x2T24 + x3T24) * -1 + x2024
  ) |> 
  relocate(
    ends_with("T23"), x2023,
    ends_with("T24"), x2024,
    .after = ds_conta
  )

# Exporting --------------------------------------------------------------

write_rds(dre, "data/processed_data/dre.rds")

