library(haven)
library(dplyr)

erfi <- read_sas("erfi_extrait2.sas7bdat", catalog_file = "formats.sas7bcat")
str(erfi$MA_AGER)

erfi <- create_var_label(erfi, "MA_AGER")