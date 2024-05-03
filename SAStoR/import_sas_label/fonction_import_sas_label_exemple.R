source("https://raw.githubusercontent.com/arnomuller/Fonction_R/main/SAStoR/import_sas_label/fonction_import_sas_label.R")

erfi <- import_sas_label(data_file = "erfi_extrait2.sas7bdat", 
                         catalog_file = "formats.sas7bcat",
                         label_manquant = 5,
                         blanc_as_NA = TRUE)
table(erfi$MB_STOC)
table(erfi$MA_AGER)