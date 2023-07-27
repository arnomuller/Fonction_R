erfi <- import_sas_label(data_file = "erfi_extrait2.sas7bdat", 
                         catalog_file = "formats.sas7bcat",
                         blanc_as_NA = FALSE)
table(erfi$MB_STOC)
table(erfi$MA_AGER)






