require(Microsoft365R)
get_business_onedrive() #I ran this first and the browser logged me in
personal_drive <- get_business_onedrive()

t <- personal_drive$list_shared_files()
t$`2E_RPOE_DNA_extraction.xlsx`$download(dest = "../language/data/derivatives/PS_VC-participants-metadata2.csv", 
                                         overwrite = T)
