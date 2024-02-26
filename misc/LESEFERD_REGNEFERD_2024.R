# Special fix for LESEFERD and REGNEFERD 


# LESEFERD
# Read files
# Combining: LESEFERD_INNVKAT_2024-02-06-14-43.csv + LESEFERD_INNVKAT_2_2024-02-20-17-09.csv 
LESEFERD_INNVKAT <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/LESEFERD_INNVKAT_end2021_2024-02-26-09-32.csv")
LESEFERD_INNVKAT_2 <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/LESEFERD_INNVKAT_2_2024-02-26-09-36.csv")
leseferddatetag <- "_2024-02-26-09-36.csv"

# Add data on 2021_2022 + 2022_2023 from 2-year kube
leseferd_innvkat <- rbindlist(list(LESEFERD_INNVKAT,
                                   LESEFERD_INNVKAT_2[AAR %in% c("2021_2022", "2022_2023")]))

# Write to datert folder with same date-tag as LESEFERD_INNVKAT_2
fwrite(leseferd_innvkat, paste0("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/LESEFERD_INNVKAT", leseferddatetag), sep = ";")

# REGNEFERD
# Read files
# Combining: REGNEFERD_2024-02-06-14-46.csv + REGNEFERD_2_2024-02-20-17-11.csv
REGNEFERD <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/REGNEFERD_end2021_2024-02-26-09-35.csv")
REGNEFERD_2 <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/REGNEFERD_2_2024-02-26-09-39.csv")
regneferddatetag <- "_2024-02-26-09-39.csv"

# Add data on 2021_2022 + 2022_2023 from 2-year kube
regneferd <- rbindlist(list(REGNEFERD,
                            REGNEFERD_2[AAR %in% c("2021_2022", "2022_2023")]))

# Write to datert folder with same date-tag as REGNEFERD_2
fwrite(regneferd, paste0("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/REGNEFERD", regneferddatetag), sep = ";")
