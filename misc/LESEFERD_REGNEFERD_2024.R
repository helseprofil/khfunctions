# Special fix for LESEFERD and REGNEFERD 


# LESEFERD
# Read files
# Combining: LESEFERD_INNVKAT_2024-02-06-14-43.csv + LESEFERD_INNVKAT_2_2024-02-20-17-09.csv 
LESEFERD_INNVKAT <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/LESEFERD_INNVKAT_2024-02-06-14-43.csv")
LESEFERD_INNVKAT_2 <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/LESEFERD_INNVKAT_2_2024-02-20-17-09.csv")

# Delete data for periods containing both 2021 and 2022
LESEFERD_INNVKAT[AAR %in% c("2020_2022", "2021_2023"),
                 let(RATE = NA_real_,
                     SMR = NA_real_,
                     MEIS = NA_real_,
                     SPVFLAGG = 2)]

# Add data on 2022_2023 from 2-year kube
leseferd_innvkat <- rbindlist(list(LESEFERD_INNVKAT,
                                   LESEFERD_INNVKAT_2[AAR == "2022_2023"]))

# Write to datert folder with manual date tag
fwrite(leseferd_innvkat, "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/LESEFERD_INNVKAT_2024-02-23-15-20.csv", sep = ";")

# REGNEFERD
# Read files
# Combining: REGNEFERD_2024-02-06-14-46.csv + REGNEFERD_2_2024-02-20-17-11.csv
REGNEFERD <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/REGNEFERD_2024-02-06-14-46.csv")
REGNEFERD_2 <- fread("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/REGNEFERD_2_2024-02-20-17-11.csv")

# Delete data for periods containing both 2021 and 2022
REGNEFERD[AAR %in% c("2020_2022", "2021_2023"),
          let(RATE = NA_real_,
              SMR = NA_real_,
              MEIS = NA_real_,
              SPVFLAGG = 2)]

# Add data on 2022_2023 from 2-year kube
regneferd <- rbindlist(list(REGNEFERD,
                         REGNEFERD_2[AAR == "2022_2023"]))

# Write to datert folder with manual date tag
fwrite(regneferd, "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/REGNEFERD_2024-02-23-15-25.csv", sep = ";")
