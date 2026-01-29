# 1. Load the raw Excel files
MaissaLajimi <- readxl::read_excel("data-raw/MaissaLajimi.xlsx")
ElecTrain    <- readxl::read_excel("data-raw/Elec-train.xlsx")

# 2. Fix the timestamp using the original Excel name (Timestamp)
ElecTrain$Timestamp[1] <- "1/1/2010 1:15"

# 3. Now rename the columns to lowercase
colnames(ElecTrain) <- c("timestamp", "power", "temp")

# 4. Save the clean objects into the /data/ folder
usethis::use_data(MaissaLajimi, overwrite = TRUE)
usethis::use_data(ElecTrain, overwrite = TRUE)
