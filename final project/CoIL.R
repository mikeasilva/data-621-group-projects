# CoIL Challenge Source Code
library(tidyverse)
# Download the data sets from UCI if they are not present
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/tic-mld/"
files <- c("ticdata2000.txt", "ticeval2000.txt", "tictgts2000.txt")
for (file_name in files) {
  file_path <- paste0("data/", file_name)
  file_url <- paste0(url, file_name)
  if (!file.exists(file_path)) {
    message(paste("Downloading", file_name))
    download.file(file_url, file_path)
  }
}
# Read in the data
df <- read.delim("data/ticdata2000.txt", header = FALSE)
names(df) <- c("MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF", "MOSHOOFD",
               "MGODRK", "MGODPR", "MGODOV", "MGODGE", "MRELGE", "MRELSA",
               "MRELOV", "MFALLEEN", "MFGEKIND", "MFWEKIND", "MOPLHOOG",
               "MOPLMIDD", "MOPLLAAG", "MBERHOOG", "MBERZELF", "MBERBOER",
               "MBERMIDD", "MBERARBG", "MBERARBO", "MSKA", "MSKB1", "MSKB2",
               "MSKC", "MSKD", "MHHUUR", "MHKOOP", "MAUT1", "MAUT2", "MAUT0",
               "MZFONDS", "MZPART", "MINKM30", "MINK3045", "MINK4575",
               "MINK7512", "MINK123M", "MINKGEM", "MKOOPKLA", "PWAPART",
               "PWABEDR", "PWALAND", "PPERSAUT", "PBESAUT", "PMOTSCO",
               "PVRAAUT",  "PAANHANG", "PTRACTOR", "PWERKT", "PBROM",
               "PLEVEN", "PPERSONG", "PGEZONG", "PWAOREG", "PBRAND", 
               "PZEILPL", "PPLEZIER", "PFIETS", "PINBOED", "PBYSTAND",
               "AWAPART", "AWABEDR", "AWALAND", "APERSAUT", "ABESAUT",
               "AMOTSCO", "AVRAAUT", "AAANHANG", "ATRACTOR", "AWERKT",
               "ABROM", "ALEVEN", "APERSONG", "AGEZONG", "AWAOREG",
               "ABRAND", "AZEILPL", "APLEZIER", "AFIETS", "AINBOED",
               "ABYSTAND", "CARAVAN")
eval <- read.delim("data/ticeval2000.txt", header = FALSE)
temp <- read.delim("data/tictgts2000.txt", header = FALSE)
eval$CARAVAN <- temp$V1
names(eval) <- names(df)
