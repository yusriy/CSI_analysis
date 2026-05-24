library(httr2)
library(readr)
source("download_tidbrepo.R") # Source the download function first

db_id <- "306d4236-97b2-4a17-a82d-957ac987afac"
tb_id <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" # <----- Change this to the actual data table ID.

df <- download_tidbrepo(db_id = db_id, tb_id = tb_id)
