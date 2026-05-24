library(httr2)
library(readr)
source("R/download_tidbrepo.R")

db_id <- "306d4236-97b2-4a17-a82d-957ac987afac"
tb_id <- "88fed7ba-6a54-4e06-9dad-0599e3938cb9" # Honey 1

df <- download_tidbrepo(db_id = db_id, tb_id = tb_id)