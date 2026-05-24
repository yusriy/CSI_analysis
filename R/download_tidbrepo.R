download_tidbrepo <- 
  function(db_id, tb_id, base_url = "https://tidbrepo.usm.my/api/v1/database/") {
    
    url <- paste0(base_url, db_id, "/table/", tb_id, "/data")
    
    resp <- request(url) |> req_headers(Accept = "text/csv") |> req_perform()
    
    csv_text <- resp_body_string(resp)
    
    df <- read_csv(
      I(csv_text),
      show_col_types = FALSE
    )
  }