
data(taulut)

kohtaantoindeksit_db <- taulut$table_location
pttdatahaku::ptt_create_db_list(kohtaantoindeksit_db, call = "ptt_get_statfi_robonomist(url)", overwrite = TRUE)

pttdatahaku::ptt_db_update("kohtaantoindeksit_db")

for(i in 1:dim(taulut)[1]) {

  url <- paste0("https://pxnet2.stat.fi/PXWeb/api/v1/fi/", taulut$table_location[i])
  data_name <- sapply(url,
                      function(x) {paste(stringr::str_match(x, "statfin_\\s*(.*?)\\s*pxt_\\s*(.*?)\\s*.px")[,2:3], collapse = "")},
                      USE.NAMES = FALSE)

  data <- pttdatahaku::ptt_read_data(data_name)

  assign(data_name, data)
  do.call("use_data", list(as.name(data_name), overwrite = TRUE))

}
