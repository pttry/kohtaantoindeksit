data(taulut)

for(i in 1:dim(taulut)[1]) {

  url <- statfitools::statfi_parse_url(taulut$table_location[i])
  query <- taulut$query[i][[1]]

  data <- pttdatahaku::ptt_get_statfi_robonomist(url, query = query)
  data_name <- statfitools::url_to_table_code(url)

  assign(data_name, data)
  do.call("use_data", list(as.name(data_name), overwrite = TRUE))

}
