data(taulut)

for(i in 1:dim(taulut)[1]) {

  url <- statfitools::statfi_parse_url(taulut$table_location[i])
  query <- taulut$query[i][[1]]

  px_data <- pxweb::pxweb_get(url = url, query = query)
  data <- as.data.frame(px_data,
                        column.name.type = "code",
                        variable.value.type = "code") |>
    statfitools::clean_times2() |>
    dplyr::select(-all_of(names(query)[query == "SSS"])) |>
    tidyr::pivot_longer(where(is.numeric)) |>
    dplyr::rename(tiedot = name) |>
    statfitools::clean_names()

  data_name <- statfitools::url_to_table_code(url)

  assign(data_name, data)
  do.call("use_data", list(as.name(data_name), overwrite = TRUE))

}


