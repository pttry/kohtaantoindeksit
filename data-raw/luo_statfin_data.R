data(taulut)

for(i in 1:dim(taulut)[1]) {

  url <- paste0("https://pxnet2.stat.fi/PXWeb/api/v1/fi/", taulut$table_location[i])
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

  data_name <- sapply(url,
                      function(x) {paste(stringr::str_match(x, "statfin_\\s*(.*?)\\s*pxt_\\s*(.*?)\\s*.px")[,2:3], collapse = "")},
                      USE.NAMES = FALSE)

  assign(data_name, data)
  do.call("use_data", list(as.name(data_name), overwrite = TRUE))

}


