url <- "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_12r5.px"
query <- list("Alue"=c("*"),
       "Kuukausi"=c("*"),
       "Tiedot"=c("TYOTTOMATLOPUSSA","TYOVOIMATK","AVPAIKATLOPUSSA"))

tyonv_12r5 <- as.data.frame(pxweb::pxweb_get(url, query))
tyonv_12r5 <- statfitools::clean_times2(tyonv_12r5)
tyonv_12r5$Alue <- statficlassifications::names_to_codes(tyonv_12r5$Alue)
