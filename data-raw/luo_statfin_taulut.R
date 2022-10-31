taulut <- tibble::tribble(
  ~table_location,                                      ~query,
  "StatFin/tyonv/statfin_tyonv_pxt_12r5.px/",           list("Alue"=c("*"), "Kuukausi"=c("*"), "Tiedot"=c("TYOTTOMATLOPUSSA","TYOVOIMATK","AVPAIKATLOPUSSA")),
  "StatFin/tyonv/statfin_tyonv_pxt_12ti.px/",           list("Alue"=c("SSS","MK01","MK02","MK04","MK05","MK06","MK07","MK08","MK09","MK10","MK11", "MK12","MK13","MK14","MK15","MK16","MK17","MK18","MK19","MK21"), "Kuukausi"=c("*"), "Ammattiryhmä" = c("*"), "Tiedot"=c("TYOTTOMATLOPUSSA","AVPAIKATLOPUSSA")),
  "StatFin/atp/statfin_atp_pxt_11n1.px/",               list("Suuralue"=c("SSS","SA1","SA2","SA3","SA4"), "Vuosineljännes"=c("*"),"Tiedot"=c("atp_lkm")),
  "StatFin/tyti/statfin_tyti_pxt_137i.px/",             list("Vuosineljännes"=c("*"),"Sukupuoli"=c("SSS"), "Suuralue 2012"=c("SSS","SA1","SA2","SA3","SA4"), "Tiedot"=c("Tyottomat")),
)

usethis::use_data(taulut, overwrite = TRUE)


# taulut <- tibble::tribble(
#   ~table_location,                                      ~query,
#   "StatFin/tym/tyonv/kk/statfin_tyonv_pxt_12r5.px/",    list("Alue"=c("*"), "Kuukausi"=c("*"), "Tiedot"=c("TYOTTOMATLOPUSSA","TYOVOIMATK","AVPAIKATLOPUSSA")),
#   "StatFin/tym/tyonv/kk/statfin_tyonv_pxt_12ti.px/",    list("Alue"=c("SSS","MK01","MK02","MK04","MK05","MK06","MK07","MK08","MK09","MK10","MK11", "MK12","MK13","MK14","MK15","MK16","MK17","MK18","MK19","MK21"), "Kuukausi"=c("*"), "Ammattiryhmä" = c("*"), "Tiedot"=c("TYOTTOMATLOPUSSA","AVPAIKATLOPUSSA")),
#   "StatFin/tym/atp/nj/statfin_atp_pxt_11n1.px/",        list("Suuralue"=c("SSS","SA1","SA2","SA3","SA4"), "Vuosineljännes"=c("*"),"Tiedot"=c("atp_lkm")),
#   "StatFin/tym/tyti/nj/statfin_tyti_pxt_137i.px/",      list("Vuosineljännes"=c("*"),"Sukupuoli"=c("SSS"), "Suuralue 2012"=c("SSS","SA1","SA2","SA3","SA4"), "Tiedot"=c("Tyottomat")),
# )
