kohtaantoindeksit_db <- c("https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyonv__kk/statfin_tyonv_pxt_12r5.px/",
                          "https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyonv__kk/statfin_tyonv_pxt_12ti.px/",
                          "https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__atp__nj/statfin_atp_pxt_11n1.px/",
                          "https://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti__nj/statfin_tyti_pxt_137i.px/")
pttdatahaku::ptt_create_db_list(kohtaantoindeksit_db, call = "ptt_get_statfi_robonomist(url)", overwrite = TRUE)

pttdatahaku::ptt_db_update("kohtaantoindeksit_db")
