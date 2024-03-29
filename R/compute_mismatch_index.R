#' Compute mismatch index
#'
#' @param data
#' @param as
#'
#' @return
#' @export
#'
#' @examples
compute_mismatch_index <- function(data, as, by = NULL) {

mismatch <- data.frame()

for(a in as){

  mismatch_temp <- data |>
                   dplyr::group_by(time, {{by}}) |>
                   dplyr::summarize(mismatch = 1 - sum((AVPAIKATLOPUSSA/kokomaa_AVPAIKATLOPUSSA)^a * (TYOTTOMATLOPUSSA/kokomaa_TYOTTOMATLOPUSSA)^(1-a)),
                                    mismatch_abs = mean(kokomaa_TYOTTOMATLOPUSSA*kokomaa_AVPAIKATLOPUSSA)^a - sum((TYOTTOMATLOPUSSA*AVPAIKATLOPUSSA)^a)) |>
                   dplyr::ungroup() |>
                   dplyr::group_by({{by}}) |>
                   dplyr::mutate(mismatch_sa = statfitools::sa_series(mismatch, time),
                                 mismatch_trend = statfitools::trend_series(mismatch, time),
                                 mismatch_loess = statfitools::loess_series(mismatch, time, span = 0.5),
                                 mismatch_abs_sa = statfitools::sa_series(mismatch_abs, time),
                                 mismatch_abs_trend = statfitools::trend_series(mismatch_abs, time),
                                 mismatch_abs_loess = statfitools::loess_series(mismatch_abs, time, span = 0.5)) |>
                    tidyr::gather(tiedot, value, -time, -{{by}}) |>
                   dplyr::ungroup()
  mismatch_temp$a <- a
  mismatch <- rbind(mismatch, mismatch_temp)
  print(a); Sys.sleep(0.01)
}
 mismatch
}
