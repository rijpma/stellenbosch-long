library("data.table")
library("xgboost")
library("Metrics")

stel_new = fread("~/repos/capelinker/data_raw/stellenbosch_long_trainready.csv")
x_eval = stel_new[train == FALSE]
x_eval[, `(Intercept)` := 1]

load(file = "~/repos/capelinker/data/stellenbosch_long_noyear.rda")

xgb = xgboost::xgb.DMatrix(
    as.matrix(x_eval[, .SD, .SDcols = m$feature_names]))
x_eval[, predicted := predict(m, xgb)]
Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
Metrics::fbeta_score(x_eval$correct, x_eval$predicted > 0.5)

table(x_eval$correct, x_eval$predicted > 0.5) |> knitr::kable()