## train on stellenbosch new only
rm(list = ls())

# library("remotes")
# remotes::install_github("rijpma/capelinker")

library('data.table')
# setDTthreads(1)
library("xgboost")
library("stringi")
library("capelinker")
library("stringdist")

# opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024oct18.csv")
opg = fread("~/data/cape/opg/stellenbosch_long_train_cleaned_auke_2024nov1.csv")
opg[opg == ""] = NA

# ground truth training data
gt = fread("~/data/cape/opg/GT_Training_data_030624.csv", encoding = "Latin-1")
# keep ids and lniks only
gt = gt[, list(persid_from, year_from, persid_to, year_to, Match, coder)]

gt[persid_from == 130222 & persid_to ==  128895, Match := 0] # unlink add to the notes
gt[persid_from == 112599 & persid_to ==  61283, Match := 1] # link add to the notes

# merge with opg
stel_new = merge(
    gt,
    opg[, list(persid_from = persid, 
               names_men_from = names_men,
               names_men_clean_from = names_men_clean,
               names_women_from = names_women,
               names_women_clean_from = names_women_clean,
               add_info_from = add_info,
               mlast_from = mlast,
               mfirst_from = mfirst,
               wlast_from = wlast,
               wfirst_from = wfirst,
               minitials_from = minitials,
               winitials_from = winitials,
               # closest_couple_from = closest_couple, 
               # closest_couple_osa_from = closest_couple_osa, 
               # closest_couple_full_from = closest_couple_full, 
               closest_couple_full_osa_from = closest_couple_full_osa, 
               closest_man_from = closest_man,
               # closest_man_osa_from = closest_man_osa,
               settler_children_from = settler_children,
               old_from = old,
               young_from = young,
               widow_from = widow,
               widow_of_from = widow_of,
               wid_of_last_name_from = wid_of_last_name,
               wid_of_first_name_from = wid_of_first_name,
               son_of_init_from = son_of_init,
               son_of_dummy_from = son_of_dummy
           )],
    by = "persid_from",
    all.x = TRUE,
    all.y = FALSE
)
stel_new = merge(
    stel_new,
    opg[, list(persid_to = persid, 
               names_men_to = names_men,
               names_men_clean_to = names_men_clean,
               names_women_to = names_women,
               names_women_clean_to = names_women_clean,
               add_info_to = add_info,
               mlast_to = mlast,
               mfirst_to = mfirst,
               wlast_to = wlast,
               wfirst_to = wfirst,
               minitials_to = minitials,
               winitials_to = winitials,
               # closest_couple_to = closest_couple, 
               # closest_couple_osa_to = closest_couple_osa, 
               # closest_couple_full_to = closest_couple_full, 
               closest_couple_full_osa_to = closest_couple_full_osa, 
               closest_man_to = closest_man,
               # closest_man_osa_to = closest_man_osa,
               settler_children_to = settler_children,
               old_to = old,
               young_to = young,
               widow_to = widow,
               widow_of_to = widow_of,
               wid_of_last_name_to = wid_of_last_name,
               wid_of_first_name_to = wid_of_first_name,
               son_of_init_to = son_of_init,
               son_of_dummy_to = son_of_dummy
           )],
    by = "persid_to",
    all.x = TRUE,
    all.y = FALSE
)

stel_new[, mlastdist := stringdist::stringdist(mlast_from, mlast_to, method = "jw")]
stel_new[, wlastdist := stringdist::stringdist(wlast_from, wlast_to, method = "jw")]
stel_new[, mfirstdist := stringdist::stringdist(mfirst_from, mfirst_to, method = "jw")]
stel_new[, wfirstdist := stringdist::stringdist(wfirst_from, wfirst_to, method = "jw")]
stel_new[, minitialsdist_osa := stringdist::stringdist(minitials_from, minitials_to, method = "osa")]
stel_new[, winitialsdist_osa := stringdist::stringdist(winitials_from, winitials_to, method = "osa")]
stel_new[, nextmfirst := capelinker::stringdist_closest(mfirst_to), by = list(persid_from, year_to)]

stel_new[, wifepresent_from := as.numeric(!is.na(wlast_from))]
stel_new[, wifepresent_to := as.numeric(!is.na(wlast_to))]
stel_new[, wifepresent_both := wifepresent_from * wifepresent_to]

stel_new[, husband_present_from :=as.numeric(!is.na(names_men_clean_from))]
stel_new[, husband_present_to :=as.numeric(!is.na(names_men_clean_to))]

stel_new[, N := .N, by = list(persid_from, year_to)]
stel_new[, logN := log(N)]

stel_new[, young_dist := young_from - young_to]
stel_new[, old_dist := old_from - old_to]
stel_new[, widow_dist := widow_from - widow_to]
stel_new[, son_dummy_dist := son_of_dummy_from - son_of_dummy_to]

stel_new[ ,settler_children_dist := settler_children_from - settler_children_to]
stel_new[ ,settler_children_gk := gk(settler_children_from, settler_children_to)]
stel_new[, settler_children_lgt := exp(settler_children_dist) / (1 + exp(settler_children_dist))]

stel_new[, wid_of_full_name_from := paste(wid_of_first_name_from, wid_of_last_name_from)]
stel_new[, wid_of_full_name_to := paste(wid_of_first_name_to, wid_of_last_name_to)]

stel_new[, son_of_init_dist := stringdist(son_of_init_from, son_of_init_to, method = "osa")]
stel_new[, widow_first_name_dist := stringdist(wid_of_first_name_from, wid_of_first_name_to, method = "jw")]
stel_new[, widow_last_name_dist := stringdist(wid_of_last_name_from, wid_of_last_name_to, method = "jw")]
stel_new[, widow_full_name_dist := stringdist(wid_of_full_name_from, wid_of_full_name_to, method = "jw")]
stel_new[, widow2husb_dist := stringdist(wid_of_last_name_from, mlast_to, method = "jw")]
# mlast_to is earlier, husband while still alive

# don't use wid_of_firstname, wid_of_lastname, wid_of_voor, wid_of_initials
# careful 
# widow2wife
stel_new[, yeardist := year_from - year_to]
stel_new[, yeardist_norm := (yeardist) / (diff(range(yeardist)) + 1)]
stel_new[, yeardist_gk := gk(year_from, year_to)]
stel_new[, log_yeardist := log(year_from - year_to)]
stel_new[, yeardist_lgt := exp(yeardist) / (1 + exp(yeardist))]
stel_new[, y20_from := year_from - (year_from %% 20)]
stel_new[, y25_from := year_from - (year_from %% 25)]
stel_new[, decade_from := year_from - (year_from %% 10)]

stel_new[, correct := Match]
# stel_new[, predicted := predict(m, xgbm_ff(.SD, f, labelled = FALSE))]

stel_new[, sum(correct), by = y20_from]

share_train = 0.8
set.seed(123)
stel_new[, train := persid_from %in% sample(
  unique(persid_from),
  ceiling(length(unique(persid_from)) * share_train))]
stel_new[, uniqueN(persid_from), by = list(train)]
stel_new[, sum(correct), by = list(train)]

fwrite(stel_new, "~/repos/capelinker/data_raw/stellenbosch_long_trainready.csv")

x_train = stel_new[train == TRUE]
x_eval = stel_new[train == FALSE]

# far more kids, but still feels high.
# plot(opg[, mean(is.na(settler_children)), by = year])
# plot(opg[!is.na(wlast) & !is.na(mlast), mean(is.na(settler_children)), by = year])

vrbs = c(
    "mlastdist",
    "mfirstdist",
    "minitialsdist_osa",
    "wlastdist",
    "wfirstdist",
    "winitialsdist_osa",
    # "nextmfirst",
    "wifepresent_from",
    "wifepresent_to",
    # "wifepresent_both", # seems to deteriorate
    "log_yeardist",
    # "yeardist_gk",
    # "yeardist_norm",
    # "yeardist_lgt",
    # "year_from",
    "year_to",
    "logN",
    # "closest_couple_osa_from",
    # "closest_couple_osa_to",
    # "closest_man_osa_from",
    # "closest_man_osa_to",
    # "closest_couple_from",
    # "closest_couple_to",
    # "closest_couple_full_from",
    # "closest_couple_full_to",
    "closest_couple_full_osa_from",
    "closest_couple_full_osa_to",
    "closest_man_from",
    "closest_man_to",
    # "settler_children_dist",
    "settler_children_gk",
    # "settler_children_lgt",
    "old_dist",
    "young_dist",
    "widow_dist",
    # "son_dummy_dist",
    "son_of_init_dist",
    "widow_first_name_dist",
    "widow_last_name_dist",
    "widow2husb_dist",
    "widow_full_name_dist",
    # "husband_present_from",
    # "husband_present_to",
    NULL
)





vrbs = c(
    "mlastdist",
    "mfirstdist",
    "minitialsdist_osa",
    "wlastdist",
    "wfirstdist",
    "winitialsdist_osa",
    "nextmfirst",
    # "wifepresent_from",
    # "wifepresent_to",
    # "wifepresent_both", # seems to deteriorate
    "log_yeardist",
    # "yeardist_gk",
    # "yeardist_norm",
    # "yeardist_lgt",
    # "year_from",
    # "y25_from",
    # "year_to",
    "logN",
    # "closest_couple_osa_from",
    # "closest_couple_osa_to",
    # "closest_man_osa_from",
    # "closest_man_osa_to",
    # "closest_couple_from",
    # "closest_couple_to",
    "closest_couple_full_from",
    "closest_couple_full_to",
    # "closest_couple_full_osa_from",
    # "closest_couple_full_osa_to",
    # "closest_man_from",
    # "closest_man_to",
    # "settler_children_dist",
    # "settler_children_lgt",
    # "settler_children_gk",
    # "old_dist",
    # "young_dist",
    # "widow_dist",
    # "son_dummy_dist",
    # "son_of_init_dist",
    # "widow_first_name_dist",
    # "widow_last_name_dist",
    # "widow2husb_dist",
    # "widow_full_name_dist",
    # "husband_present_from",
    # "husband_present_to",
    NULL
)

# couple_from_full_osa trimmed
vrbs = c("mlastdist",
    "mfirstdist",
    "minitialsdist_osa",
    "wlastdist",
    "wfirstdist",
    "winitialsdist_osa",
    "wifepresent_from",
    "wifepresent_to",
    "log_yeardist",
    "logN",
    "closest_couple_full_osa_from",
    "closest_couple_full_osa_to",
    "closest_man_from",
    "closest_man_to",
    # "old_dist",
    "young_dist",
    "widow_dist",
    "son_of_init_dist",
    "widow_first_name_dist")


# couple_from_full_osa
vrbs = c(
    "mlastdist",
    "mfirstdist", 
    "minitialsdist_osa", 
    "wlastdist", 
    "wfirstdist", 
    "winitialsdist_osa", 
    "wifepresent_from", 
    "wifepresent_to", 
    "log_yeardist", 
    "logN", 
    "closest_couple_full_osa_from", 
    "closest_couple_full_osa_to", 
    "closest_man_from", 
    "closest_man_to", 
    "old_dist", 
    "young_dist", 
    "widow_dist", 
    "son_of_init_dist", 
    "widow_first_name_dist", 
    "widow_last_name_dist", 
    # "widow2husb_dist", 
    # "widow_full_name_dist",
    NULL)

modlist = list()
startvariable = 1
for (i in startvariable:length(vrbs)){
    print(vrbs[1:i])
    cat("\n", rep("-", 40), "\n")
    f = paste(vrbs[1:i], collapse = "+")
    f = formula(paste("correct ~", f))

    m = xgboost::xgb.train(
        data = capelinker::xgbm_ff(x_train, f),
        nrounds = 200,
        watchlist = list(train = xgbm_ff(x_train, f), 
                          eval = xgbm_ff(x_eval, f)),
        verbose = 0,
        params = list(
            max_depth = 6,        # default 6
            min_child_weight = 1, # default 1 larger is more consevative
            gamma = 1,            # default 0, larger is more conservative
            eta = 0.3,            # default 0.3 lower for less overfitting
            max_delta_step = 0,   # deafult 0, useful for unbalanced, higher is more conservative
            subsample = 0.8,        # default 1 lower is less overfitting
            colsample_bytree = 0.5, # default 1 
            objective = "binary:logistic"
        )
    )
    modlist[[i]] = m
}
ell = lapply(modlist, \(x) x$evaluation_log$eval_logloss)
dell = as.data.table(ell)
colnames(dell) = vrbs[startvariable:length(vrbs)]

x_eval[, `(Intercept)` := 1]

perflist = list()
for (i in startvariable:length(modlist)){
    m = modlist[[i]]
    xgb = xgboost::xgb.DMatrix(
        as.matrix(x_eval[, .SD, .SDcols = m$feature_names]))
    x_eval[, predicted := predict(m, xgb)]
    prec = Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
    rec = Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
    f1b = Metrics::fbeta_score(x_eval$correct, x_eval$predicted > 0.5)
    perflist[[i]] = c(prec, rec, f1b)
}
perf = do.call(rbind, perflist)
perf = data.table(vrb = vrbs[startvariable:length(vrbs)], perf)
perf


# m = modlist[[31 + startvariable - 1]] # used to be 21
m = modlist[[length(modlist)]] # used to be 21


save(m, 
    file = "~/repos/capelinker/data/stellenbosch_long_noyear.rda", 
    version = 2)



i = xgboost::xgb.importance(model = m)

setdiff(vrbs, i$Feature) |> cat(sep = "\n")
# doesn't even make the importance: 

matplot(xgboost::xgb.importance(model = m)[, -"Feature"], type = "b")
abline(h = 0.01)

# the good run from previous work 84-86
# that's 77
tail(perf, 15) |> knitr::kable(digits = 2)
tail(perf, 10)[, list(mean(V1), mean(V2), mean(V3))]
perf[order(V3)] |> knitr::kable(digits = 2)
# in principle we could also drop the widow info, but let's run with this now


# super minimalist: just the standard distances and closest couples

# super aggr, everything below mlast/minitialsdist
# that gets you to 83

# dropping
# old_dist
# widow_first_name_dist
# 87 still possible


# 88-87 still possibple


# dropping
# we now have a few 88 model, 86 still easily possible
# wifepresent_both yeardist_gk yeardist_norm yeardist_lgt year_from y25_from closest_couple_osa_from closest_couple_osa_to closest_man_osa_from closest_man_osa_to closest_couple_from

closest_couple_to
# basic model: 85-86


# dropping super aggressively, keep only the best 10
# mfirstdist wlastdist minitialsdist_osa year_from log_yeardist wfirstdist closest_man_from nextmfirst year_to logN closest_man_to closest_couple_full_osa_to closest_couple_full_osa_from
 # this drops us to 80

# m = modlist[[which(vrbs == "son_of_init_dist")]]
# # xgb.save(m, fname = "~/repos/capelinker/data/stellenbosch_long.bin")
# # saveRDS(m, "~/repos/capelinker/data/stellenbosch_long.rds")
# save(m, 
#     file = "~/repos/capelinker/data/stellenbosch_long.rda", 
#     version = 2)
# dropping aggresively
# widow_first_name_dist
# settler_children_gk
# son_of_init_dist
# here we really start to lose, 85 max, drops below 84 pretty quickly
# we actually also don't use closestX in one of those 81s


# dropped
# widow_last_name_dist
# 86-88 still possible

# dropped
# wifepresent_from


# dropped 
# olddist
# here the 88s dissappear, but 87 still very much possible

# dropped
# 88/86 still possible
# closest_couple_osa_from
# closest_couple_osa_to
# closest_man_osa_from
# closest_man_osa_to
# closest_couple_from
# closest_couple_to
# closest_couple_full_from
# closest_couple_full_to


# also dropped
# yeardist_gk
# yeardist_norm
# yeardist_lgt
# 87-88 still possible

# also dropped
# settler_children_dist
# settler_children_lgt
# 87 88 still possible
# |vrb                   |   V1|   V2|   V3|
# |:---------------------|----:|----:|----:|
# |settler_children_gk   | 0.91| 0.75| 0.82|
# |old_dist              | 0.95| 0.77| 0.85|
# |young_dist            | 0.93| 0.74| 0.82|
# |widow_dist            | 0.95| 0.75| 0.84|
# |son_dummy_dist        | 0.89| 0.75| 0.82|
# |son_of_init_dist      | 0.95| 0.77| 0.85|
# |widow_first_name_dist | 0.96| 0.77| 0.85|
# |widow_last_name_dist  | 0.94| 0.83| 0.88|
# |widow2husb_dist       | 0.93| 0.82| 0.87|
# |widow_full_name_dist  | 0.92| 0.81| 0.86|

# dropped 
# 86-87 still possible
# wifepresent_both
# husband_present_from, husband_present_to

# dropped
# husband_present_from, husband_present_to
# 87-88 still possible

# all variables
# 87 possible in this set
#           V1       V2        V3
#        <num>    <num>     <num>
# 1: 0.9358901 0.776569 0.8485529
#                       vrb        V1        V2        V3
#                    <char>     <num>     <num>     <num>
#  1:            young_dist 0.9200000 0.7698745 0.8382688
#  2:            widow_dist 0.9068627 0.7740586 0.8352144
#  3:        son_dummy_dist 0.9207921 0.7782427 0.8435374
#  4:      son_of_init_dist 0.9680851 0.7615063 0.8524590
#  5: widow_first_name_dist 0.9554455 0.8075314 0.8752834
#  6:  widow_last_name_dist 0.9329897 0.7573222 0.8360277
#  7:       widow2husb_dist 0.9462366 0.7364017 0.8282353
#  8:  widow_full_name_dist 0.9326923 0.8117155 0.8680089
#  9:  husband_present_from 0.9219512 0.7907950 0.8513514
# 10:    husband_present_to 0.9538462 0.7782427 0.8571429


fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_settlerkids_gk.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_settlerkids.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_couple_from_full_osa_year.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_couple_from_full_osa.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_couple_from_full.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_sondum.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_newbase.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_year_to.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_year.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_nonextmfirst.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_gtfix.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_yearlgt.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_yeargk.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_noN.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_sonNA.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_wifeboth.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_husbpresent.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_wifepresent.csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_osa", Sys.time(), ".csv"))
# fwrite(perf, paste0("~/repos/capelinker/out/stel_long_perf_base", Sys.time(), ".csv"))

toplot = perf[-c(1:4)]
par(mar = c(9,3,2,1))
matplot(toplot[, -1], type = "b")
grid()
axis(1, at = 1:nrow(toplot), labels = toplot$vrb, las = 2)

# xgboost::xgb.importance(model = modlist[[18]])[1:length(vrbs)]

sapply(stel_new, \(x) mean(x == "", na.rm = TRUE)) |> knitr::kable()

xgb = xgboost::xgb.DMatrix(
    as.matrix(x_eval[, .SD, .SDcols = m$feature_names]))
x_eval[, predicted := predict(m, xgb)]
Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
Metrics::fbeta_score(x_eval$correct, x_eval$predicted > 0.5)
table(x_eval$correct, x_eval$predicted > 0.5)

# %linked by yeardist
stel_new[, yeardist]
toplot = stel_new[, sum(correct) > 0, by = list(year_from, persid_from, year_to)][, mean(V1), by = year_from - year_to]
plot(toplot)

# prec/rec
x_eval[, ]
x_eval[, list(sum(correct), Metrics::precision(correct, predicted > 0.5)), by = round(yeardist, -1)][order(round)]
plot(x_eval[, Metrics::precision(correct, predicted > 0.5), by = round(yeardist, -1)], pch = 19)
plot(x_eval[, Metrics::recall(correct, predicted > 0.5), by = round(yeardist, -1)], pch = 19)
plot(x_eval[, Metrics::fbeta_score(correct, predicted > 0.5), by = round(yeardist, -1)], pch = 19)

x_eval[, dpredicted := predicted > 0.5]
x_eval[, fn := as.numeric((correct == 1) & (dpredicted == 0))]
x_eval[, fp := as.numeric((correct == 0) & (dpredicted == 1))]
x_eval[, nofn := all(fn == 0), by = persid_from]
x_eval[, nofp := all(fp == 0), by = persid_from]

prc = data.table(
    thresh = 0:100 / 100)
prc[, precision := sapply(thresh, \(x) Metrics::precision(x_eval$correct, x_eval$predicted > x))]
prc[, recall := sapply(thresh, \(x) Metrics::recall(x_eval$correct, x_eval$predicted > x))]
prc[, f1 := sapply(thresh, \(x) Metrics::fbeta_score(x_eval$correct, x_eval$predicted > x))]
plot(recall ~ precision, data = prc)
points(recall ~ precision, data = prc[thresh == 0.5], col = 2, pch = 19)


x_eval[, mean(predicted), by = fn]
x_eval[, range(predicted), by = fn]
x_eval[, quantile(predicted), by = fn]
# so the absolute lowest fn is 1e-7, and even that is exceptional
x_eval[predicted > 1e-6 & nofn == FALSE]


# mlast_|wlast_|mfirst_|wfirst_|minitials_|winitials_|son_of_init_|widow_of_|wid_of_first_name_|wid_of_first_name_|wid_of_full_name_|wid_of_last_name

fn = x_eval[nofn == FALSE & predicted > 1e-6][order(-year_from, persid_from, -year_to, -predicted),
    # .SD, .SDcols = patterns("year_")]
    list(year_from, persid_from, 
        predicted, correct, fn,
        add_info_from, names_men_clean_from, names_women_clean_from, 
        year_to, persid_to,
        add_info_to, names_men_clean_to, names_women_clean_to, 
        .SD,
        mlast_from, wlast_from, mfirst_from, wfirst_from, minitials_from, winitials_from, son_of_init_from, widow_of_from, wid_of_first_name_from, wid_of_first_name_from, wid_of_full_name_from, wid_of_last_name_from,
        mlast_to, wlast_to, mfirst_to, wfirst_to, minitials_to, winitials_to, son_of_init_to, widow_of_to, wid_of_first_name_to, wid_of_first_name_to, wid_of_full_name_to, mlast_to,
        NULL), .SDcols = vrbs[1:21]]
fn = fn[, rbindlist(list(.SD, list(NA)), fill = TRUE), by = list(persid_from, year_to)]
fn[, persid_from := nafill(persid_from, "locf")]

fwrite(fn, "~/data/cape/opg/stellnbosch_long_falsenegatives_21.csv")

x_eval[, range(predicted), by = fp]

fp = x_eval[nofp == 0 & predicted > 1e-9][order(-year_from, persid_from, -year_to, -predicted),
    # .SD, .SDcols = patterns("year_")]
    list(year_from, persid_from, 
        predicted, correct, fp,
        add_info_from, names_men_clean_from, names_women_clean_from, 
        year_to, persid_to,
        add_info_to, names_men_clean_to, names_women_clean_to, 
        .SD,
        mlast_from, wlast_from, mfirst_from, wfirst_from, minitials_from, winitials_from, son_of_init_from, widow_of_from, wid_of_first_name_from, wid_of_first_name_from, wid_of_full_name_from, wid_of_last_name_from,
        mlast_to, wlast_to, mfirst_to, wfirst_to, minitials_to, winitials_to, son_of_init_to, widow_of_to, wid_of_first_name_to, wid_of_first_name_to, wid_of_full_name_to, mlast_to,
        NULL), .SDcols = vrbs[1:21]]
fp = fp[, rbindlist(list(.SD, list(NA)), fill = TRUE), by = list(persid_from, year_to)]
fp[, persid_from := nafill(persid_from, "locf")]

fwrite(fp, "~/data/cape/opg/stellnbosch_long_falsepositives_21.csv")

# yeardist
# closest couple to
# 133120 -- we're picking up data along the timeline, very difficult to encode in the data

xgboost::xgb.importance(model = m)


x_eval[correct == 1 & predicted < 0.5]
x_eval[correct == 1 & predicted < 0.5][order(persid_from), 
    list(
        names_men_clean_from, names_women_clean_from, year_from,
        names_men_clean_to, names_women_clean_to, year_to,
    NULL)]

stel_new[, allcorrect := uniqueN]

ell = lapply(modlist, \(x) x$evaluation_log$eval_logloss)
xgboost::xgb.DMatrix(as.matrix(x_eval[, .SD, .SDcols = vrbs[1:15]]))


x_eval[, predicted := predict(m, xgbm_ff(.SD, f))]

plot(m$evaluation_log$eval_logloss)



# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.6363636
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.6054054

# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.631016
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.6378378

# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9010989
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.6212121

# widow dist
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9368421
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7447699

# same
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9166667
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7824268

# no widow2widow or widow2husb name dist
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9538462
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7782427

# baseline 91/77

# widow_name_dist & widow2husb_dist & correct NA
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9371728
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.748954

# widow2wido only
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9555556
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7196653

# widow2husb only
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.8780488
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7531381
# presumably widow2husb bad because sons

# widow full
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.9119171
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7364017

# widow last and first separately
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.956044
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7280335

# widow last and first separately, widow2husb
# > Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.8823529
# > Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
# [1] 0.7531381


table(x_eval$correct, x_eval$predicted > 0.5)
Metrics::precision(x_eval$correct, x_eval$predicted > 0.5)
Metrics::recall(x_eval$correct, x_eval$predicted > 0.5)
Metrics::fbeta_score(x_eval$correct, x_eval$predicted > 0.5)

data.table(

    )

xgboost::xgb.importance(model = m)

prc = data.table(
    thresh = 0:100 / 100)
prc[, precision := sapply(thresh, \(x) Metrics::precision(x_eval$correct, x_eval$predicted > x))]
prc[, recall := sapply(thresh, \(x) Metrics::recall(x_eval$correct, x_eval$predicted > x))]
prc[, f1 := sapply(thresh, \(x) Metrics::fbeta_score(x_eval$correct, x_eval$predicted > x))]
plot(recall ~ precision, data = prc)
points(recall ~ precision, data = prc[thresh == 0.5], col = 2, pch = 19)

# find the best model again and explore prc and list false negatives

names(alabels)
out = x_eval[correct == 0 & predicted > 0.5, .SD, 
  .SDcols = patterns("year|names_.*clean|add_info|close_names|[mw](last|first)dist|initialsdist_osa|predicted|correct|coder)")]
fwrite(out, "~/data/cape/opg/stel_new_falsepos.csv")
writexl::write_xlsx(out, "~/data/cape/opg/stel_new_falsepos.xlsx")

# notes
# duplicates casting doubt over a chain and this is very hard for the model to learn
    # one option is to just make the chains, and chop them up later
    # 
# long single blocks
    # maybe use economic data
# clustering in train/eval split and data in general
# stack de gr and stel data on there
# update the training data with predictions
    # move a window through the training data thing
    # idea is to correct false negatives
    # looping is to prevent predicting in the training data
        # don't know where the loop is now
# prune the training data for marstat transitions
    # count to the marstart tranisition in each block given that we said correct 
        # single2married 1 ok
        # married2widow 1 probably ok
        # widow2married no
# should we use the economic data

# nextmfull_to [x]
    # who did this?
# nextmfull_from [x[]]
# wifepresent [x]
# size candidate block [x]
# overall name uniqueness 
    # for one year, entire district [x]
    # for entire block [x]
# add info from variables
    # junior/jonge 
        # junior dummy [x]
    # senious/oude
        # senior dummy [x]
    # x-zoon is to distinguish two in the same generation while jun/sen is to distinguish dad/son
        # make initials [x]
    # widow [x]
        # widow in wife name fullname_woman "weduwe j vd merwe" -- def. wife lost husb
        # widow in husb name fullname_man "weduwe j vd merwe" -- can be both; later more standardised 
        # widow is separate in add-info "weduwe"
            # we create widow_name and compare it to husb, wife, widow
            # we do lose in blocking


# todo list
    # 4 variables from add-info
        # jonathan
    # create other easier variables
    # get marstat count (how many marstat changes does an individual has)
        # this had to do with pruning the training data for marriage status changes, where sing2mar ok, mar2single ok singagain2mar one to far
    # try with extra gr st training data
    # whether clustering is better
    # get the model predictions for the entire training dataset
        # and then check our training data with the predictions

# widow and son_of cleaned, and with the oude and jonge dummy up to scratch

# precision recall curve
# widow distances (see notes above)
# marstat changes in the training data?
# augment with other training data?
# list those false negatives
