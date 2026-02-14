## train on stellenbosch new only
# rm(list = ls())

# library("remotes")
# remotes::install_github("rijpma/capelinker")

library('data.table')
# setDTthreads(1)
library("xgboost")
library("stringi")
library("capelinker")
library("stringdist")
library("tinyplot")
library("igraph")

# roll this out on the new data
# work the briding algorithm (it's not doing much atm)

# opg = fread("~data/cape/opg/Stel_namesplit_full_090824.csv")
# opg = fread("~/data/cape/opg/Stel_namesplit_full_240715.csv")
# opg = fread("~/data/cape/opg/Stel_namesplit_full_171024.csv", 
# nb below is the old data, with the new cleaning method; we ignore discrepancies in the candidate dataset because we cannot fix those without 
# nb also the encoding now seems to be utf8?
# opg = fread("~/data/cape/opg/Stel_namesplit_old_data_new_method_171024.csv", 
#     encoding = "Latin-1",
#     na.string = "")
# opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024oct18.csv",
#     na.string = "")
opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024nov1.csv")
opg[opg == ""] = NA

# load the model
# pretrained_model = xgb.load("~/repos/capelinker/data/stellenbosch_long.bin")
# load("~/repos/capelinker/data/stellenbosch_long.rda")
load("~/repos/capelinker/data/stellenbosch_long_noyear.rda")
pretrained_model = m

mtchlist = list()
match_threshold = 0.5
years = rev(sort(unique(opg$year)))
for (y in years){
    cat("year: ", y, "-------------\n")
    cnd = candidates(
        dat_from = opg[year == y],
        dat_to = opg[year %in% (y - 1):(y - 60)],
        blockvariable_from = "mlast", 
        blockvariable_to = "mlast",
        idvariable_from = "persid", 
        idvariable_to = "persid", 
        blocktype = c("bigram distance"), 
        linktype = c("one:one"), 
        maxdist = 0.5
    )

    # variables for the model
    cnd[, `(Intercept)` := 1]

    cnd[, mlastdist := stringdist::stringdist(mlast_from, mlast_to, method = "jw")]
    cnd[, mfirstdist := stringdist::stringdist(mfirst_from, mfirst_to, method = "jw")]
    cnd[, minitialsdist_osa := stringdist::stringdist(minitials_from, minitials_to, method = "osa")]
    cnd[, wlastdist := stringdist::stringdist(wlast_from, wlast_to, method = "jw")]
    cnd[, wfirstdist := stringdist::stringdist(wfirst_from, wfirst_to, method = "jw")]
    cnd[, winitialsdist_osa := stringdist::stringdist(winitials_from, winitials_to, method = "osa")]
    
    # this can be done once in the main data once? Doesn't seem that way
    cnd[, nextmfirst := capelinker::stringdist_closest(mfirst_to), by = list(persid_from, year_to)]

    cnd[, son_of_init_dist := stringdist(son_of_init_from, son_of_init_to, method = "osa")]

    cnd[, wifepresent_from := as.numeric(!is.na(wlast_from))]
    cnd[, wifepresent_to := as.numeric(!is.na(wlast_to))]

    cnd[, log_yeardist := log(year_from - year_to)]

    cnd[, N := .N, by = list(persid_from, year_to)]
    cnd[, logN := log(N)]

    cnd[ ,settler_children_gk := gk(settler_children_from, settler_children_to)]

    cnd[, young_dist := young_from - young_to]
    cnd[, old_dist := old_from - old_to]
    cnd[, widow_dist := widow_from - widow_to]
    cnd[, son_dummy_dist := son_of_dummy_from - son_of_dummy_to]

    cnd[, widow_first_name_dist := stringdist(wid_of_first_name_from, wid_of_first_name_to, method = "jw")]
    cnd[, widow_last_name_dist := stringdist(wid_of_last_name_from, wid_of_last_name_to, method = "jw")]
    cnd[, son_of_init_dist := stringdist(son_of_init_from, son_of_init_to, method = "osa")]

    # xgbdm matrix for predictions
    dm = xgboost::xgb.DMatrix(
        as.matrix(cnd[, .SD, .SDcols = pretrained_model$feature_names]))

    cnd[, predicted := predict(
        object = pretrained_model, 
        newdata = dm)]

    # find the highest predicted link for "from candidates" X year_to
    cnd[, rnk_from := rank(-predicted), by = list(year_to, persid_from)]
    
    # this is to ensure that persid_to is not actually preferred in a nother block (this is probably rare)
    cnd[, rnk_to := rank(-predicted), by = list(persid_to)]
    # think whether grouping still works. Old comment:
    # look why this is just persid
    # by=list(year_from, persid_to)]
    # no because year_from is fixed

    out = cnd[rnk_to == 1 & rnk_from == 1 & predicted > match_threshold, 
        list(persid_from, persid_to, year_to, predicted)]

    # hist(out$year_to)
    cat("matched: ", nrow(out), " -------------\n")

    # there was a reason to add this self-self link in here as well
    out = out[, list(persid_to = c(unique(persid_from), persid_to)), by = persid_from]
    out = merge(
        out,
        cnd[, list(persid_from, persid_to, predicted)],
        by = c("persid_from", "persid_to"),
        all.x = TRUE, all.y = FALSE)

    mtchlist[[as.character(y)]] = out

}

opg_mtchd = rbindlist(mtchlist, idcol = "year_from")
opg_mtchd = merge(
    opg_mtchd, 
    opg[, list(persid_to = persid, year_to = year)], 
    by = "persid_to",
    all.x = TRUE)
# fwrite(opg_mtchd, "~/repos/capelinker/out/stellenbosch_matches_olddata_oct20model.csv")
fwrite(opg_mtchd, "~/repos/capelinker/out/stellenbosch_matches_olddata_nov3model.csv")

# opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_oct20model.csv")
