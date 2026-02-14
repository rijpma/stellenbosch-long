rm(list = ls())

# library("remotes")
# remotes::install_github("rijpma/capelinker")
library("capelinker")

library('data.table')
library("xgboost")
library("stringi")
library("stringdist")

# opg = fread("~data/cape/opg/Stel_namesplit_full_090824.csv")
# opg = fread("~/data/cape/opg/Stel_namesplit_full_240715.csv")
# nb below is the old data, with the new cleaning method; we ignore discrepancies in the candidate dataset because we cannot fix those without 

# nb also the encoding now seems to be utf8?
opg_train = fread("~/data/cape/opg/Stel_namesplit_old_data_new_method_171024.csv", 
    # encoding = "Latin-1",
    na.string = "")
# no identifiers, 1834 missing, but we still want to move to this dataset tp see of it works
# opg_new = fread("~/data/cape/opg/Stel_namesplit_full_171024.csv")
opg_new = fread("~/data/cape/opg/Stel_namesplit_july24data_281024.csv",
    encoding = "Latin-1")


clean = function(opg){

    # small fixes: combined names, non-names, analphabetics
    opg[names_men_clean == "frans joosten van lubstad &", mlast := "lubstad"]
    opg[names_men_clean == "frans joosten van lubstad &", mfirst := "frans joosten"]
    opg[names_men == "Maijboom (no names}", mlast := "MAIJBOOM"]
    opg[, mlast := stri_replace_all_fixed(mlast, "ILLEGIBLE.", "")]
    opg[, mlast := stri_replace_all_fixed(mlast, "EUROPA", "")]
    opg[, mlast := stri_replace_all_fixed(mlast, "HUIS", "")]
    opg[, mlast := stri_replace_all_fixed(mlast, "VERTROKKEN", "")]
    opg[, mlast := stri_replace_all_fixed(mlast, "DOOD", "")]
    opg[, mlast := stri_replace_all_fixed(mlast, "NUMBERS PRESENT", "")]
    opg[, mlast := stri_replace_all_regex(mlast, "[.]\\W{0,}$", "")]

    opg[, mfirst := stri_replace_all_regex(mfirst, "\\(.*", "")]

    opg[, wlast := stri_replace_all_fixed(wlast, "NO WIFE'S NAME LISTED", "")]
    opg[, wlast := stri_replace_all_fixed(wlast, "GEBOREN:", "")]
    opg[, wlast := stri_replace_all_fixed(wlast, "&", "")]
    opg[, wlast := stri_replace_all_fixed(wlast, 'NB: ""VROUW"" NUMERICALLY LISTED', "")]
    opg[, wlast := stri_replace_all_fixed(wlast, ":", "")]
    opg[, wlast := stri_replace_all_regex(wlast, ".*\\)", "")]

    opg[, wfirst := stri_replace_all_fixed(wfirst, "GEBOREN:", "")]
    opg[, wfirst := stri_replace_all_fixed(wfirst, "'HUISVROUW'", "")]
    opg[, wfirst := stri_replace_all_fixed(wfirst, "WOMAN'S NAME CUT", "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, ":.*", "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, ":.*", "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, '\\".*', "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, '\\{.*', "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, '&.*', "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, '.*,', "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, "\\(.*", "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, ".*\\)", "")]

    # no letters in string
    opg[, mlast := stri_replace_all_regex(mlast, "^\\W+$", "")]
    opg[, mfirst := stri_replace_all_regex(mfirst, "^\\W+$", "")]
    opg[, wlast := stri_replace_all_regex(wlast, "^\\W+$", "")]
    opg[, wfirst := stri_replace_all_regex(wfirst, "^\\W+$", "")]

    opg[nchar(wfirst) == 1, wfirst]

    # remove "and his wife"
    hiswife = "ZYNE? VR?R?OUW?"
    opg[, wfirst := stri_replace_all_regex(wfirst, hiswife, "")]
    opg[, wlast := stri_replace_all_regex(wlast,  hiswife, "")]

    # drop empty observartions
    nrow(opg)

    # these are urls without info
    opg = opg[!stri_detect_fixed(names_men, "http")]
    nrow(opg)

    # these are source references without info
    opg = opg[!(stri_detect_regex(names_men_clean, "[^sz.]\\d") & names_women_clean == "")]
    nrow(opg)
    # the pattern takes a number without .sz in front of it because those are stray numbers in name fields

    # these are source references without info
    opg = opg[!(stri_detect_regex(names_women_clean, "\\d") & names_men == "")]

    # strip out more analphabetics

    # leave period in place
    wfirst_replace = "[…;´]"
    opg[stri_detect_regex(wfirst, "\\d"), wfirst := ""]
    opg[, wfirst := stri_replace_all_regex(wfirst, wfirst_replace, "")]

    # the remaining numbers here are source references but rest filled in
    wlast_replace = "[…\",:]"
    opg[stri_detect_regex(wlast, "\\d"), wlast := ""]
    opg[, wlast := stri_replace_all_regex(wlast, wlast_replace, "")]

    mfirst_replace = "[:\")}05]"
    opg[, mfirst := stri_replace_all_regex(mfirst, mfirst_replace, "")]

    # leave period in place
    mlast_replace = "[,{}\"259)]"
    opg[, mlast := stri_replace_all_regex(mlast, mlast_replace, "")]

    # harmonise all string variables
    cvars = c(
        "names_men_clean"           , "names_women_clean" , "mfirst_s"         , "mmiddle_s"      , "mvoor"         , "mlast"            ,
        "msuffix"                   , "mpatronym"         , "mtoponym"         , "wfirst_s"       , "wmiddle_s"     , "wvoor"            , "wlast" ,
        "wsuffix"                   , "wpatronym"         , "wtoponym"         , "widow_of"       , "mpatronym_sur" , "mpatronym_middle" ,
        "wpatronym_sur"             , "wpatronym_middle"  , "mfirst"           , "minitials"      , "wfirst"        , "winitials"        ,
        "wid_of_first_name"         , "wid_of_voor"       , "wid_of_last_name" , "wid_of_initial" ,
        "assign_widow_to_male_name" , "son_of"            , "son_of_init"

    )
    opg[, (cvars) := lapply(.SD, tolower), .SDcols = cvars]
    opg[, (cvars) := lapply(.SD, stri_replace_all_regex, "\\s{2,}", ""), .SDcols = cvars]
    opg[, (cvars) := lapply(.SD, stri_trim_both), .SDcols = cvars]

    # ensure all missing character vars are empty strings for joining below
    opg[is.na(names_men_clean)   , names_men_clean := ""]
    opg[is.na(names_women_clean) , names_women_clean := ""]
    opg[is.na(mfirst)            , mfirst := ""]
    opg[is.na(mvoor)             , mvoor := ""]
    opg[is.na(mlast)             , mlast := ""]
    opg[is.na(wfirst)            , wfirst := ""]
    opg[is.na(wvoor)             , wvoor := ""]
    opg[is.na(wlast)             , wlast := ""]
    opg[is.na(msuffix)           , msuffix := ""]
    opg[is.na(wsuffix)           , wsuffix := ""]
    opg[is.na(son_of)            , son_of := ""]
    opg[is.na(son_of_init)       , son_of_init := ""]
    
    # couple names
    opg[, couplename := paste(names_men_clean, names_women_clean)]
    opg[, couplename_full := paste(mfirst, mvoor, mlast, wfirst, wvoor, wlast, son_of_init)]
    opg[, couplename_extrafull := paste(mfirst, mvoor, mlast, msuffix, son_of, wfirst, wvoor, wlast, wsuffix)]

    # ensure no extra spaces in couple name
    cars = c("couplename", "couplename_full", "couplename_extrafull")
    opg[, (cvars) := lapply(.SD, stri_replace_all_regex, "\\s{2,}", ""), .SDcols = cvars]
    opg[, (cvars) := lapply(.SD, stri_trim_both), .SDcols = cvars]

    # this used to be active, rerun everything
    # opg[opg == ""] = NA


    # split this out?
    # closest couple in the block
    # takes long (2m for jw, 10m for osa, probably due to pmax step), so unused ones commented out
    # 2 minutes
    # opg[, closest_couple := stringdist_closest(couplename), by = year]
    # 10 minutes (probably because of the pmax in stringsimmatrix)
    # opg[, closest_couple_osa := stringdist_closest(couplename, method = "osa"), by = year]

    # opg[, closest_couple_full := stringdist_closest(couplename_full), by = year]
    # 10 minutes (probably because of the pmax in stringsimmatrix)
    opg[, closest_couple_full_osa := stringdist_closest(couplename_full, method = "osa"), by = year]

    # from now we want
    opg[opg == ""] = NA
    # inspect for NAs and how you want them to behave every step of the way

    # closest man in to-block, again osa is slow (6 v 1 m)
    opg[, closest_man := stringdist_closest(names_men_clean), by = year]
    # opg[, closest_man_osa := stringdist_closest(names_men_clean, method = "osa"), by = year]

    # settlerchildren data in other file...
    # this should not be in the function
    # also currently unused
    # sc = fread("~/data/cape/opg/stellenbosch_clean_jsversion.csv")
    # sc[, persid := .I] # need to make this, but checking on year, nr, names_men shows they are identical
    # sc = sc[, list(persid, year, settler_sons, settler_daughters, settler_children)]
    # sc[, settler_children_new := ifelse(is.na(settler_children), settler_sons + settler_daughters, settler_children)]

    # opg = merge(opg, sc[, list(persid, settler_children_new)], by = "persid", all.x = TRUE)
    # opg[!is.na(settler_children), list(settler_children, settler_children_new)]
    # opg[settler_children != settler_children_new]
    # opg[, settler_children := settler_children_new]
    # opg[, settler_children_new := NULL]
}

opg_cleaned = clean(opg_new)
preflight(opg_cleaned)

opg_train_cleaned = clean(opg_train)
preflight(opg_train_cleaned)

stri_subset_regex(opg_cleaned$mlast, "['.]", omit_na = TRUE)

stri_subset_regex(opg_cleaned$mfirst, "[,;&']", omit_na = TRUE)
stri_subset_regex(opg_cleaned$mfirst, "[,]", omit_na = TRUE)
# mfirst == "na, plesie" and many like them suggests some sort of paste(NA) |> tolower
# the combine names need manual fixing, skipped for now

stri_subset_regex(opg_cleaned$wlast, "[.&':]", omit_na = TRUE)
stri_subset_regex(opg_cleaned$wfirst, "['´]", omit_na = TRUE)

fwrite(opg_cleaned, "~/data/cape/opg/stellenbosch_long_cleaned_auke_2024nov1.csv")
fwrite(opg_train_cleaned, "~/data/cape/opg/stellenbosch_long_train_cleaned_auke_2024nov1.csv")
# fwrite(opg, "~/data/cape/opg/stellenbosch_long_cleaned_auke_2024oct18.csv")
