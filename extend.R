# # opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024nov1.csv")
# # # opg = opg[persid %in% c(685,1004,1005,1243,1684,2027,2194,2268,2711,2842,3099,3143,3674,4235,4525,4978,5227,5518)][order(-year)]
# # # opg = opg[persid %in% c(117266,119044,121076,123004,124510,126378,127692,128701,129057,130184,131985,133152,137127,139384,140475)][order(-year)]
# opg = opg[persid %in% c(5929,2130,5300,4614,4362,4058,5907,2131,4945,3762,3214,2958,2390,7251,6854,3495,6219,3172,3496,8372,6218,8776,5591,6576)][order(-year)]

# which(names(mtchlist) == "1722")
# opg[, index := NA_integer_]
# opg[, score := NA_real_]
# # for (i in 114:length(mtchlist)){
# for (i in 114:123){
#     link_rows(opg, approach = "extend_nodup")
#     oops = opg[!is.na(index), any(duplicated(index)), by = year][, any(V1)]
#     cat(i, " - ", oops, "\n")
# }
# i = 124
# # warnings()
# # opg

# mtchlist[i]
# d = copy(opg)
# vrbs = c("year", "persid", "index", "score", "index_candidate", "score_candidate", "hold_index_candidate", "hold_score_candidate", "worse_score", "duplicate_years")
# d
# run up until here for checking

# # i = 114
# # i = 115
# # link_rows(opg, approach = "extend_nodup")
# # # nb this does all it's action in-place, so don't run 
# # options(knitr.kable.NA = '-')
# # export = function(x, con) writeLines(knitr::kable(x, digits = 3), con = con)

duplicates = function(x) duplicated(x) | duplicated(x, fromLast = TRUE)

approach = "extend_nodup"
approach = "extend_best"
link_rows = function(d, approach = "simple"){

    # knitr::kable(d)
    # make 
    d[, index_candidate := NA_integer_]
    d[, hold_index_candidate := NA_integer_]
    d[, hold_score_candidate := NA_real_]
    d[, score_candidate := NA_real_]
    d[, worse_score := NA]
    d[, new_unassigned_block := NA]
    d[, duplicate_years := NA]

    # assign potential links and scores from mtchlist (should be passed...)
    d[match(mtchlist[[i]]$persid_to, persid), index_candidate := mtchlist[[i]]$persid_from]
    d[match(mtchlist[[i]]$persid_to, persid), score_candidate := mtchlist[[i]]$predicted]
    # export(d[, ..vrbs], "~/desktop/extend_candidates.txt")


    # completely empty linksets get filled in (this by itself is the "simple" approach)
    # find unassigned blocks for all index candidates
    d[, new_unassigned_block := all(is.na(index)), by = index_candidate]

    d[new_unassigned_block == TRUE & !is.na(index_candidate), index := index_candidate]
    d[new_unassigned_block == TRUE & !is.na(index_candidate), score := score_candidate]
    # d[new_unassigned_block == TRUE, index := persid[persid == index_candidate & !is.na(index_candidate)], by = index_candidate]
    # export(d[, ..vrbs], "~/desktop/no_extend.txt")
    
    if (approach == "extend" | approach == "extend_best" | approach == "extend_nodup"){        
        # new, unlinked hh now linked get the pre-existing index
        # d[new_unassigned_block == FALSE & !is.na(index_candidate), index := max(index, na.rm = TRUE), by = index_candidate]

        # alternative:
        # knitr::kable(d)
        # line below ensures that extending only happens from the fromyear, not filling back
        # d[new_unassigned_block == FALSE, hold_index_candidate := index[persid == index_candidate & !is.na(index_candidate)], by = index_candidate]

        # d[new_unassigned_block == FALSE & !is.na(index_candidate), nafill(index, type = "locf"), by = index_candidate]
        d[new_unassigned_block == FALSE & !is.na(index_candidate), hold_index_candidate := index, by = index_candidate]
        d[new_unassigned_block == FALSE & !is.na(index_candidate), hold_index_candidate := nafill(hold_index_candidate, type = "locf"), by = index_candidate]
        d[new_unassigned_block == FALSE & !is.na(index_candidate), hold_index_candidate := nafill(hold_index_candidate, type = "nocb"), by = index_candidate]

        d[new_unassigned_block == FALSE, hold_score_candidate := score_candidate, by = index_candidate]
        d[new_unassigned_block == FALSE & !is.na(index_candidate), score_candidate := nafill(score_candidate, type = "locf"), by = index_candidate]
        d[new_unassigned_block == FALSE & !is.na(index_candidate), score_candidate := nafill(score_candidate, type = "nocb"), by = index_candidate]

        # export(d[, ..vrbs], "~/desktop/extend_shared.txt")

        # at this point it is about to:
            # assign the best score from the mtchlist, which is fine
            # but also in 1701 it's about to insert an inferior score
            # but you don't know the block yet, so fix this after making the index!

        # set to NA if it's duplicated in that year and the lowest score
        # this should set 

        if (approach == "extend" | approach == "extend_best"){
            d[new_unassigned_block == FALSE & is.na(index) & !is.na(index_candidate), index := hold_index_candidate, by = index_candidate]
            # this now fills in scores even when there is no link, which is bad
            d[new_unassigned_block == FALSE & !is.na(index) & is.na(score) & !is.na(score_candidate), score := hold_score_candidate, by = index_candidate]
            # export(d[, ..vrbs], "~/desktop/extend_simple.txt")


            if (approach == "extend_best"){
                # this is easiest done after making score because the comparison across two variables is easier
                d[!is.na(score), worse_score := score < max(score, na.rm = TRUE), by = list(index, year)]
                # remaining duplicates (9) probably because exactly equal scores
                # drop worse scores
                d[worse_score == TRUE, index := NA]
                d[worse_score == TRUE, score := NA]
                # export(d[, ..vrbs], "~/desktop/extend_best.txt")

            }
        }  
        if (approach == "extend_nodup"){
            # find duplicate years for an index_candidate, 
            # d[, duplicate_years := duplicates(year), by = index_candidate]
            # d[, ..vrbs]
            # ok so the issue is that the duplicate_years are made by
            # index_candiate, but it's about to fill it in there because
            # index_candidate is only for one of the two...

            # but that might be impossible to fix because the full cluster is not known beforehand (unless we do subgraphs thing first...)
            d[new_unassigned_block == FALSE & is.na(index) & !is.na(index_candidate) & duplicate_years == FALSE, index := hold_index_candidate, by = index_candidate]            
            d[new_unassigned_block == FALSE & is.na(score) & !is.na(score_candidate) & duplicate_years == FALSE, score := hold_score_candidate, by = index_candidate]
            # export(d[, ..vrbs], "~/desktop/extend_nodup.txt")

            # so if we do the block lines 81-85, we can then say
            # CHECK and implement

            # d[!is.na(score), duplicate_years := duplicates(year), by = list(index)]
            # d[duplicate_years == TRUE & !is.na(index_candidate), index := NA]
            # d[duplicate_years == TRUE & !is.na(index_candidate), score := NA]


        }
    }

    # d[new_unassigned_block == FALSE & !is.na(index_candidate) & is.na(index), index := max(index, na.rm = TRUE), by = index_candidate]
    # index[1] assumes some amount of sorting, either do or fix
    # knitr::kable(d)
    # linksets get merged
    if (approach == "merge"){
        d[, unique(index), by = index_candidate]
        d
        d[, index := max(index, na.rm = TRUE)[!is.na(index_candidate)], by = index_candidate]
    }
    # knitr::kable(d)

    # return(d$index)
}

