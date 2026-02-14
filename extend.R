# one_subgraph = opg[persid %in% mtchlist[[1]]$persid_from, subgraph]
# dat = copy(opg[subgraph == one_subgraph][order(-year)])
# dat = copy(opg)

link_rows = function(dat, matches, approach){

    # premake variables to avoid type errors
    dat[, index_candidate := NA_integer_]
    dat[, hold_index_candidate := NA_integer_]
    dat[, hold_score_candidate := NA_real_]
    dat[, score_candidate := NA_real_]
    dat[, worse_score := NA]
    dat[, new_unassigned_block := NA]
    dat[, duplicated_years := NA]

    # copy current state of index to keep track of where links come from
    dat[, index_at_start := index]

    # assign potential links and scores from matches
    dat[match(matches$persid_to, persid), index_candidate := matches$persid_from]
    dat[match(matches$persid_to, persid), score_candidate := matches$predicted]
    dat[match(matches$persid_to, persid), year_from_candidate := matches$year_from]

    # completely empty linksets should always get filled in (this by itself is the "simple" approach)
    # step 1 find unassigned blocks for all index candidates
    dat[, new_unassigned_block := all(is.na(index)), by = index_candidate]

    # copy non-missing index_candidate to index if new_unassigned_block
    # this is simple approach
    dat[new_unassigned_block == TRUE & !is.na(index_candidate), index := index_candidate]
    dat[new_unassigned_block == TRUE & !is.na(index_candidate), score := score_candidate]

    # now use index_candidate to extend index
    if (approach == "extend" | approach == "extend_best" | approach == "extend_nodup"){

        # in unassigned blocks and when index_candidate exists, fill hold_index_candidate with value of index
        # and then expand this in both directions using nafill (nb needs numeric data)
        dat[new_unassigned_block == FALSE & !is.na(index_candidate), hold_index_candidate := index, by = index_candidate]
        dat[new_unassigned_block == FALSE & !is.na(index_candidate), hold_index_candidate := nafill(hold_index_candidate, type = "locf"), by = index_candidate]
        dat[new_unassigned_block == FALSE & !is.na(index_candidate), hold_index_candidate := nafill(hold_index_candidate, type = "nocb"), by = index_candidate]

        # do the same for score but now take score_candidate
        # expanding is to get a value for self-self links which are now the reverse link
        dat[new_unassigned_block == FALSE, hold_score_candidate := score_candidate, by = index_candidate]
        dat[new_unassigned_block == FALSE & !is.na(index_candidate), hold_score_candidate := nafill(hold_score_candidate, type = "locf"), by = index_candidate]
        dat[new_unassigned_block == FALSE & !is.na(index_candidate), hold_score_candidate := nafill(hold_score_candidate, type = "nocb"), by = index_candidate]

        # first extend and select best
                # alternative approach: never extend in duplicated years in the first place (that is: prefer first link always)
        if (approach == "extend_nodup"){
            # note we need the subgraph to idenfity the duplicated years

            dat[new_unassigned_block == FALSE & is.na(index) & !is.na(index_candidate) & duplicated_years == FALSE, index := hold_index_candidate] #, by = index_candidate]
            dat[new_unassigned_block == FALSE & is.na(score) & !is.na(score_candidate) & duplicated_years == FALSE, score := hold_score_candidate] #, by = index_candidate]

        } else if (approach == "extend" | approach == "extend_best"){

            # the potential extension in hold_index_candidate gets copied into index if new_unassigned_block and index missing and index_candidate not missing
            dat[new_unassigned_block == FALSE & is.na(index) & !is.na(index_candidate), index := hold_index_candidate, by = index_candidate]

            # this now fills in scores even when there is no link, which is bad
            dat[new_unassigned_block == FALSE & !is.na(index) & is.na(score), score := hold_score_candidate, by = index_candidate]

            dupls = dat[duplicated(year), year]
            dat[year %in% dupls, list(index, score, index_candidate, score_candidate)]
            dat[, list(year, index, score, index_candidate, score_candidate, hold_index_candidate, hold_score_candidate)]
            # if duplicate year link, keep the highest score
            if (approach == "extend_best"){
                # create variable if there is a worse score
                dat[!is.na(score), worse_score := score < max(score, na.rm = TRUE), by = list(index, year)]

                # set index and score to missing in that case
                dat[worse_score == TRUE, index := NA]
                dat[worse_score == TRUE, score := NA]
            }
        }
    }

    # what persid and year did the link originate from
    dat[!is.na(index) & is.na(index_at_start), frompersid := index_candidate]
    dat[!is.na(index) & is.na(index_at_start), fromyear := year_from_candidate]
}
