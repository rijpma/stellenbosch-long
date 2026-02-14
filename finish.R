library("data.table")

opg = fread("~/data/cape/opg/stellenbosch_long_linked_bfirst_apr2025.csv")
setorder(opg, year)

gt = fread("~/data/cape/opg/GT_Training_data_030624.csv", encoding = "Latin-1")

opg[, hhobs := persid]
# setnames(opg, "persid", "hhhobs")
setnames(opg, "frompersid", "from_hhobs")
setnames(opg, "index", "hhid")
setnames(opg, "fromyear", "from_year")

setnames(opg, "index_n", "hhid_ext")
setnames(opg, "len_n", "len_ext")
setnames(opg, "fromyear_n", "from_year_ext")
setnames(opg, "frompersid_n", "from_hhobs_ext")
setnames(opg, "score_n", "score_ext")


setnames(opg, "subgraph", "cluster")
setnames(opg, "len_g", "len_cluster")

opg[, hhid_ext_safe := hhid_ext]
opg[, from_year_ext_safe := from_year_ext]
opg[, from_hhobs_ext_safe := from_hhobs_ext]
opg[, score_ext_safe := score_ext]
opg[len_cluster > 45, hhid_ext_safe := hhid]
opg[len_cluster > 45, from_year_ext := from_year]
opg[len_cluster > 45, from_hhobs_ext := from_hhobs]
opg[len_cluster > 45, score_ext := score]

opg[, has_duplicated_years := any(duplicated(year)), by = cluster]
opg[has_duplicated_years == TRUE, hhid_ext_safe := hhid]
opg[has_duplicated_years == TRUE, from_year_ext_safe := from_year]
opg[has_duplicated_years == TRUE, from_hhobs_ext_safe := from_hhobs]
opg[has_duplicated_years == TRUE, score_ext_safe := score]

# don't want to break all 1685 links (no data 86-94)
opg[year > 1685, large_gap := (year - shift(year)) > 6, by = hhid_ext_safe]
opg[is.na(large_gap), large_gap := FALSE]
opg[, has_large_gap := any(large_gap)]
opg[, group_with_gap := cumsum(large_gap), by = hhid_ext_safe]
opg[, group_size_gapped := .N, by = list(hhid_ext_safe, group_with_gap)]
setorder(opg, -group_size_gapped, year)
opg[, is_largest_group_gapped := group_with_gap == group_with_gap[1], by = hhid_ext_safe]

# should all be in true
opg[,sum(is_largest_group_gapped == FALSE), by = has_large_gap]

opg[is_largest_group_gapped == FALSE, hhid_ext_safe := hhid]
opg[is_largest_group_gapped == FALSE, from_year_ext_safe := from_year]
opg[is_largest_group_gapped == FALSE, from_hhobs_ext_safe := from_hhobs]
opg[is_largest_group_gapped == FALSE, score_ext_safe := score]

opn = fread("~/downloads/Stel_numericdata_july24data.csv")

dim(opg)
dim(opn)
# the sixty row difference is all not real data (urls etc)
# checks out on equality of names and years, only diff is quote marks reading
opg = merge(
    opg[, -c("settler_children", "settler_daughters", "settler_sons")],
    opn[, -c("nr", "year", "names_men", "names_women", "add_info", "district", "field_cornet")],
    all.x = TRUE, by = "persid")
dim(opg)

opg = opg[, .SD, .SDcols = -patterns("_b$|group|gap")]

opg[, len_ext_safe := .N, by = hhid_ext_safe]

fwrite(opg, "~/data/cape/opg/stellenbosch_long_linked_apr2025.csv.gz")

# plots and tables
# confusion matrix




par(mfrow = c(1, 1))
plot(opg[len > 1, .N, by = len], xlim = c(0, 80))
points(opg[len_s > 1, .N, by = len_s], col = 2)
points(opg[len_n > 1, .N, by = len_n], col = 3)


par(mfrow = c(1, 2))
opg[, diff(year), by = hhid][V1 > 0, .N, by = V1] |> plot(log = "y")
grid()
gt[Match == 1][order(persid_from, year_to), diff(year_to), by = persid_from][V1 > 0, .N, by = V1] |> plot(log = "y")
grid()
