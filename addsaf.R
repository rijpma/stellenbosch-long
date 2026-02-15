# ---
# author: Auke Rijpma
# date: feb 2026
# adds SAF data to stellenbosch long panel
# ---

rm(list = ls())

setwd("~/data/cape/opg")

library("data.table")

opg = fread("./stellenbosch_long_linked_apr2025.csv.gz")
saf = fread("./saf2opg_1600_1824_2024nov7.csv")

# IDs in SAF have stel-long prefix
saf[, persid := as.integer(gsub(".*_", "", persid))]

# merge saf into opg
out = merge(
    opg,
    saf[, list(persid, couple_id, individual_id, nth_marriage, score_saf2opg = pred)],
    by.x = "hhobs",
    by.y = "persid",
    all.x = TRUE
)
nrow(out) == nrow(opg)

# some checks
# out[couple_id == "van eedea2_mar_2", list(names_men_clean, names_women_clean, hhid)]
# out[couple_id == "van eedea2_mar_2", list(names_men_clean, names_women_clean, hhid_ext)]
# out[couple_id == "van eedea2_mar_2", list(names_men_clean, names_women_clean, hhid_ext_safe)]
# out[couple_id == "ventera1_mar_2", list(couple_id, names_men_clean, names_women_clean, hhid, hhid_ext, hhid_ext_safe)]
# out[couple_id == "appela2b2_mar_1", list(couple_id, names_men_clean, names_women_clean, hhid, hhid_ext, hhid_ext_safe)]
# out[hhid == 5798, list(couple_id, names_men_clean, names_women_clean, hhid, hhid_ext, hhid_ext_safe)]
# out[hhid == 2551, list(couple_id, names_men_clean, names_women_clean, hhid, hhid_ext, hhid_ext_safe)]
out[!is.na(couple_id)][couple_id == sample(couple_id, 1), list(couple_id, names_men_clean, names_women_clean, hhid, hhid_ext, hhid_ext_safe, year, slave_men, vines, wine)]

# these are ok besides for the facts
# sometimes couple_id merges multiple hhid
# sometimes hhid is more extensive than couple_id

# quick check of these

# share largest hhid in couple_id
pdf("~/repos/stel-long/out/hhid_in_saf_couple.pdf", height = 5, width = 9)
par(mfrow = c(1, 3))
out[, max(table(hhid)) / .N, by = couple_id][order(V1)]$V1 |> 
    hist(main = "share unique HHID in couple id", xlab = "share HHID")
out[, max(table(hhid_ext)) / .N, by = couple_id][order(V1)]$V1 |> 
    hist(main = "share unique HHID_ext in couple id", xlab = "share HHID_ext")
out[, max(table(hhid_ext_safe)) / .N, by = couple_id][order(V1)]$V1 |> 
    hist(main = "share unique HHID_ext_safe in couple id", xlab = "share HHID_ext_safe")
dev.off()

# these are not so bad because usually we're ok with using couple_id to merge
# (these are typically solid links)

# share largest couple_id in hhid, omitting couple_id NA trips up and meaningless
pdf("~/repos/stel-long/out/saf_couple_in_hhid.pdf", height = 5, width = 9)
par(mfrow = c(1, 3))
out[!is.na(couple_id), max(table(couple_id)) / .N, by = hhid][order(V1)]$V1 |> 
    hist(main = "share unique couple_id in HHID", xlab = "share couple_id")
out[!is.na(couple_id), max(table(couple_id)) / .N, by = hhid_ext][order(V1)]$V1 |> 
    hist(main = "share unique couple_id in HHID_ext", xlab = "share couple_id")
out[!is.na(couple_id), max(table(couple_id)) / .N, by = hhid_ext_safe][order(V1)]$V1 |> 
    hist(main = "share unique couple_id in HHID_ext_safe", xlab = "share couple_id")
dev.off()
# these are a bit more problematic because I suspect the panel linkage to have
# more false positives. they're not very frequent though


opg = fwrite(
    out,
    "./stellenbosch_long_linked_wsaf_feb2026.csv.gz"
)
