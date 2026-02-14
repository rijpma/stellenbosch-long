setwd("~/repos/stel-long/")

library("data.table")
library("tinyplot")

gt = fread("~/data/cape/opg/GT_Training_data_030624.csv", encoding = "Latin-1")

# opg = fread("~/data/cape/opg/stellenbosch_long_linked_graphs.csv")
opg = fread("~/data/cape/opg/stellenbosch_long_linked_wsaf_feb2026.csv.gz", na.strings = "")

# opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_oct20model.csv")
opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_nov3model.csv")

mtchlist = split(opg_mtchd[order(-year_from), list(year_from, persid_from, year_to, persid_to)], by = "year_from")

gt[, list(
    pairs = .N,
    `unique from HH` = uniqueN(persid_from),
    `total matches` = sum(Match)
)] |> t() |> knitr::kable()
gt[, sum(Match), by = persid_from][, list(
    `min links` = min(V1),
    `med links` = median(V1),
    `avg links` = mean(V1),
    `max links` = max(V1)
)] |> t() |> knitr::kable(digits = 1)

# distribution of link lengths
opg_mtchd[, year_from := as.integer(year_from)]
opg_mtchd[!is.na(predicted), linkdist := year_from - year_to]


gt[, linkdist := year_from - year_to]
pdf("out/linkdists.pdf", height = 5, width = 10)
par(mfrow = c(1, 2))
plot(gt[Match == 1, .N, by = linkdist][order(linkdist)], 
    main = "GT data",
    xlab = "year dist",
    type = "b", 
    pch = 19)
plot(opg_mtchd[, .N, by = linkdist][order(linkdist)], 
    main = "Predictions",
    xlab = "year dist",
    type = "b", 
    pch = 19)
dev.off()
# + gt
# fix breaks

# series length
toplot = melt(opg, measure.vars = c("len", "len_ext", "len_ext_safe"), id.vars = "hhobs")
tinyplot::plt(
    ~ value, 
    facet = ~ variable, 
    data = toplot, 
    type = "hist"
)


opg[, max_linkdist := diff(range(year)), by = hhid]
opg[, max_linkdist_ext := diff(range(year)), by = hhid_ext]
opg[, max_linkdist_ext_safe := diff(range(year)), by = hhid_ext_safe]

par(mfrow = c(1, 3))
hist(opg[, max_linkdist])
hist(opg[, max_linkdist_ext])
hist(opg[, max_linkdist_ext_safe])


# linkage rates by approach
opg[, mean(len > 1)]
opg[, mean(len_ext > 1)]
opg[, mean(len_ext_safe > 1)]

toplot = rbind(
    none = opg[, .N, by = list(len = len)],
    extend = opg[, .N, by = list(len = len_ext)],
    extend_safe = opg[, .N, by = list(len = len_ext_safe)],
    idcol = "approach"
)

pdf("out/linklengths_by_safeapproach.pdf", height = 6)
par(mfrow = c(1, 1))
plt(N ~ len | approach, data = toplot[order(len)], type = "b", pch = 20,  log = "y")
grid()
dev.off()

# plot same given linked at all (ie remove N = 1)
toplot = melt(opg[len > 1], measure.vars = c("len", "len_ext", "len_ext_safe"), id.vars = "hhobs")
toplot = toplot[, .N, by = list(len = value, approach = variable)]
plt(N ~ len, facet = ~ approach, data = toplot)


# linkage rates over time
toplot = rbind(
    none = opg[, mean(len > 1), by = year],
    extend = opg[, mean(len_ext > 1), by = year],
    extend_safe = opg[, mean(len_ext_safe > 1), by = year],
    idcol = "approach"
)
pdf("out/linkrates_by_safeapproach.pdf", heig = 6)
plt(V1 ~ year | approach, data = toplot, type = "l", lwd = 1.5)
dev.off()

# saf linked rates over time
opg[, wifepresent := !(is.na(names_women_clean) | names_women_clean == "")]
toplot = cube(
    opg,
    mean(!is.na(couple_id)),
    by = c("year", "wifepresent")
)
toplot[, couple := fcase(
    is.na(wifepresent), "all",
    wifepresent == TRUE, "yes",
    wifepresent == FALSE, "no"
)]
toplot[is.na(wifepresent)]

pdf("out/linkrates_by_saf.pdf", height = 5)
plt(V1 ~ year, facet = ~ couple, 
    data = toplot[couple != "no"],
    type = "b", 
    pch = 20, 
    col = 2
)
dev.off()
