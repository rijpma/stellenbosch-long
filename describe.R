setwd("~/repos/stel-long/")

gt = fread("~/data/cape/opg/GT_Training_data_030624.csv", encoding = "Latin-1")

opg = fread("~/data/cape/opg/stellenbosch_long_linked_graphs.csv")

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
par(mfrow = c(2, 2))
hist(opg[, len_o])
hist(opg[, len])
hist(opg[, len_e])
hist(opg[, len_m])

opg[, max_linkdist_o := diff(range(year)), by = index_o]
opg[, max_linkdist := diff(range(year)), by = index]
opg[, max_linkdist_e := diff(range(year)), by = index_e]
opg[, max_linkdist_m := diff(range(year)), by = index_m]

par(mfrow = c(2, 2))
hist(opg[, max_linkdist_o])
hist(opg[, max_linkdist])
hist(opg[, max_linkdist_e])
hist(opg[, max_linkdist_m])


# linkage rates by approach
opg[, mean(len_o > 1)]
opg[, mean(len > 1)]
opg[, mean(len_e > 1)]
opg[, mean(len_m > 1)]

toplot = rbind(
    old = opg[, .N, by = list(len = len_o)],
    simple = opg[, .N, by = list(len = len)],
    extend = opg[, .N, by = list(len = len_e)],
    merge = opg[, .N, by = list(len = len_m)],
    idcol = "approach"
)
library("tinyplot")
pdf("out/linklengths_by_approach.pdf", height = 6)
par(mfrow = c(1, 1))
plt(N ~ len | approach, data = toplot[order(len)], type = "b", pch = 20, xlim = c(1, 100), log = "y")
grid()
dev.off()

plt(N ~ len | approach, data = toplot[order(len)][approach %in% c("old", "simple")], type = "b", pch = 20, log = "y")
abline(v = 2)
plt(N ~ len | approach, data = toplot[order(len)], type = "b", pch = 20, xlim = c(1, 150))
plt(N ~ len | approach, data = toplot[order(len)], type = "b", pch = 20, log = "xy")
plot(opg[!is.na(index_m), .N, by = list(len = len_m)])

par(mfrow = c(2, 2))
plot(opg[len > 1, .N, by = list(len = len_o)])
plot(opg[len > 1, .N, by = list(len = len)])
plot(opg[len > 1, .N, by = list(len = len_e)])
plot(opg[len > 1, .N, by = list(len = len_m)])

# linkage rates over time
toplot = rbind(
    old = opg[, mean(len_o > 1), by = year],
    simple = opg[, mean(len > 1), by = year],
    extend = opg[, mean(len_e > 1), by = year],
    merge = opg[, mean(len_m > 1), by = year],
    idcol = "approach"
)
plt(V1 ~ year | approach, data = toplot, type = "l")

pdf("out/linkrates_by_approach.pdf")
par(mfrow = c(2, 2))
plot(opg[, mean(len_o > 1), by = year][order(year)], 
    main = "old",
    type = "b", pch = 20, ylim = c(0, 1))
plot(opg[, mean(len > 1), by = year][order(year)], 
    main = "simple",
    type = "b", pch = 20, ylim = c(0, 1))
plot(opg[, mean(len_e > 1), by = year][order(year)], 
    main = "extend",
    type = "b", pch = 20, ylim = c(0, 1))
plot(opg[, mean(len_m > 1), by = year][order(year)], 
    main = "merge",
    type = "b", pch = 20, ylim = c(0, 1))
dev.off()

pdf("out/medianlinklength_by_approach.pdf")
par(mfrow = c(2, 2))
plot(opg[, median(len_o), by = year][order(year)], 
    main = "old",
    ylim = c(0, 60),
    type = "b", pch = 20)
plot(opg[, median(len), by = year][order(year)], 
    main = "simple",
    ylim = c(0, 60),
    type = "b", pch = 20)
plot(opg[, median(len_e), by = year][order(year)], 
    main = "extend",
    ylim = c(0, 60),
    type = "b", pch = 20)
plot(opg[, median(len_m), by = year][order(year)], 
    main = "merge",
    ylim = c(0, 60),
    type = "b", pch = 20)
dev.off()
