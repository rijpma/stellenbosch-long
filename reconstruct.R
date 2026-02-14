setwd("~/repos/stel-long/")

library("capelinker")
library("data.table")

source("extend.R")

# cleanup
# make sure that link_rows works on the old approach
# evaluate start from best linkage year (1771) and work down
    # shortens link length by half, gets rids of 2/3 of duplicate years
    # maybe respecting the chronology is important
    # maybe highest q loses some of the longest chains

# run simple and best on the new approach (high-q)
# maybe try the N as well

# note that

# opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024oct18.csv",
#     na.string = "")
opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024nov1.csv")
# opg = opg[persid %in% c(685,1004,1005,1243,1684,2027,2194,2268,2711,2842,3099,3143,3674,4235,4525,4978,5227,5518)][order(-year)]
# opg = opg[persid %in% c(117266,119044,121076,123004,124510,126378,127692,128701,129057,130184,131985,133152,137127,139384,140475)][order(-year)]
# opg = opg[persid %in% c(5929,2130,5300,4614,4362,4058,5907,2131,4945,3762,3214,2958,2390,7251,6854,3495,6219,3172,3496,8372,6218,8776,5591,6576)]

opgm = fread("~/data/cape/opg/stellenbosch_long_linked_graphs.csv")
opgm = opgm[between(len_g, 5, 100)]
set.seed(123321)
clusters_to_sample = opgm[, sample(cluster, 1), by = len_g]$V1
# clusters_to_sample = 68605
persids_to_sample = opgm[cluster %in% clusters_to_sample, persid]
opgm = opgm[cluster %in% clusters_to_sample]

opg = merge(
    opg,
    opgm[, list(persid, cluster, len_g)],
    by = "persid",
    all = FALSE
)

opg[opg == ""] = NA
# opg = fread("example140877.csv",
#     na.string = "")
# opg[opg == ""] = NA
opg_unlinked = copy(opg)

# opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_oct20model.csv")
# opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_oct20model.csv")
opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_nov3model.csv")

opg_mtchd = opg_mtchd[persid_from %in% persids_to_sample | persid_to %in% persids_to_sample]

mtchlist = split(opg_mtchd[order(-year_from), list(year_from, persid_from, year_to, persid_to, predicted)], by = "year_from")
# if we want to sort by quality of year, do this
lengths = sapply(mtchlist, function(x) x[, sum(predicted, na.rm = TRUE) + 1, by = persid_from][, mean(V1)])
# or(lengths)
mtchlist = mtchlist[order(lengths)]

# if sort by quality of persid_from, do this
opg_mtchd[, quality := sum(predicted, na.rm = TRUE) + 1, by = persid_from]
opg_mtchd[, len := .N, by = persid_from]
opg_mtchd[order(-quality), list(persid_from, quality, len)] |> unique()
mtchlist = split(opg_mtchd[order(-quality, persid_from), list(year_from, persid_from, year_to, persid_to, predicted)], by = "persid_from")
mtchlist[1]
mtchlist[length(mtchlist)]
length(mtchlist)

fromplist = list()
fromylist = list()
indexlist = list()
problist = list()
approach = "extend_new"
approach = "simple"
approach = "extend_best"
# for (approach in c("old", "simple", "extend", "extend_best", "extend_nodup", "merge")){
for (approach in c("simple", "extend_best")){
# for (approach in c("extend_best")){

    # roll out over opgaafrollen
    # expand index
    opg[, index := NULL]
    opg[, score := NULL]
    opg[, index2 := NULL]
    opg[, index_candidate := NULL]
    opg[, fromyear := NULL]
    opg[, frompersid := NULL]


    # first set of matches
    # I'm not sure why this isn't just in the loop?
    # reorder dat to match the order in mtchlist[[1]], then set index to persid
    opg[match(mtchlist[[1]]$persid_to, persid), index := mtchlist[[1]]$persid_from]

    # this added the relevant prediction score
    # dat[match(mtchlist[[1]]$persid_to, persid), pred := mtchlist[[1]]$pred]
    # add origin year and persid for bookkeeping
    # NB used to this, but match() doesn't work this way
    # opg[match(mtchlist[[1]]$persid_to, persid), fromyear := names(mtchlist)[1]]
    # opg[match(mtchlist[[1]]$persid_to, persid), frompersid := index]
    opg[!is.na(index), fromyear := names(mtchlist)[1]] # UPDATE
    opg[!is.na(index), frompersid := index]

    # first index fill from 1844
    # opg[year==1844, index := persid]

    # this order is probably not best
    # Not sure why this is here anymore (well we obviously want unlinked individuals to have an index, but why it's hardcoded to be 1844?)


    # loop over remaining years
    i = 2
    i = 3
    i = 4
    i = 5
    for (i in 2:length(mtchlist)){
    # for (i in 2:4){
    #     t0 = Sys.time()

    # for (i in 2:10e3){
    # for (i in 2:7){
        # cat("Indexed: ", sum(!is.na(opg$index)), '-- ')
        # fwrite(data.table(step = i, 
        #                 total = sum(!is.na(opg$index)),
        #                 fromyear = unique(mtchlist[[i]]$year_from)
        #             ),
        #  "~/repos/stel-long/out/test.csv", append = TRUE)
        opg[, index_candidate := as.integer(NA)]
        opg[match(mtchlist[[i]]$persid_to, persid), index_candidate := mtchlist[[i]]$persid_from]
        opg[match(mtchlist[[i]]$persid_to, persid), score_candidate := mtchlist[[i]]$predicted]

        # dat[, index_candidate := mtchlist[[i]][, persid[match(dat[, persid_from], mtchlist[[i]][, persid_to])]]]
        # also add the necessary persid somehow...

        # don't overwrite links already made
        opg[!is.na(index), index_candidate := NA]

        # dat$index_candidate = mtchlist[[i]]$persid[match(dat$persid, mtchlist[[i]]$persid.1)]

        # variant 1: just the new links
        # variant 2: extend, don't merge
        # variant 3: extend and merge
        if (approach == "old"){
            capelinker::expand_index(opg)
        } else {
            link_rows(opg, approach = approach)
        }

        # cat("total number of bridges:", sum(opg$bridge, na.rm = TRUE), "\n")

        opg[index %in% persid & is.na(frompersid), fromyear := names(mtchlist)[i]]
        opg[index %in% persid & is.na(frompersid), frompersid := index]

        # opg[, list(year, persid, names_men_clean, names_women_clean, index, score, fromyear, frompersid)][order(-year)] |>
            # knitr::kable()
        # i = i + 1
        cat(i, " - ")

    }
    # t1 = Sys.time()
    # (t1 - t0) * 10
    indexlist[[approach]] = opg$index
    fromylist[[approach]] = opg$fromyear
    fromplist[[approach]] = opg$frompersid
    problist[[approach]] = opg$frompersid
}
length(indexlist)

opg_unlinked$index = indexlist[["simple"]]
opg_unlinked$fromyear = fromylist[["simple"]]
opg_unlinked$frompersid = fromplist[["simple"]]

opg_unlinked$index_b = indexlist[["extend_best"]]
opg_unlinked$fromyear_b = fromylist[["extend_best"]]
opg_unlinked$frompersid_b = fromplist[["extend_best"]]

opg_unlinked[is.na(index), index := persid]
opg_unlinked[is.na(index_b), index_b := persid]

opg_unlinked[, len := .N, by = index]
opg_unlinked[, len_b := .N, by = index_b]

opg_unlinked[, mean(len)]
opg_unlinked[, mean(len_b)]

opg_unlinked[, sum(duplicated(year)), by = index][, summary(V1)]
opg_unlinked[, sum(duplicated(year)), by = index_b][, summary(V1)]

all(opg_unlinked$persid == opg$persid)
all(opg_unlinked$persid == opg_mtchd$persid)

# d = opg_unlinked[len_g == 40]
# m = as.matrix(d[, lapply(list(frompersid, persid), as.character)])
# g = graph_from_edgelist(na.omit(m))
# plot(g)

counts = opg_unlinked[, list(all(index_b == index), unique(len_g)), by = cluster]
clusters_to_check = counts[V1 == FALSE][order(V2)][V2 %% 5 == 0, cluster]

# all the corresponding graphs
tograph = opg_mtchd[persid_from != persid_to, list(persid_from, persid_to)]
tograph = tograph[, lapply(.SD, as.character)]

library("igraph")
x = graph_from_edgelist(as.matrix(tograph[, list(persid_from, persid_to)]))

# add clusters
clusters = opg_unlinked[match(names(V(x)), as.character(opg_unlinked$persid)), cluster]
V(x)$cluster = clusters

# add couple names
couples_sorted = opg_unlinked[match(names(V(x)), as.character(opg_unlinked$persid)), paste0(toupper(minitials), mlast, "-", toupper(winitials), wlast)]
V(x)$couple = couples_sorted

# add index
index = opg_unlinked[match(names(V(x)), as.character(opg_unlinked$persid)), as.character(index)]
V(x)$index = index

# add index_b
index_b = opg_unlinked[match(names(V(x)), as.character(opg_unlinked$persid)), as.character(index_b)]
V(x)$index_b = index_b

# add year of couple
years = opg_unlinked[match(names(V(x)), as.character(opg_unlinked$persid)), year]
V(x)$year = years

# add edge strength
E(x)$weight = opg_mtchd[persid_from != persid_to, predicted]

V(x)$cluster

subgraphs = decompose(x)
subgraphs = subgraphs[order(sapply(subgraphs, length))]

tocheck = sapply(subgraphs, \(g) all(V(g)$cluster %in% clusters_to_check))
subgraphs = subgraphs[tocheck]

pdf("./out/reconstruction_examples.pdf", width = 12, height = 12)
par(mfrow = c(1, 2))
for (subgraph in subgraphs){

    tree_crds = layout_as_tree(subgraph)
    tree_crds[, 2] = V(subgraph)$year

    lbls = paste(
        names(V(subgraph)),
        V(subgraph)$couple,
        V(subgraph)$year,
        sep = "\n"
    )
    cluster = unique(V(subgraph)$cluster)
    resize = length(V(subgraph))
    resize = 1 / log(resize / 5)
    print(resize)


    for (ind in c("index", "index_b")){
        col = as.factor(vertex_attr(subgraph, ind))
        plot(subgraph,
            main = paste(cluster, ind),
            vertex.color = col, 
            layout = tree_crds,
            vertex.label = lbls,
            vertex.label.cex = resize,
            # vertex.size = degree(subgraph)
        )
        ny = diff(range(V(subgraph)$year)) + 1
        grid(ny = floor(ny / 5))
    }
}
dev.off()

# out = opg_unlinked[cluster %in% clusters_to_check]
# out = out[order(len_g, -year, index_b, index), list(cluster, len_g, persid, year, index_b, frompersid_b, index, frompersid, add_info, names_men_clean, names_women_clean)]

# out = out[, rbind(.SD, NA, fill = TRUE), by = cluster]
# fwrite(out, "out/compare96_simple_best.csv")
# writexl::write_xlsx(out, "out/compare96_simple_best.xlsx")

opg[!is.na(index)]
optimistic = opg_mtchd[, list(len = unique(len)), by = persid_from][, .N, by = len]
plot(optimistic, log = "xy")
reality = opg[!is.na(index), list(len = .N), by = index][, .N, by = len]
points(N ~ len, data = reality, col = 2)



opg_unlinked$index_o = indexlist[["old"]]
opg_unlinked$index_e = indexlist[["extend"]]
opg_unlinked$index_nd = indexlist[["extend_nodup"]]
opg_unlinked$index_m = indexlist[["merge"]]

opg_unlinked$fromyear_o = fromylist[["old"]]
opg_unlinked$fromyear_e = fromylist[["extend"]]
opg_unlinked$fromyear_nd = fromylist[["extend_nodup"]]
opg_unlinked$fromyear_m = fromylist[["merge"]]

opg_unlinked$frompersid_o = fromplist[["old"]]
opg_unlinked$frompersid_e = fromplist[["extend"]]
opg_unlinked$frompersid_nd = fromplist[["extend_nodup"]]
opg_unlinked$frompersid_m = fromplist[["merge"]]

# this enforces the no duplicate year extending rule (but still allows it as a start-off point, if you want to avoid that you need the graph solution and code it into link_rows)
opg_unlinked[order(-year, -fromyear_nd), dupl_year := duplicated(year), by = index_nd]
opg_unlinked[dupl_year == TRUE, index_nd := persid]
opg_unlinked[dupl_year == TRUE, fromyear_nd := NA]
opg_unlinked[dupl_year == TRUE, frompersid_nd := NA]

opg_unlinked[is.na(index_o), index_o := persid]
opg_unlinked[is.na(index_e), index_e := persid]
opg_unlinked[is.na(index_nd), index_nd := persid]
opg_unlinked[is.na(index_m), index_m := persid]

opg_unlinked[, len_o := .N, by = index_o]
opg_unlinked[, len_e := .N, by = index_e]
opg_unlinked[, len_nd := .N, by = index_nd]
opg_unlinked[, len_m := .N, by = index_m]

opg_unlinked[, mean(len_e)]
opg_unlinked[, mean(len_nd)]
opg_unlinked[, mean(len_m)]

opg_unlinked[, sum(duplicated(year)), by = index_e][, summary(V1)]
opg_unlinked[, sum(duplicated(year)), by = index_nd][, summary(V1)]

opg_unlinked[, sum(duplicated(year)), by = index_e][, mean(V1 > 0)]

# ok clearly nd is not working atm, so let's find an example
# see notes in extend, this is very hard w/o doing graph first
# you could also put a link counter in there and just nix the latest
# or just use fromyear?

# > opg_unlinked[, mean(len)]
# [1] 9.612637
# > opg_unlinked[, mean(len_e)]
# [1] 27.15415
# > opg_unlinked[, mean(len_b)]
# [1] 22.88712
# > opg_unlinked[, mean(len_nd)]
# [1] 23.00623
# > opg_unlinked[, mean(len_m)]
# [1] 49.92295
# > opg_unlinked[, sum(duplicated(year)), by = index][, summary(V1)]
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       0       0       0       0       0       0 
# > opg_unlinked[, sum(duplicated(year)), by = index_e][, summary(V1)]
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0000  0.0000  0.0000  0.1187  0.0000 93.0000 
# > opg_unlinked[, sum(duplicated(year)), by = index_b][, summary(V1)]
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000000 0.0000000 0.0002497 0.0000000 1.0000000 
# > opg_unlinked[, sum(duplicated(year)), by = index_nd][, summary(V1)]
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       0       0       0       0       0       0 



opg_unlinked[, dupl := duplicated(year), by = index_nd]
opg_unlinked[index_m %in% index_m[dupl == TRUE], .N, by = index_m][N == 10]
opg_unlinked[dupl == TRUE, .N, by = index_en][N == 2]
opg_unlinked[index_m == 8776][order(year), list(year, persid, dupl, index_nd, index_m)]

opg_unlinked[index_nd == 7251][order(year), list(year, persid, dupl, index_nd, index_e, index_m, fromyear_nd, fromyear_e)][order(-year)]


opg_unlinked[index_en == 5518]
opg_unlinked[index_m == 140475]
opg_unlinked[index_m == 140475, persid]
opg_unlinked[index_m == 140475, cat(persid, sep = ",")]
opg_unlinked[index]
# fwrite(opg_unlinked, "~/data/cape/opg/stellenbosch_long_linked.csv")
fwrite(opg_unlinked, "~/data/cape/opg/stellenbosch_long_linked_fixextend.csv")
