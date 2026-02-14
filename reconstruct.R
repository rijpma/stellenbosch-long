rm(list = ls())

setwd("~/repos/stel-long/")

library("capelinker")
library("data.table")
library("igraph")

source("extend.R")

# cleaned opgaafrollen data
opg = fread("~/data/cape/opg/stellenbosch_long_cleaned_auke_2024nov1.csv", na = "")

# read in matches, to be made into list and list of graphs below
opg_mtchd = fread("~/repos/capelinker/out/stellenbosch_matches_olddata_nov3model.csv")

# identify all distinct subgraphs to
# 1 check results
# 2 be able to id duplicate years
# 3 check how close the linked household is to all possible links
tograph = opg_mtchd[persid_from != persid_to, list(persid_from, persid_to)]

# needs ids to be character
tograph = tograph[, lapply(.SD, as.character)]

g = igraph::graph_from_edgelist(as.matrix(tograph[, list(persid_from, persid_to)]))
subgraphs = igraph::decompose(g)

# get persids from nodes (vertices)
l = lapply(subgraphs, \(x) names(V(x)))

# take biggest persid as name of each subgraph
names(l) = sapply(l, \(g) max(as.numeric(g)))

# create data.table with subgraphs and persids
l = lapply(l, as.data.table)
subgraphs = rbindlist(l, idcol = "subgraph")
subgraphs[, subgraph := as.integer(subgraph)]
subgraphs[, V1 := as.integer(V1)]

# merge back into opg
dim(opg)
opg = merge(
    opg,
    subgraphs,
    by.x = "persid",
    by.y = "V1",
    all.x = TRUE)
dim(opg)

# length one should also have a subgraph id
opg[is.na(subgraph), subgraph := persid]

# get subgraph sizes
opg[, len_g := .N, by = subgraph]

# sample subset to check
set.seed(321)
subgraphs_to_check = opg[between(len_g, 10, 100) & len_g %% 5 == 0][, sample(subgraph, 1), by = len_g][, V1]

check = TRUE
if (check){
    # subgraphs_to_check = 84573
    persids_to_check = opg[subgraph %in% subgraphs_to_check, persid]

    opg_mtchd = opg_mtchd[persid_from %in% persids_to_check | persid_to %in% persids_to_check]
    opg = opg[persid %in% persids_to_check]
}

# keep a copy without links to insert different link strategies in
opg_nolinks = copy(opg)

# split the giant table into a matchlist depending on strategy

strategy = "best links first"
# strategy = "start from end"

if (strategy == "start from end"){
    mtchlist = split(opg_mtchd[order(-year_from), list(year_from, persid_from, year_to, persid_to, predicted)], by = "year_from")
}

if (strategy == "best links first"){
    opg_mtchd[, quality := sum(predicted, na.rm = TRUE) + 1, by = persid_from]
    opg_mtchd[, len := .N, by = persid_from]
    opg_mtchd[order(-quality), list(persid_from, quality, len)] |> unique()
    mtchlist = split(opg_mtchd[order(-quality, persid_from), list(year_from, persid_from, year_to, persid_to, predicted)], by = "persid_from")
}

opg[, duplicated_years := duplicated(year) | duplicated(year, fromLast = TRUE), by = subgraph]


# lists to fill with variables to reinsert later
fromplist = list()
fromylist = list()
indexlist = list()
problist = list()

# loop over approaches
approaches = c("simple", "extend_best", "extend_nodup")
# approaches = c("extend_nodup")
for (approach in approaches){

    # link_rows needs an index variable to identify unassigned blocks, so create here
    opg[, index := NA_integer_]

    for (i in 1:length(mtchlist)){
    # for (i in 1:15){
        link_rows(opg, matches = mtchlist[[i]], approach = approach)
        cat(i, " - ")
        # print(opg[!is.na(index), any(duplicated(year))])
    }

    indexlist[[approach]] = opg$index
    fromylist[[approach]] = opg$fromyear
    fromplist[[approach]] = opg$score
    problist[[approach]] = opg$score


    # make sure variables are not filled from previous iterations
    opg[, index := NULL]
    opg[, score := NULL]
    opg[, index_candidate := NULL]
    opg[, fromyear := NULL]
    opg[, frompersid := NULL]

    gc()

}

opg_nolinks$index = indexlist[["simple"]]
opg_nolinks$fromyear = fromylist[["simple"]]
opg_nolinks$frompersid = fromplist[["simple"]]
opg_nolinks$score = problist[["simple"]]

opg_nolinks$index_b = indexlist[["extend_best"]]
opg_nolinks$fromyear_b = fromylist[["extend_best"]]
opg_nolinks$frompersid_b = fromplist[["extend_best"]]
opg_nolinks$score_b = problist[["extend_best"]]

opg_nolinks$index_n = indexlist[["extend_nodup"]]
opg_nolinks$fromyear_n = fromylist[["extend_nodup"]]
opg_nolinks$frompersid_n = fromplist[["extend_nodup"]]
opg_nolinks$score_n = problist[["extend_nodup"]]

opg_nolinks[is.na(index), index := persid]
opg_nolinks[is.na(index_b), index_b := persid]
opg_nolinks[is.na(index_n), index_n := persid]

opg_nolinks[, len := .N, by = index]
opg_nolinks[, len_b := .N, by = index_b]
opg_nolinks[, len_n := .N, by = index_n]

opg_nolinks[, mean(len)]
opg_nolinks[, mean(len_b)]
opg_nolinks[, mean(len_n)]

opg_nolinks[, sum(duplicated(year)), by = index][, summary(V1)]
opg_nolinks[, sum(duplicated(year)), by = index_b][, summary(V1)]
opg_nolinks[, sum(duplicated(year)), by = index_n][, summary(V1)]

fwrite(opg_nolinks, "~/data/cape/opg/stellenbosch_long_linked_bfirst_apr2025.csv")

opg_nolinks = fread("~/data/cape/opg/stellenbosch_long_linked_bfirst_apr2025.csv")


opg_tocheck = opg_nolinks[subgraph %in% subgraphs_to_check]
# currently this works because all the other clusters are NA

# viz the graphs
# remake for now
tograph = opg_mtchd[persid_from != persid_to, list(persid_from, persid_to)]

# need ids to be character
tograph = tograph[, lapply(.SD, as.character)]

g = graph_from_edgelist(as.matrix(tograph[, list(persid_from, persid_to)]))
subgraphs = decompose(g)

# add clusters
clusters = opg_tocheck[match(names(V(g)), as.character(opg_tocheck$persid)), subgraph]
V(g)$cluster = clusters

# add couple names
couples_sorted = opg_tocheck[match(names(V(g)), as.character(opg_tocheck$persid)), paste0(toupper(minitials), mlast, "-", toupper(winitials), wlast)]
V(g)$couple = couples_sorted

# add index
index = opg_tocheck[match(names(V(g)), as.character(opg_tocheck$persid)), as.character(index)]
V(g)$index = index

# add index_b
index_b = opg_tocheck[match(names(V(g)), as.character(opg_tocheck$persid)), as.character(index_b)]
V(g)$index_b = index_b

# add index_n
index_n = opg_tocheck[match(names(V(g)), as.character(opg_tocheck$persid)), as.character(index_n)]
V(g)$index_n = index_n

# add year of couple
years = opg_tocheck[match(names(V(g)), as.character(opg_tocheck$persid)), year]
V(g)$year = years


# add edge strength
scores = opg_mtchd[persid_from != persid_to][match(attr(E(g), "vnames") , paste0(persid_from, "|", persid_to)), predicted]
E(g)$score = scores
E(g)$weight = scores
is_weighted(g)

plot(gu)
gu = as_undirected(g)
summary(gu)
ldc = cluster_leiden(gu, resolution = 0.2)
# plot(ldc, gu)

# communities(ldc)

leiden = ldc$membership[match(names(V(g)), ldc$names)]
V(g)$leiden = leiden

subgraphs = decompose(g)
subgraphs = subgraphs[order(sapply(subgraphs, length))]

tocheck = sapply(subgraphs, \(g) all(V(g)$cluster %in% subgraphs_to_check))
subgraphs = subgraphs[tocheck]

tomerge = data.table(index_l = ldc$membership, persid = as.integer(ldc$names))
opg_tocheck = merge(
    opg_tocheck,
    tomerge,
    by = "persid"
)
opg_tocheck[, len_l := .N, by = index_l]

pdf("./out/reconstruction_examples.pdf", width = 12, height = 12)
par(mfrow = c(2, 2))
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


    for (ind in c("index", "index_b", "index_n", "leiden")){
        col = as.factor(vertex_attr(subgraph, ind))
        plot(subgraph,
            main = paste(cluster, ind),
            vertex.color = col,
            arrow.size = 0.5,
            layout = tree_crds,
            vertex.label = lbls,
            vertex.label.cex = resize,
            edge.width = E(subgraph)$weight * 2
            # vertex.size = degree(subgraph)
        )
        ny = diff(range(V(subgraph)$year)) + 1
        grid(ny = floor(ny / 5))
    }
}
dev.off()

out = opg_tocheck[subgraph %in% subgraphs_to_check]
out = out[order(len_g, -year, index_b, index),
    list(cluster, len_g, persid, year,
        index_b, frompersid_b,
        index_n, frompersid_n,
        index_l, frompersid_l = NA,
        index, frompersid,
        add_info,
        names_men_clean, names_women_clean)]

out = out[, rbind(.SD, NA, fill = TRUE), by = cluster]
fwrite(out, "out/compare96_simple_best.csv")
writexl::write_xlsx(out, "out/compare96_simple_best.xlsx")

# 84573 is allowing duplicate year linkage
# 92259 is making a link chain longer than the biggest one
opg_tocheck[subgraph == 84573, list(year, duplicated(year), index)]
