library("data.table")

opg = fread("~/data/cape/opg/stellenbosch_long_linked_graphs.csv",
    na.string = "")
opg[opg == ""] = NA

cat(names(opg), sep = "\n")

opn = fread("~/downloads/Stel_numericdata_july24data.csv")

intersect(names(opn), names(opg)) |> cat(sep = "\n")

dim(opg)
opg = merge(
    opg, 
    opn[, -c("nr", "year", "add_info", "district", "field_cornet", "names_men", 
             "names_women", "settler_sons", "settler_daughters", 
             "settler_children")], 
    all.x = TRUE, by = "persid"
)
dim(opg)

opg[, index_o := NULL]
opg[, index_m := NULL]
opg[, len_o := NULL]
opg[, len_m := NULL]
opg[, fromyear_o := NULL]
opg[, fromyear_m := NULL]
opg[, frompersid_o := NULL]
opg[, frompersid_m := NULL]



index
index_e
fromyear
fromyear_e
frompersid
frompersid_e
len
len_e
cluster
index_g
len_g
simple_is_subgraph
simple_is_extended
share_simple_in_subgraph
share_simple_in_extended



fwrite(opg, "~/data/cape/opg/stellenbosch_long_linked_full_nov2024.csv")

plot(opg[, mean(vines, na.rm = TRUE), by = year])