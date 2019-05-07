rm(list=ls())
#ler dados:
now=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/artiodactyla-NOW/now_export.csv", sep=";", header=T, quote = "", as.is=T)
dim(now)

pbdb=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/artiodactyla-PBDB/pbdb_data_artiodactyla.csv", sep=",", header=T, skip = 18, as.is=T)
dim(pbdb)

#remover ruminantes:
#1. The research we made in NOW database was made within all the Artiodactyla group. There is a vast number of families which should be removed from the data. Lets remove them. We will follow Cantalapiedra (2015) taxonomy:

#now database:
now=now[-(which(now$FAMILY=="Agriochoeridae")),]
now=now[-(which(now$FAMILY=="Anoplotheriidae")),]
now=now[-(which(now$FAMILY=="Anthracotheriidae")),]
now=now[-(which(now$FAMILY=="Cainotheriidae")),]
now=now[-(which(now$FAMILY=="Camelidae")),]
now=now[-(which(now$FAMILY=="Cebochoeridae")),]
now=now[-(which(now$FAMILY=="Dacrytheriidae")),]
now=now[-(which(now$FAMILY=="Dichobunidae")),]
now=now[-(which(now$FAMILY=="Entelodontidae")),]
now=now[-(which(now$FAMILY=="Haplobunodontidae")),]
now=now[-(which(now$FAMILY=="Hippopotamidae")),]
now=now[-(which(now$FAMILY=="incertae sedis")),]
now=now[-(which(now$FAMILY=="Merycoidodontidae")),]
now=now[-(which(now$FAMILY=="Mixtotheriidae")),]
now=now[-(which(now$FAMILY=="Palaeochoeridae")),]
now=now[-(which(now$FAMILY=="Protoceratidae")),]
now=now[-(which(now$FAMILY=="Raoellidae")),]
now=now[-(which(now$FAMILY=="Suidae")),]
now=now[-(which(now$FAMILY=="Tayassuidae")),] 
now=now[-(which(now$FAMILY=="Titanohyracidae")),] #note: these are hyracoids
now=now[-(which(now$FAMILY=="Xiphodontidae")),]

#PBDB database:
pbdb=pbdb[-(which(pbdb$family=="Agriochoeridae")),]
pbdb=pbdb[-(which(pbdb$family=="Achaenodontidae")),] #actually this are helohyidae (foss & prothero 2007), but still non-ruminant artiodactyls 
pbdb=pbdb[-(which(pbdb$family=="Amphimerycidae")),]
pbdb=pbdb[-(which(pbdb$family=="Anoplotheriidae")),]
pbdb=pbdb[-(which(pbdb$family=="Anthracotheriidae")),]
pbdb=pbdb[-(which(pbdb$family=="Cainotheriidae")),]
pbdb=pbdb[-(which(pbdb$family=="Camelidae")),]
pbdb=pbdb[-(which(pbdb$family=="Cebochoeridae")),]
pbdb=pbdb[-(which(pbdb$family=="Choeropotamidae")),]
pbdb=pbdb[-(which(pbdb$family=="Diacodexeidae")),]
pbdb=pbdb[-(which(pbdb$family=="Dichobunidae")),]
pbdb=pbdb[-(which(pbdb$family=="Entelodontidae")),]
pbdb=pbdb[-(which(pbdb$family=="Hippopotamidae")),]
pbdb=pbdb[-(which(pbdb$family=="Homacodontidae")),]
pbdb=pbdb[-(which(pbdb$family=="Leptochoeridae")),]
pbdb=pbdb[-(which(pbdb$family=="Merycoidodontidae")),]
pbdb=pbdb[-(which(pbdb$family=="Mixtotheriidae")),]
pbdb=pbdb[-(which(pbdb$family=="Palaeochoeridae")),]
pbdb=pbdb[-(which(pbdb$family=="Protoceratidae")),]
pbdb=pbdb[-(which(pbdb$family=="Raoellidae")),]
pbdb=pbdb[-(which(pbdb$family=="Sanitheriidae")),]
pbdb=pbdb[-(which(pbdb$family=="Suidae")),]
pbdb=pbdb[-(which(pbdb$family=="Tayassuidae")),]
pbdb=pbdb[-(which(pbdb$family=="Xiphodontidae")),]


dim(now)
dim(pbdb)