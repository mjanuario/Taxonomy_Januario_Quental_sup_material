source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/excluding_non_ruminants.R")
dim(now)
dim(pbdb)
#2. removing occurences wich have explicit taxonomy uncertainty


# now database:
now=now[-(which(now$ID_STATUS=="genus id uncertain")),]
now=now[-(which(now$ID_STATUS=="family id uncertain")),]
now=now[-(which(now$TAXON_STATUS=="taxonomic validity uncertain")),]
now=now[-(which(now$TAXON_STATUS=="informal species")),]
now=now[-(which(now$TAXON_STATUS=="genus attrib of species uncertain")),]
now=now[-(which(now$TAXON_STATUS=="family attrib of genus uncertain")),]
now=now[-(which(now$FAMILY=="indet.")),]
now=now[-(which(now$FAMILY=="Findout")),]
now=now[-which(now$GENUS=="indet.")]
now=now[-which(now$GENUS=="Indet.")]

#PBDB database:
pbdb=pbdb[-(which(pbdb$flags=="R")),]
pbdb=pbdb[-(which(pbdb$flags=="F")),]
pbdb=pbdb[-(which(pbdb$genus=="")),]

dim(now)
dim(pbdb)