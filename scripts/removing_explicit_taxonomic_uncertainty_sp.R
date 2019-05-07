source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/excluding_non_ruminants.R")
dim(now)
dim(pbdb)
#2. removing occurences wich have explicit taxonomy uncertainty


# now database:
now=now[-(which(now$ID_STATUS=="genus id uncertain")),]
now=now[-(which(now$ID_STATUS=="species id uncertain")),]
now=now[-(which(now$ID_STATUS=="family id uncertain")),]
now=now[-(which(now$TAXON_STATUS=="taxonomic validity uncertain")),]
now=now[-(which(now$TAXON_STATUS=="informal species")),]
now=now[-(which(now$TAXON_STATUS=="genus attrib of species uncertain")),]
now=now[-(which(now$SPECIES=="sp.")),]
now=now[-(which(now$SPECIES=="indet")),]
now=now[-(which(now$SPECIES=="indet.")),]
now=now[-(which(now$FAMILY=="indet.")),]
now=now[-(which(now$FAMILY=="Findout")),]
dim(now)

#PBDB database:
pbdb=pbdb[-(which(pbdb$flags=="R")),]
pbdb=pbdb[-(which(pbdb$flags=="F")),]
pbdb=pbdb[-(which(pbdb$species_reso=="cf.")),]
pbdb=pbdb[-(which(pbdb$species_reso=="aff.")),]
pbdb=pbdb[-(which(pbdb$species_reso=="?")),]
pbdb=pbdb[-(which(pbdb$species_reso=="informal")),]
pbdb=pbdb[-(which(pbdb$species_reso=="\"")),]
pbdb=pbdb[-(which(pbdb$species_reso=="ex gr.")),]
pbdb=pbdb[-(which(pbdb$species_name=="? sp.")),]
pbdb=pbdb[-(which(pbdb$species_name=="sp. 1")),]
pbdb=pbdb[-(which(pbdb$species_name=="sp. 2")),]
pbdb=pbdb[-(which(pbdb$species_name=="indet.")),]
pbdb=pbdb[-(which(pbdb$species_name=="indeterminate")),]
pbdb=pbdb[-(which(pbdb$species_name=="sp.")),]
pbdb=pbdb[-(which(pbdb$species_name=="spp.")),]
pbdb=pbdb[-(which(pbdb$primary_reso=="?")),]
pbdb=pbdb[-(which(pbdb$primary_reso=="\"")),]
pbdb=pbdb[-(which(pbdb$primary_reso=="aff.")),]
pbdb=pbdb[-(which(pbdb$primary_reso=="cf.")),]
pbdb=pbdb[-(which(pbdb$primary_reso=="informal")),]
pbdb=pbdb[-(which(pbdb$primary_reso=="n. gen.")),]
dim(pbdb)


pbdb$gen_sp=paste(pbdb$genus, pbdb$species_name, sep="_")
now$gen_sp=paste(now$GENUS, now$SPECIES, sep="_")
