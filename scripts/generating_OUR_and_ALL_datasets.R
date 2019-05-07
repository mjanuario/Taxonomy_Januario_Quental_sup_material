#######################################
#generating ALL dataset: ##############
#######################################


####################################################
########### FOR SPECIES ############################
####################################################
source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/excluding_non_ruminants.R")
pbdb$gen_sp=paste(pbdb$genus, pbdb$species_name, sep="_")
now$gen_sp=paste(now$GENUS, now$SPECIES, sep="_")

pbdb_sp=pbdb[,c(143,13,12)]
colnames(pbdb_sp)=c("Species", "MinT", "MaxT")#"Species" is a parameter for PyRate, so it is needed to assign the genera in column "Species" for this analysis

now_sp=now[,c(48, 11,7)]
colnames(now_sp)=c("Species", "MinT", "MaxT")#"Species" is a parameter for PyRate, so it is needed to assign the genera in column "Species" for this analysis

#merging datasets:
ALL_sp_dataset=rbind(now_sp, pbdb_sp)
dim(ALL_sp_dataset)
head(ALL_sp_dataset)

#adding which are extant and which are extinct species
require(ape)
trees=read.nexus("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Cantalapiedra_etal_ruminantia_trees.nex")
extant=trees[[1]]$tip.label
ALL_sp_dataset$Status=ALL_sp_dataset$Species %in% extant
ALL_sp_dataset$Status[which(ALL_sp_dataset$Status=="TRUE")]="extant"
ALL_sp_dataset$Status[which(ALL_sp_dataset$Status=="FALSE")]="extinct"
ALL_sp_dataset=ALL_sp_dataset[,c(1,4,2,3)]
head(ALL_sp_dataset)

#one species have an incorrect name. We must correct it:
ALL_sp_dataset$Species[5572]
ALL_sp_dataset$Species[5572]="Cervus_seneze"
#ALL_sp_dataset$Species[grepl(ALL_sp_dataset$Species, pattern = "\xe9")]

#writing data:
write.table(ALL_sp_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/ALL_sp.tsv", sep="\t", quote = F, row.names = F, fileEncoding = )


####################################################
########### FOR GENERA #############################
####################################################
source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/excluding_non_ruminants.R")


pbdb_gen=pbdb[,c(25,13,12)]
colnames(pbdb_gen)=c("Species", "MinT", "MaxT")#"Species" is a parameter for PyRate, so it is needed to assign the genera in column "Species" for this analysis

now_gen=now[,c(26, 11,7)]
colnames(now_gen)=c("Species", "MinT", "MaxT")#"Species" is a parameter for PyRate, so it is needed to assign the genera in column "Species" for this analysis

#merging datasets:
ALL_gen_dataset=rbind(now_gen, pbdb_gen)
dim(ALL_gen_dataset)

#adding which are extant and which are extinct species
require(ape)
trees=read.nexus("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Cantalapiedra_etal_ruminantia_trees.nex")
extant= unique(sub(pattern = "_.*", "", trees[[1]]$tip.label))
ALL_gen_dataset$Status=ALL_gen_dataset$Species %in% extant
ALL_gen_dataset$Status[which(ALL_gen_dataset$Status=="TRUE")]="extant"
ALL_gen_dataset$Status[which(ALL_gen_dataset$Status=="FALSE")]="extinct"
ALL_gen_dataset=ALL_gen_dataset[,c(1,4,2,3)]
head(ALL_gen_dataset)

write.table(ALL_gen_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/ALL_gen.tsv", sep="\t", quote = F, row.names = F)


#######################################
#generating OUR dataset: ##############
#######################################

####################################################
########### FOR SPECIES ############################
####################################################

source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/applying_taxonomic_corrections.R")

#PBDB:
#rounding lat and long:
pbdb$lat_r=round(pbdb$lat, digits = 2)
pbdb$lng_r=round(pbdb$lng, digits = 2)

#subsetting the data
pbdb_for_OUR=pbdb[,c(144,13,12,150,151)]

#remove geografical replicates:
dim(pbdb_for_OUR)
pbdb_for_OUR=unique(pbdb_for_OUR)
dim(pbdb_for_OUR)
colnames(pbdb_for_OUR)=c("Species", "MinT", "MaxT", "lat", "long")

#NOW:
#rounding lat and long:
now$lat_r=round(now$LAT, digits = 2)
now$lng_r=round(now$LONG, digits = 2)

#subsetting the data
now_for_OUR=now[,c(49,11,7,55,56)]

#remove geografical replicates:
dim(now_for_OUR)
now_for_OUR=unique(now_for_OUR)
dim(now_for_OUR)
colnames(now_for_OUR)=c("Species", "MinT", "MaxT", "lat", "long")


#remove geografical replicates between datasets:
OUR_data=rbind(now_for_OUR, pbdb_for_OUR)

#remove geographical replicates:
dim(OUR_data)
OUR_data=unique(OUR_data)
dim(OUR_data)

#removing spacial data:
OUR_sp_dataset=OUR_data[,1:3]

#adding which are extant and which are extinct species
require(ape)
trees=read.nexus("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Cantalapiedra_etal_ruminantia_trees.nex")
extant=trees[[1]]$tip.label
OUR_sp_dataset$Status=OUR_sp_dataset$Species %in% extant
OUR_sp_dataset$Status[which(OUR_sp_dataset$Status=="TRUE")]="extant"
OUR_sp_dataset$Status[which(OUR_sp_dataset$Status=="FALSE")]="extinct"
OUR_sp_dataset=OUR_sp_dataset[,c(1,4,2,3)]
head(OUR_sp_dataset)
#######################################

write.table(OUR_sp_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/OUR_sp.tsv", sep="\t", quote = F, row.names = F)


####################################################
########### FOR GENERA ############################
####################################################

source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/applying_taxonomic_corrections.R")


#PBDB:
#rounding lat and long:
pbdb$lat_r=round(pbdb$lat, digits = 2)
pbdb$lng_r=round(pbdb$lng, digits = 2)

#subsetting the data
pbdb_for_OUR=pbdb[,c(144,13,12,150,151)]
head(pbdb_for_OUR)

#remove geografical replicates:
dim(pbdb_for_OUR)
pbdb_for_OUR=unique(pbdb_for_OUR)
dim(pbdb_for_OUR)
colnames(pbdb_for_OUR)=c("Species", "MinT", "MaxT", "lat", "long")

#NOW:
#rounding lat and long:
now$lat_r=round(now$LAT, digits = 2)
now$lng_r=round(now$LONG, digits = 2)

#subsetting the data
now_for_OUR=now[,c(49,11,7,55,56)]

#remove geografical replicates:
dim(now_for_OUR)
now_for_OUR=unique(now_for_OUR)
dim(now_for_OUR)
colnames(now_for_OUR)=c("Species", "MinT", "MaxT", "lat", "long")


#merging datasetsremove geografical replicates between datasets:
OUR_data=rbind(now_for_OUR, pbdb_for_OUR)

#remove geographical replicates:
dim(OUR_data)
OUR_data=unique(OUR_data)
dim(OUR_data)

#removing spacial data:
OUR_dataset=OUR_data[,1:3]

#removing specific form all names:
OUR_dataset$Species=sub(pattern = "_.*", "", OUR_dataset$Species)
OUR_gen_dataset=OUR_dataset
head(OUR_gen_dataset)

#adding which are extant and which are extinct species
require(ape)
trees=read.nexus("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Cantalapiedra_etal_ruminantia_trees.nex")
extant= unique(sub(pattern = "_.*", "", trees[[1]]$tip.label))
OUR_gen_dataset$Status=OUR_gen_dataset$Species %in% extant
OUR_gen_dataset$Status[which(OUR_gen_dataset$Status=="TRUE")]="extant"
OUR_gen_dataset$Status[which(OUR_gen_dataset$Status=="FALSE")]="extinct"
OUR_gen_dataset=OUR_gen_dataset[,c(1,4,2,3)]
head(OUR_gen_dataset)
#######################################

#WRITING DATASETS 
write.table(OUR_gen_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/OUR_gen.tsv", sep="\t", quote = F, row.names = F)



####################################################
#Generating "pyrate.py" files:######################
####################################################
source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_things/pyrate_utilities.r")
ls()
reps=100


#generate 100 PyRate replicas:

#for ALL species
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/ALL")
extract.ages(file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/ALL/ALL_sp.tsv", replicates = reps, cutoff = 15, random = T) 

#for OUR species
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR")
extract.ages(file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/OUR_sp.tsv", replicates = reps, cutoff = 15, random = T) 

#for ALL genera
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/ALL")
extract.ages(file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/ALL/ALL_gen.tsv", replicates = reps, cutoff = 15, random = T) 

#for OUR genera
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR")
extract.ages(file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/OUR/OUR_gen.tsv", replicates = reps, cutoff = 15, random = T) 
