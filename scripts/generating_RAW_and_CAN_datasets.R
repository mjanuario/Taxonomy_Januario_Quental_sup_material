####################################################
########### FOR SPECIES ############################
####################################################

source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/removing_explicit_taxonomic_uncertainty_sp.R")

#######################################
#generating RAW dataset: ##############
#######################################
colnames(pbdb)[143]="Species"
colnames(pbdb)[13]="MinT"
colnames(pbdb)[12]="MaxT"
colnames(pbdb)[33]="lat"
colnames(pbdb)[32]="long"
#rounding lat and long:
pbdb$lat=round(pbdb$lat, digits = 2)
pbdb$long=round(pbdb$long, digits = 2)

#making pre-dataset:
pbdb_for_RAW=pbdb[,c(143,13,12,33,32)]
dim(pbdb_for_RAW)
#removing geographic replicates
pbdb_for_RAW=unique(pbdb_for_RAW) 
dim(pbdb_for_RAW)

colnames(now)
colnames(now)[48]="Species"
colnames(now)[11]="MinT"
colnames(now)[7]="MaxT"
colnames(now)[5]="lat"
colnames(now)[6]="long"
#rounding lat and long:
now$lat=round(now$lat, digits = 2)
now$long=round(now$long, digits = 2)

#making pre-dataset:
now_for_RAW=now[,c(48,11,7,5,6)]
dim(now_for_RAW)
#removing geographic replicates
now_for_RAW=unique(now_for_RAW) 
dim(now_for_RAW)

#merging datasets:
RAW_dataset=rbind(now_for_RAW, pbdb_for_RAW)
dim(RAW_dataset)
RAW_dataset=unique(RAW_dataset)
dim(RAW_dataset)

#adding which are extant and which are extinct species
require(ape)
trees=read.nexus("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Cantalapiedra_etal_ruminantia_trees.nex")
extant=trees[[1]]$tip.label
RAW_dataset$Status=RAW_dataset$Species %in% extant
RAW_dataset$Status[which(RAW_dataset$Status=="TRUE")]="extant"
RAW_dataset$Status[which(RAW_dataset$Status=="FALSE")]="extinct"
RAW_dataset=RAW_dataset[,c(1,6,2,3)]
head(RAW_dataset)
#######################################

#######################################
#generating CAN dataset: ##############
#######################################

can=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/cantalapiedra2015dataset.csv", sep=",", header=T)
can$Status=can$species %in% extant #adding which are extant and which are extinct species
can$Status[which(can$Status=="TRUE")]="extant"
can$Status[which(can$Status=="FALSE")]="extinct"
CAN_dataset=can[,c(4,7,6,5)]
colnames(CAN_dataset)=c("Species", "Status", "MinT", "MaxT") 

#final datasets:
head(RAW_dataset)
head(CAN_dataset)

#writing species datasets:
write.table(RAW_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/RAW/RAW_sp.tsv", sep="\t", quote = F, row.names = F)
write.table(CAN_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/CAN/CAN_sp.tsv", sep="\t", quote = F, row.names = F)

####################################################
########### FOR GENERA #############################
####################################################

source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/removing_explicit_taxonomic_uncertainty_gen.R")

#######################################
#generating RAW dataset: ##############
#######################################
pbdb_gen=pbdb[,c(25,13,12,33,32)]
colnames(pbdb_gen)=c("Species", "MinT", "MaxT", "lat", "long")#"Species" is a parameter for PyRate, so it is needed to assign the genera in column "Species" for this analysis

#rounding lat and long:
pbdb_gen$lat=round(pbdb_gen$lat, digits = 2)
pbdb_gen$long=round(pbdb_gen$long, digits = 2)
dim(pbdb_gen)
#removing geographic replicates:
pbdb_gen=unique(pbdb_gen)
dim(pbdb_gen)

now_gen=now[,c(26, 11,7,5,6)]
colnames(now_gen)=c("Species", "MinT", "MaxT", "lat", "long")#"Species" is a parameter for PyRate, so it is needed to assign the genera in column "Species" for this analysis

#rounding lat and long:
now_gen$lat=round(now_gen$lat, digits = 2)
now_gen$long=round(now_gen$long, digits = 2)
dim(now_gen)
#removing geographic replicates:
now_gen=unique(now_gen)
dim(now_gen)

#merging datasets:
RAW_gen_dataset=rbind(now_gen, pbdb_gen)
dim(RAW_gen_dataset)
RAW_gen_dataset=unique(RAW_gen_dataset)
dim(RAW_gen_dataset)

#adding which are extant and which are extinct species
require(ape)
trees=read.nexus("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Cantalapiedra_etal_ruminantia_trees.nex")
extant= unique(sub(pattern = "_.*", "", trees[[1]]$tip.label))
RAW_gen_dataset$Status=RAW_gen_dataset$Species %in% extant
RAW_gen_dataset$Status[which(RAW_gen_dataset$Status=="TRUE")]="extant"
RAW_gen_dataset$Status[which(RAW_gen_dataset$Status=="FALSE")]="extinct"
RAW_gen_dataset=RAW_gen_dataset[,c(1,6,2,3)]
head(RAW_gen_dataset)

#######################################
#generating CAN dataset: ##############
#######################################

can=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/cantalapiedra2015dataset.csv", sep=",", header=T)
can$species=can$genus
can$Status=can$species %in% extant #adding which are extant and which are extinct species
can$Status[which(can$Status=="TRUE")]="extant"
can$Status[which(can$Status=="FALSE")]="extinct"
CAN_gen_dataset=can[,c(4,7,6,5)]
colnames(CAN_gen_dataset)=c("Species", "Status", "MinT", "MaxT") 

#final datasets:
head(RAW_gen_dataset)
head(CAN_gen_dataset)

#writing genera datasets:
write.table(RAW_gen_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/RAW/RAW_gen.tsv", sep="\t", quote = F, row.names = F)
write.table(CAN_gen_dataset, file="~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/CAN/CAN_gen.tsv", sep="\t", quote = F, row.names = F)


####################################################
#Generating "pyrate.py" files:######################
####################################################
source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_things/pyrate_utilities.r")
ls()
reps=100


#generate 100 PyRate replicas:

#for CAN species
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/CAN")
extract.ages(file = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/CAN/CAN_sp.tsv", replicates = reps, cutoff = 15, random = T) 

#for RAW species
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/RAW")
extract.ages(file = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/RAW/RAW_sp.tsv", replicates = reps, cutoff = 15, random = T) 

#for CAN genera
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/CAN")
extract.ages(file = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/CAN/CAN_gen.tsv", replicates = reps, cutoff = 15, random = T) 

#for RAW genera
setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/RAW")
extract.ages(file = "~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data/RAW/RAW_gen.tsv", replicates = reps, cutoff = 15, random = T) 
