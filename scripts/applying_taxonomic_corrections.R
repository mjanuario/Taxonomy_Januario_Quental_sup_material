#######################
# CORRECTING TAXONOMY #
#######################

source("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/scripts/removing_explicit_taxonomic_uncertainty_sp.R")
#one occ. have an important typo:
which(pbdb$gen_sp=="Rabaticeras_\"lemutai\"" )
pbdb$gen_sp[3036]<-"Rabaticeras_lemutai" #correcting this occurence


#reading the taxonomical reviews table:
tax=read.table("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/taxonomical_revision_updated_aug_2018.csv", sep=",", header=T, quote = "", as.is=T)
#checking
dim(tax)
colnames(tax)

####################################################
#separating the taxonomy dataset for each database:#
####################################################

#############################################
# generating free space in the data frames: #
#############################################

#NOTE: this vector creation is necessary

#for now:
dummy_now_category=rep(NA, times=length(now[,1]))
dummy_now_obs1=rep(NA, times=length(now[,1]))
dummy_now_obs2=rep(NA, times=length(now[,1]))
dummy_now_new_gen_sp=rep(NA, times=length(now[,1]))
dummy_now_new_family=rep(NA, times=length(now[,1]))
dummy_now_is_Ruminant=rep(NA, times=length(now[,1]))

#######################
#applying corrections in now:
#######################

for(i in 1:length(tax[,1]))
{
  places<-now$gen_sp==tax$database_name[i]
  dummy_now_new_gen_sp = replace(dummy_now_new_gen_sp, places,  tax$revised_name[i])
  dummy_now_new_family = replace(dummy_now_new_family, places, tax$revised_family[i])
  dummy_now_obs1 = replace(dummy_now_obs1, places, tax$obs1[i])
  dummy_now_obs2 = replace(dummy_now_obs2, places, tax$obs2[i])
  dummy_now_category = replace(dummy_now_category, places, tax$category[i])
  dummy_now_is_Ruminant = replace(dummy_now_is_Ruminant, places, tax$is_ruminant[i])
}

#puting all taxonomy together
now$revised_name=dummy_now_new_gen_sp
now$revised_family=dummy_now_new_family
now$obs1=dummy_now_obs1
now$obs2=dummy_now_obs2
now$category=dummy_now_category
now$is_ruminant=dummy_now_is_Ruminant

#checking (and showing the occurences with some correction)
idd=which(now$gen_sp!=now$revised_name)
idd=unique(idd, which(now$FAMILY!=now$revised_family))
head(now[idd,c(24,50,48,49)], n = 30)


#for pbdb:
dummy_pbdb_category=rep(NA, times=length(pbdb[,1]))
dummy_pbdb_obs1=rep(NA, times=length(pbdb[,1]))
dummy_pbdb_obs2=rep(NA, times=length(pbdb[,1]))
dummy_pbdb_new_gen_sp=rep(NA, times=length(pbdb[,1]))
dummy_pbdb_new_family=rep(NA, times=length(pbdb[,1]))
dummy_pbdb_is_Ruminant=rep(NA, times=length(pbdb[,1]))

#######################
#applying corrections in pbdb:
#######################

for(i in 1:length(tax[,1]))
{
  places<-pbdb$gen_sp==tax$database_name[i]
  dummy_pbdb_new_gen_sp = replace(dummy_pbdb_new_gen_sp, places,  tax$revised_name[i])
  dummy_pbdb_new_family = replace(dummy_pbdb_new_family, places, tax$revised_family[i])
  dummy_pbdb_obs1 = replace(dummy_pbdb_obs1, places, tax$obs1[i])
  dummy_pbdb_obs2 = replace(dummy_pbdb_obs2, places, tax$obs2[i])
  dummy_pbdb_category = replace(dummy_pbdb_category, places, tax$category[i])
  dummy_pbdb_is_Ruminant = replace(dummy_pbdb_is_Ruminant, places, tax$is_ruminant[i])
}

#puting all taxonomy together
pbdb$revised_name=dummy_pbdb_new_gen_sp
pbdb$revised_family=dummy_pbdb_new_family
pbdb$obs1=dummy_pbdb_obs1
pbdb$obs2=dummy_pbdb_obs2
pbdb$category=dummy_pbdb_category
pbdb$is_ruminant=dummy_pbdb_is_Ruminant



#checking (and showing the occurences with some correction)
idd=which(pbdb$gen_sp!=pbdb$revised_name)
idd=unique(idd, which(pbdb$family!=pbdb$revised_family))
head(pbdb[idd,c(23,145,143,144)], n = 30)

####################################
#applying a second review on data: #
####################################


dim(now)
dim(pbdb)

#removing invalid names (discovered as "invalid" only during taxonomical review):
pbdb=pbdb[-(which(pbdb$category=="invalid")),]
now=now[-(which(now$category=="invalid")),]

#removing non ruminant species:
pbdb=pbdb[-(which(pbdb$is_ruminant=="no")),]
unique(now$is_ruminant)#now have no non-ruminant species (discovered a posteriori)

#dimensions of each dataset:
dim(pbdb)
dim(now)

#cleaning dummy objects:
rm(list = c("dummy_now_category", "dummy_now_is_Ruminant", "dummy_now_new_family", "dummy_now_new_gen_sp", "dummy_now_obs1", "dummy_now_obs2", "dummy_pbdb_category", "dummy_pbdb_is_Ruminant", "dummy_pbdb_new_family", "dummy_pbdb_new_gen_sp", "dummy_pbdb_obs1", "dummy_pbdb_obs2", "i", "idd", "places", "tax"))
ls()