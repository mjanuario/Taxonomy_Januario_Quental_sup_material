############################
#for speciation rates:
############################
files=paste0(rep(c("CAN", "RAW", "OUR", "ALL"), each=300),"_",
       rep(c("sp", "gen"), each=1200),"_",
       1:100,"_",
       sprintf('%0.4d', 1:2400),
       rep(c("_Grj", "rj", "_Grj"), each=1200),"_",
       "sp_rates.log"
       )

for(j in 0:23){
  aux=data.frame()
  for(i in 1:100){
    t=read.table(files[j*100+i], sep="\t", header = F, col.names = paste0('V', 1:50), fill = T)
    t=t[sample(1:1000, size = 100),]
    t[,51]=i
    aux=rbind(aux, t)
  }  
  write.table(paste0("sp_", j, ".tsv"), sep="\t", col.names = T, row.names = F)
}

############################
#for extinction rates:
############################
files=paste0(rep(c("CAN", "RAW", "OUR", "ALL"), each=300),"_",
             rep(c("sp", "gen"), each=1200),"_",
             1:100,"_",
             sprintf('%0.4d', 1:2400),
             rep(c("_Grj", "rj", "_Grj"), each=1200),"_",
             "ex_rates.log"
)

for(j in 0:23){
  aux=data.frame()
  for(i in 1:100){
    t=read.table(files[j*100+i], sep="\t", header = F, col.names = paste0('V', 1:50), fill = T)
    t=t[sample(1:1000, size = 100),]
    t[,51]=i
    aux=rbind(aux, t)
  }  
  write.table(paste0("ex_", j, ".tsv"), sep="\t", col.names = T, row.names = F)
}

############################
#for mcmcs:
############################
files=paste0(rep(c("CAN", "RAW", "OUR", "ALL"), each=300),"_",
             rep(c("sp", "gen"), each=1200),"_",
             1:100,"_",
             sprintf('%0.4d', 1:2400),
             rep(c("_Grj", "rj", "_Grj"), each=1200),"_",
             "mcmc.log"
)

for(j in 0:23){
  aux=data.frame()
  for(i in 1:100){
    t=read.table(files[j*100+i], sep="\t", header = F, col.names = paste0('V', 1:50), fill = T)
    t=t[sample(1:1000, size = 100),]
    t[,51]=i
    aux=rbind(aux, t)
  }  
  write.table(paste0("mcmc_", j, ".tsv"), sep="\t", col.names = T, row.names = F)
}





