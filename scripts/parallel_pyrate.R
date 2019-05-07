#making run files:

#2400 runs, varing 3 datasets (raw, our, can) and 3 models (mG, qShift and mG+qShift)
runs=paste("python PyRate.py", #pyRate call
           rep(c("CAN_sp_PyRate.py -N 197 -j", "RAW_sp_PyRate.py -N 197 -j","OUR_sp_PyRate.py -N 197 -j","ALL_sp_PyRate.py -N 197 -j","CAN_gen_PyRate.py -N 197 -j", "RAW_gen_PyRate.py -N 197 -j","OUR_gen_PyRate.py -N 197 -j","ALL_gen_PyRate.py -N 197 -j"), each=300), #dataset variation
           1:100, #replicate variation
           paste("-out _", sprintf('%0.4d', 1:2400), sep=""), #number of replica and analysis
           rep(c("-mG", "-qShift", "-mG -qShift"), each=100), #model variation
           rep(c("", "intervals.txt", "intervals.txt"), each=100), #input or nor for qShift models
           "-A4 -pP 1.5 1.5  -s 100000 -b 10000000 -n 110000000 -p 1000000 -fR 0.1 -fS 0.3 -M 10 -tR 2 -fU 0.1 0.3 0 ") #mcmc parameters

#making slaves:
datasets=c(rep(c("cd OUR", "cd ALL", "cd OUR", "cd ALL"), each=50),rep(c("cd OUR", "cd ALL", "cd OUR", "cd ALL"), each=25))

runs=runs[c(601:1200,1801:2400)]


slaves=list()
for(i in 1:200)
{
  beg=(6*((i-1))+1)
  end=(6*i)
  slaves[[i]]=c(datasets[i], runs[beg:end])
}

#writing:
#setwd("~/Downloads/Taxonomy_Januario_Quental_supplementar_material/data/Pyrate_data")
#for(i in 1:200)
#{
#  write.table(slaves[[i]], file=paste("slave", i, ".sh", sep=""), row.names = F, col.names = F, quote = F)
#}


#making master sh file:
master1=paste("sh slave", 1:50, ".sh > erro", 1:50, ".txt &", sep="")
master2=paste("sh slave", 51:100, ".sh > erro", 51:100, ".txt &", sep="")
master3=paste("sh slave", 101:150, ".sh > erro", 101:150, ".txt &", sep="")
master4=paste("sh slave", 151:200, ".sh > erro", 151:200, ".txt &", sep="")

#writing:
#write.table(master1, "master1.sh", row.names = F, col.names = F, quote = F)
#write.table(master2, "master2.sh", row.names = F, col.names = F, quote = F)
#write.table(master3, "master3.sh", row.names = F, col.names = F, quote = F)
#write.table(master4, "master4.sh", row.names = F, col.names = F, quote = F)