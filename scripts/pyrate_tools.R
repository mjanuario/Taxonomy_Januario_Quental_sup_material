################################################################################
#                                 PACKAGES:                                    #
require(LaplacesDemon)
require(TeachingDemos)
require(scales)
require(DescTools)
library(tidyr)
################################################################################

################################################################################
#                                FIRST FUNCTION:                               #
################################################################################

read.pyrate.rates <- function(file) {
  
  columnames <- paste0('V', 1:50) #creating an arbitrarily large number of columns
  data <- read.table(file, header = F, sep = "\t", col.names = columnames, fill = T)
  return(data)
  
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

make.bins <- function(rate.pyrate, max_time, min_time, resolution){
  
if(resolution>1){stop("resolution cannot have a value larger than 1")}
  
  x <- unlist(rate.pyrate) 
  x[length(x)+1]  <- floor(sum(!is.na(x))/2) # calculating number of shifts
  
if (x[length(x)] == 0) { # if rate has no shifts:
      res <- rep(x[1], times = length(seq(from=max_time, to = min_time, by = resolution*-1)))
} else {
    
    #only rates estimated for a given PyRate iteration
    rates <- x[1:(x[length(x)] + 1)] 
    
    #calculating only rate Shift times (in discrete time) estimated for a given PyRate iteration
    if(resolution<1){ 
      times_s <- round(c(max_time, x[seq(length(rates) + 1, length(rates) + x[length(x)])], min_time), match(TRUE, round(resolution, 1:20) == resolution)) 
    }else{
      times_s <- round(c(max_time, x[seq(length(rates) + 1, length(rates) + x[length(x)])], min_time),0)   
    } 
    
    index <- diff(times_s) * -10
    res <- rep(rates, times = index)}
  
  #sometimes the rounding this functions does creates imprecision. The follwoing code tries to correct for this:
  while(length(res) < length(seq(min_time,max_time, by = resolution))){
    res <- c(res, rates[length(rates)])
  }
  #print(paste("order of coluns is ", max_time, "-0 by ", resolution, sep=""))
  return(setNames(res, NULL))
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

make.RTT=function(data, max_time, resolution, min_time){
  
  ########################################
  #function HELP
  
  #NOTE: this function generates Rates Through Time raw data. At first, this function may seem a bit useless, but is needed to calculate Net-Diversification rates.
  
  #Arguments:
  #data = a PyRate rates object (as the ones read by the read.pyrate.rates function)
  #max_time = The early bound of the group. This can be an arbitrary number guessed
  #min_time = The late bound of the group. This can be an arbitrary number guessed
  #resolution = The resolution, in milion years, of the data. Must be equal or smaller than 1.
  
  ########################################
  
  proc.data=proc.data=t(apply(X=data, 1, make.bins, max_time = max_time, min_time=min_time, resolution = resolution))
  colnames(proc.data)=as.character(round(seq(max_time, min_time, by=-(resolution)), digits = 1))
  return(proc.data)
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################
calc.r=function(proc.data_lam, proc.data_mu){
  ########################################
  #function HELP
  
  #this function calculates Net Diversification rates for make.bins objects
  
  #Arguments:
  #data_lam = a make.RTT object containing speciation rates. Please see make.RTT function help
  #data_mu  = a make.RTT object containing extinction rates. Please see make.RTT function help
  ########################################
  
  
  
  data_r=proc.data_lam - proc.data_mu
  return(data_r)
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

make.HPD.RTT=function(data, prob, max=F){
  ########################################
  #function HELP
  
  #this function calculates HPD and median statistics for make.bins objects
  
  #Arguments:
  #data = a make.bins object
  #prob = confidence level of intervals 
  #max = maximum threshold that is interpretable. If the max interval of the HPD exceeds this value, the interval will not be included in the output
  ########################################
  
  data=as.data.frame(data) #preparing data
  
  for_plot=data.frame(matrix(data = NA, nrow=ncol(data), ncol=4))# creating a data.frame to store  fig parameters
  colnames(for_plot)=c("mya", "median", "low", "high")
  
  #calculating HPD for each iteraction in the PyRate output:
  for(i in 1:ncol(data)){
    for_plot$low[i] = emp.hpd(data[,i], conf = prob)[1]
    for_plot$high[i] = emp.hpd(data[,i], conf = prob)[2]
    for_plot$median[i] = median(data[,i])
    for_plot$mya[i]=as.numeric(colnames(data)[i])
  }
  if(class(max)=="numeric"){
    for_plot[which(for_plot$high>max,2:4),c(2,3,4)]=NA
  }
  return(for_plot)
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

plot.pyrate=function(HPD, xlim, ylim, col, alpha, lwd, ylab, xlab, qShift=F, qShift_alpha, main){
  ########################################
  #function HELP
  
  #this function plots make.HPD.RTT objects
  
  #Arguments:
  #HPD = a list object containing one or more make.HPD.RTT object(s)
  #xlim = x-axis limits. Vector containing two values
  #ylim = y-axis limits. Vector containing two values
  #col = color(s) to be plotted
  #alpha = alpha for the different HPDs boundaries
  #lwd = line width. one single value
  #overlap = mark where intervals overlap. logical.
  ########################################
  
  
  if(class(qShift)=="numeric"){
    plot(x = as.numeric(HPD[[1]]$mya), y=HPD[[1]]$median, type="l", xlim = xlim, ylim=ylim, col="white", lwd=lwd, xlab = xlab, ylab = ylab, main=main)
    polygon(c(as.numeric(HPD[[1]]$mya), rev(as.numeric(HPD[[1]]$mya))), c(HPD[[1]]$high, rev(HPD[[1]]$low)), col = alpha("white", alpha[1]), border = NA)  
    add.int(qShift=qShift, xlim=xlim, ylim=ylim+c(-2, +2), alpha=qShift_alpha)
    for(i in 1:length(HPD)){
      lines(x = as.numeric(HPD[[i]]$mya), y=HPD[[i]]$median, col=col[i], lwd=lwd)
      polygon(c(as.numeric(HPD[[i]]$mya), rev(as.numeric(HPD[[i]]$mya))), c(HPD[[i]]$high, rev(HPD[[i]]$low)), col = alpha(col[i], alpha[i]), border = NA)  
    }
    
  }else{
  
  plot(x = as.numeric(HPD[[1]]$mya), y=HPD[[1]]$median, type="l", xlim = xlim, ylim=ylim, col=col[1], lwd=lwd, xlab = xlab, ylab = ylab, main=main, axes=F)
    Axis(side=1, labels=T)
    Axis(side=2, labels=T)
  polygon(c(as.numeric(HPD[[1]]$mya), rev(as.numeric(HPD[[1]]$mya))), c(HPD[[1]]$high, rev(HPD[[1]]$low)), col = alpha(col[1], alpha[1]), border = NA)  
  
  #adding the other HPDs
    if(length(HPD)>1){
      for(i in 2:length(HPD)){
      lines(x = as.numeric(HPD[[i]]$mya), y=HPD[[i]]$median, col=col[i], lwd=lwd)
      polygon(c(as.numeric(HPD[[i]]$mya), rev(as.numeric(HPD[[i]]$mya))), c(HPD[[i]]$high, rev(HPD[[i]]$low)), col = alpha(col[i], alpha[i]), border = NA)  
      }
    }
  }
}
  
################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

get.prior.shift=function(max_time, min_time, resolution){
  
  ########################################
  #function HELP
  
  #this function calculates BF=2 and BF=6 for empirical distributions of rate shifts in PyRate
  
  #Arguments:
  #max_time = The early bound of the group. This can be an arbitrary number guessed
  #min_time=The late bound of the group. This can be an arbitrary number guessed
  #resolution = The resolution, in milion years, of the data. Must be equal or smaller than 1.
  ########################################
  
  times_of_shift=NULL
  sampled_k=NULL
  #gamma hyper-pior
  G_shape=2 #default of pyrate
  G_rate=1 #default of pyrate
  min_time_frame_size=1
  iteration=0
  print("computing empirical priors. Please wait")
  for(i in 1:100){
    #try(
    poi_lambda=rgamma(n = 1, shape = G_shape, rate = G_rate)
    n_rates_temp=rpois(n=1000, lambda = poi_lambda)
    n_rates= n_rates_temp[n_rates_temp>0][1]
    shift_times=runif(n = n_rates-1, min = min_time+resolution, max = max_time-resolution)
    time_frames=sort(c(max_time, min_time, shift_times))
    
    if(min(diff(time_frames))<min_time_frame_size){
      iteration=iteration #no suceed iteration happened
    }else{
      iteration=iteration+1
      times_of_shift=c(times_of_shift, shift_times)
      sampled_k=c(sampled_k, n_rates)
    }
  }
  #)
  #expected_K= ?????? DUNNO
  prior_s=mean(table(round(x = times_of_shift, digits=match(TRUE, round(resolution, 1:20) == resolution)))/iteration)
  
  calc_BF=function(threshold, emp_prior){
    A = exp(threshold/2)*emp_prior/(1-emp_prior)
    return( A/(A+1) )
  }
  
  bf2=calc_BF(2, prior_s)
  bf6=calc_BF(6, prior_s)
  results=c(prior_s, bf2, bf6)
  names(results)=c("P(s)", "BF=2", "BF=6")
  return(results)
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################


extract.shifts <- function(x, max_time, min_time, resolution){
  
  ########################################
  #function HELP
  
  #this function extracts empirical discrete-timed shifts for iterations of PyRate estimatives in continuous time. Its output is a vector with the times of rate shifts for a given PyRate iteraction
  
  #Arguments:
  #x = a row of the pyrate inputs of speciation or extinction rates
  #max_time = The early bound of the group. This can be an arbitrary number guessed
  #resolution = The resolution, in milion years, of the data. Must be equal or smaller than 1.
  ########################################
  
  x <- unlist(x) 
  
  #calculating number of shifts:
  x[length(x)+1]  <- floor(sum(!is.na(x))/2) 
  
  #extracting these shifts:
  if(x[length(x)]==0){
    if(resolution>1){stop("resolution cannot have a >1 value")
    }else{
      #res=rep(x[1], times=length(seq(from=max_time, to=min_time, by=resolution*-1)))
      times_s=NA
    }
    
    
  }else{
    
    #only rates estimated for a given PyRate iteration
    rates <- x[1:(x[length(x)] + 1)] 
    
    #calculating only rate Shift times (in discrete time) estimated for a given PyRate iteration
    if(resolution>1){stop("resolution cannot have a >1 value")}
    if(resolution<1){ 
      times_s <- round(c(max_time, x[seq(length(rates) + 1, length(rates) + x[length(x)])], min_time), match(TRUE, round(resolution, 1:20) == resolution)) 
    }else{
      times_s <- round(c(max_time, x[seq(length(rates) + 1, length(rates) + x[length(x)])], min_time),0)   
    } 
    times_s=times_s[-c(which(times_s==max_time), which(times_s==min_time))]
  }
  return(times_s)
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

plot.shifts=function(x, max_time, resolution, xlim, color, min_time, print=T){
  ########################################
  #function HELP
  
  #this function plots rate shifts from empirical discrete-timed rates for iterations of PyRate estimatives in continuous time. Its output is a histogram with the frequency of rate shifts and their logBF=2 and logBF=6, as implemented in PyRate (Silvestro et al, in press)
  
  #Arguments:
  #x = a row of the pyrate inputs of speciation or extinction rates
  #max_time = The early bound of the group. This can be an arbitrary number guessed
  #resolution = The resolution, in milion years, of the data. Must be equal or smaller than 1.
  #xlim = x-axis limits. Vector containing two values
  #color = color of bars in the histogram
  
  ########################################
  obj=unlist(apply(x, 1, extract.shifts, resolution=resolution, max_time=max_time, min_time=min_time))
  
  #computing BFs:
  BFs=get.prior.shift(max_time=max_time, min_time=min_time, resolution = resolution) 
  
    variables=hist(obj, breaks = seq(from=max_time, to=min_time, by=-resolution),xlim = xlim, border = color, col=color, main="", xlab="Mya", probability = T, ylim=c(0, 1.5*BFs[3]), plot = print, warn.unused = print)
    
    if(print==TRUE){
      #ploting BFs
      abline(h=BFs[3], col=color, lty=5) #BF=6
      abline(h=BFs[2], col=color, lty=3) #BF=2
      print(BFs)  
    }
  
  return(list(BFs, shifts=list(larg_bf6=variables$mids[which(variables$density >= BFs[3])], larg_bf2=variables$mids[which(variables$density >= BFs[2])])))
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

calc.HPD.overlap=function(HPD, compact=F){
  ########################################
  #function HELP
  
  #this function plots make.HPD.RTT objects
  
  #Arguments:
  #HPD = a list object containing one or more make.HPD.RTT object(s)
  #compact = a logical telling if the results should be presented in a vectorial data frame (F) or a in a more compact form (T)
  ########################################
  
  if(length(HPD)<2){stop("you must enter at least 2 make.HPD.RTT objects to calculate overlaps")}
  
  overlaps=data.frame(matrix(nrow=nrow(HPD[[1]]), ncol=sum(1:(length(HPD)-1)), data = NA))
  
  cc=combn(letters[1:length(HPD)], 2)
  names=vector()
  if(length(HPD)>2){#if there is only one combination of overlpas, the function crashes and this is necessary
    ks=dim(cc)[2]
    for(k in 1:ks){
      names=c(names, paste0(cc[1,k], cc[2,k]))
    }
    colnames(overlaps)=names
  }else{ 
    colnames(overlaps)="ab"
  }
  
  print(paste("The HPDs  will be assigned to letters in the order that you provided"))
  
  #creating a data frame with data to be peeled
  obj=HPD[[1]][,c(1,3,4)]#creating an HPD storage:
  for(g in 2:length(HPD)){obj=cbind(obj, HPD[[g]][,c(3,4)])}
  colnames(obj)=c("mya", paste(rep(c("low", "high"), times=length(HPD)), rep(letters[1:length(HPD)], each=2), sep="_") )
  
  ids=seq(from=3, to=ncol(obj), by=2)
  comps=combn(ids, 2)
  for(f in 1:ncol(overlaps)){
    a=obj[,c(comps[1,f]-1, comps[1,f])]
    b=obj[,c(comps[2,f]-1, comps[2,f])]
    data=cbind(a,b)
    overlaps[,f]=apply(data, 1, function(x){return(x[1:2]%overlaps%x[3:4])})
  }
  overlaps$time=HPD[[1]]$mya
  overlaps=overlaps[,c(ncol(overlaps), 1:(ncol(overlaps)-1))]
  if(compact==F){
    return(overlaps)   
  }else{
    ngroups=ncol(overlaps)-1
    compact=list()
    for(i in 1:ngroups){
      v=na.omit(overlaps$time[!(overlaps[,i+1])])
      total=length(v)/((nrow(overlaps)-1)/max(overlaps$time) )
      aux=round(diff(v), digits = 1)
      if(length(aux)>0){
        ids=which(aux< -0.1)
        aux=v[sort(c(1, ids, ids+1, length(v)))]
        res=paste0(aux[seq(1, length(aux), by=2)], " - ", aux[seq(2, length(aux), by=2)])
      }else{
        res="complete overlap"
      }
      res=c(res, paste0("total = ", total, " m.y." ))
      compact[[i]]=res
    }
    names(compact)=colnames(overlaps)[-1]
    print("There is NO overlap within the following limits:")
    return(compact)
  }
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

calc.DTT=function(mcmc, max_time, min_time, resolution, stat, cumulative=F){
  
  ########################################
  #function HELP
  
  #this function calculates diversity thought time statistics from pyrate mcmc out put files. It returns two vectors: time bins and richness for each bin
  
  #Arguments:
  #mcmc = a complete MCMC output from pyrate containing all parameters
  #max_time = The early bound of the group. This can be an arbitrary number guessed
  #min_time = The late bound of the group. This can be an arbitrary number guessed
  #resolution = The resolution, in milion years, of the data. Must be equal or smaller than 1.
  #stat = statistics used in TS and TE posteriors to calculate richness. "md" for median, "me" for mean, "mo" for baysean-estimated mode, and "all" for the use of all 3 statistics. 
  
  ########################################
  
  if(sum(stat==c("md", "me", "mo", "all"))<1){stop("please enter one of the follwing statistics: \"md\", \"me\" \"mo\" or \"all\" ")}
  
  id=which(colnames(mcmc)=="tot_length")
  spp=mcmc[,(id+1):ncol(mcmc)]
  aux=dim(spp)[2]/2 
  data_pr=as.data.frame(matrix(NA, nrow = aux, ncol=7)) 
  colnames(data_pr)=c("name", "TS_md", "TE_md", "TS_me", "TE_me", "TS_mo", "TE_mo") 
  
  
  
  getmode <- function(x) {
    res=tryCatch(hdr(x, prob = 95), error=function(x) return(0))#this modification is necessary because when a species is extant its TE is truncated in zero with no range in the distribution of TE, which crashes the hdr function
    if(class(res)=="numeric"){mode=res}else{mode=res$mode}
    return(mode)
    }
  
  
    names1=gsub("_.*", "", colnames(spp)[1:aux])
    names2=gsub("_.*", "", colnames(spp)[(aux+1):ncol(spp)])           
    if(sum(names1 != names2)>0){stop("names from TS and TE do not match. Please check your mcmc input")}else{
      
      
      data_pr$name=gsub("_.*", "", colnames(spp)[1:aux])
      time=seq(from=max_time, to=min_time, by=-resolution)
      
      if(stat=="md"){
      data_pr$TS_md=apply(X = spp[,1:aux], 2, median)
      data_pr$TE_md=apply(X = spp[,(aux+1):ncol(spp)], 2, median)
      rich=rep(NA, times=length(time))
      if(cumulative==T){
        for(i in time){rich[which(time==i)]= sum(data_pr$TS_md> i)}  
      }else{
        for(i in time){rich[which(time==i)]= sum(data_pr$TS_md> i & data_pr$TE_md<= i)}  
      }
      res=data.frame(time=time, rich_md=rich)
      }
      
      if(stat=="me"){
      data_pr$TS_me=apply(X = spp[,1:aux], 2, mean)
      data_pr$TE_me=apply(X = spp[,(aux+1):ncol(spp)], 2, mean)
      rich=rep(NA, times=length(time))
      if(cumulative==T){
        for(i in time){rich[which(time==i)]= sum(data_pr$TS_me> i)}  
      }else{
      for(i in time){rich[which(time==i)]= sum(data_pr$TS_me> i & data_pr$TE_me<= i)}
      }
      res=data.frame(time=time, rich_me=rich)
      }
      
      if(stat=="mo"){
      print("please be patient. Calculating the baysean-estimated mode takes considerably more time")
      data_pr$TS_mo=apply(X = spp[,1:aux], 2, getmode)
      data_pr$TE_mo=apply(X = spp[,(aux+1):ncol(spp)], 2, getmode)  
      rich=rep(NA, times=length(time))
      for(i in time){rich[which(time==i)]= sum(data_pr$TS_mo> i & data_pr$TE_mo<= i)}
      res=data.frame(time=time, rich_mo=rich)
      } 
      
      if(stat=="all"){
      data_pr$TS_md=apply(X = spp[,1:aux], 2, median)
      data_pr$TE_md=apply(X = spp[,(aux+1):ncol(spp)], 2, median)
      
      data_pr$TS_me=apply(X = spp[,1:aux], 2, mean)
      data_pr$TE_me=apply(X = spp[,(aux+1):ncol(spp)], 2, mean)
      
      print("please be patient. Calculating the baysean-estimated mode takes considerably more time")
      data_pr$TS_mo=apply(X = spp[,1:aux], 2, getmode)
      data_pr$TE_mo=apply(X = spp[,(aux+1):ncol(spp)], 2, getmode) 
      
      rich_md=rep(NA, times=length(time))
      rich_me=rep(NA, times=length(time))
      rich_mo=rep(NA, times=length(time))
      
      for(i in time){rich_md[which(time==i)]= sum(data_pr$TS_md> i & data_pr$TE_md<= i)}
      for(i in time){rich_me[which(time==i)]= sum(data_pr$TS_me> i & data_pr$TE_me<= i)}
      for(i in time){rich_mo[which(time==i)]= sum(data_pr$TS_mo> i & data_pr$TE_mo<= i)}
      
      res=data.frame(time=time, rich_md=rich_md, rich_me=rich_me, rich_mo=rich_mo)
      }
    }
    return(res)
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

add.shifts.HDR=function(raw.data, HDR, col, y){
  
  ########################################
  #function HELP
  
  #this function adds shifts in a plot.pyrate plot. low-significance shifts (BF=2) are plotted with lines and high significant (BF=6) shifts are plotted with squares.
  
  #Arguments:
  #raw.data = a PyRate rates object (as the ones read by the read.pyrate.rates function)
  #HDR = a make.HPD.RTT object
  #col = the color in which the shifts will be plotted
  #y = the y-axis position in which the shifts will be plotted
  
  ########################################
  
  
  #taking parameters:
  max_time=max(HDR$mya)
  min_time=min(HDR$mya)
  resolution=max_time/(nrow(HDR)-1)
  time_NAs=HDR$mya[which(is.na(HDR$high))]
  
  #calculating shifts
  sfts=plot.shifts(raw.data, max_time = max_time, resolution = .1, min_time = min_time, print = F)
  
  #removing NAs
  sfts$shifts$larg_bf2=sfts$shifts$larg_bf2[!round(sfts$shifts$larg_bf2-.05, digits = 1) %in% time_NAs]
  sfts$shifts$larg_bf6=sfts$shifts$larg_bf6[!round(sfts$shifts$larg_bf6-.05, digits = 1) %in% time_NAs]
  
  #ploting shifts
  #segments(x0 = sfts$shifts$larg_bf2-.05, x1 = sfts$shifts$larg_bf2+.05, y0 = rep(y, times=length(sfts$shifts$larg_bf2)), y1 = rep(y, times=length(sfts$shifts$larg_bf2)), col=col, lwd = 4)
  points(x = sfts$shifts$larg_bf2, y = rep(y, times=length(sfts$shifts$larg_bf2)), col=col, pch="_", cex=1)
  points(x = sfts$shifts$larg_bf6, y = rep(y, times=length(sfts$shifts$larg_bf6)), col=col, pch=15, cex=.9)
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################


add.int=function(GSA=F, qShift=F, ylim, xlim, alpha){
  
  ########################################
  #function HELP
  
  #this function adds rectangles within the desired bins to auxiliate visualization in fossil plots though time
  
  #Arguments:
  #mcmc = a complete MCMC output from pyrate containing all parameters
  #GSA = should GSA geologic time scale (in epochs) been drawn? only TRUE/FALSE accepted
  #qShift = The intervals which grey esquares are plotted. 
  #ylim = the limits of the y axis
  #ylim = the early and late bound of the x axis
  #alpha the transparency of the squares
  
  ########################################
  
  col_conv=function(x)sapply(strsplit(x, " "), function(x)return(rgb(x[1], x[2], x[3], maxColorValue=255)))
  
  if(GSA==T){
    rect(100, (ylim[1]+ylim[1]*.1), 66.7, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("166 216 74")), alpha.f = alpha),border = "white") #late K
    rect(66.7, (ylim[1]+ylim[1]*.1), 56, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("253 180 98")), alpha.f = alpha),border = "white") #paleocene
    rect(56, (ylim[1]+ylim[1]*.1), 33, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("253 180 108")), alpha.f = alpha),border = "white") #eocene
    rect(33, (ylim[1]+ylim[1]*.1), 23, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("253 192 122")), alpha.f = alpha),border = "white") #oligocene
    rect(23, (ylim[1]+ylim[1]*.1), 5.3, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("255 255 0")), alpha.f = alpha),border = "white") #miocene
    rect(5.3, (ylim[1]+ylim[1]*.1), 2.6, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("255 255 153")), alpha.f = alpha),border = "white") #pliocene
    rect(2.6, (ylim[1]+ylim[1]*.1), 1.8, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("255 237 179")), alpha.f = alpha),border = "white") #gelasian
    rect(1.8, (ylim[1]+ylim[1]*.1), 0, (ylim[2]+ylim[2]*.1), col=adjustcolor(col_conv(c("255 242 186")), alpha.f = alpha),border = "white") #calabrian
  }
  
  if(class(qShift)=="numeric"){
    qShift=c(xlim[1], qShift, xlim[2])
    cols=c("white", "azure4")
    j=1
    for(i in 2:length(qShift))
    {
      if(j==1){color=cols[1]}else{color=cols[2]}
      rect(qShift[i-1], (ylim[1]+ylim[1]*.1), qShift[i], (ylim[2]+ylim[2]*.1), col=adjustcolor(color, alpha.f = alpha),border = "white")
      j=j*-1
    }
  }
  
  
  
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################


get.durations=function(mcmc, output, na.rm=F, gen=F){
  
  ########################################
  #function HELP
  
  #this function 
  
  #Arguments:
  #mcmc = a complete MCMC output from pyrate containing all parameters
  #output = the type of output of the durations. "dist" for the whole duration distribution in form of a data.frame; "md" for a duration based on the range between the medians of TS and TE; "stats" for duration based on the median of durations (TS- TE per iteration), the mdian of TS, and TE and the median of TE
  #na.rm = logical. TRUE if NAs must be removed
  
  ########################################
  
  #getting lineages indexes:
  out=data.frame()
  ids_lineages=sort(which(sub(".*_", "", colnames(mcmc))== "TS" | sub(".*_", "", colnames(mcmc))=="TE"))
  if(gen==T){
    lineages=unique(sub("_.*", "", colnames(mcmc[ids_lineages])))  
  }else{
    aux1=unlist(strsplit(x = colnames(mcmc[ids_lineages]), split = "_"))
    aux1=aux1[-seq(from=3, to=length(aux1), by=3)]
    aux1=paste(aux1[seq(1,to=length(aux1), by=2)], aux1[seq(2,to=length(aux1), by=2)], sep="_")
    lineages=aux1[1:(length(aux1)/2)]
    df=data.frame(ids_lineages, aux1)
  }
  
  
  #loop for each lineage
  for(i in 1:length(lineages)){
    if(gen==T){
      aux=mcmc[,sort(which(sub("_.*", "", colnames(mcmc))==lineages[i]))]  
    }else{
      aux=mcmc[df$ids_lineages[which(df$aux1==lineages[i])]]
      
    }
    test=unlist(strsplit(x = colnames(aux), split = "_"))
    if(gen==T){
      durs=aux[,1]-aux[,2]  
    }else{
      if(test[1]==test[4] & test[2]==test[5]){
        durs=aux[,1]-aux[,2]  
      }else{stop("deu erro rapaz, checa ae")} 
    }
    
    
    
    
    
    #calculating stats:
    if(output=="dist"){
      durs=data.frame(duration=durs, lineage=lineages[i], it=1:length(durs))
      out=rbind(out, durs)
      
    }
    
    if(output=="md"){
      durs=data.frame(lineage=lineages[i], duration=median(durs, na.rm = na.rm))
      out=rbind(out, durs)
    }
    
    if(output=="stats"){
      durs=data.frame(lineage=lineages[i], duration=median(durs, na.rm=na.rm), TS = median(aux[,1], na.rm = na.rm),  TE=median(aux[,2], na.rm=na.rm))
      out=rbind(out, durs)
    }
  }
  
  #tidying distribution data before output results
  if(output=="dist"){
    out=spread(out, lineage, duration)
  }
  return(out)
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

search_site <- function(data, site_coding=F){
  
  ########################################
  #function HELP
  
  #NOTE: this finds and retrns the fossil site using all the information provided by the user
  
  #Arguments:
  #data = a Pyrate input object. It should have its columns as 'c("Species", "Status", "MaxT", "MinT")'
  #Site_info = a data frame with exact the same number of rows than the "data", containing all the information
  #site_coding = should the function return the dictionary of sites? If ==TRUE, is retrns a lsit with the data and all the information used by the function
  
  ########################################
  
  #checking input:
  var_acept <- c("Species", "Status", "MaxT", "MinT")
  if (sum(colnames(data) %in% var_acept)<4){
    stop("Please assign data column names in the format: \"Species\" \"Status\", \"MaxT\", \"MinT\"")
  }
  
    sp_col <- which(colnames(data)=="Species")
    info_site <- data[,-sp_col]
    sites <- unique(do.call(paste, c(info_site, sep="")))
    matchs <- as.data.frame(do.call(paste, c(info_site, sep="")))
    site_number <- apply(matchs, 1, match, table=sites)
    
    data$Site_aux <- site_number
  if (site_coding == F){
        return(data)
    }else{
        coding <- cbind(data, site_info, site_number)
        result <- list(data = data, site_coding = coding)
        return(result)        
    }  
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

buffer_location <- function(data, threshold=0){
  
  ########################################
  #function HELP
  
  #This function finds and retrns the fossil site using all the information provided by the user
  
  #Arguments:
  #data = a Pyrate input object. It should have its columns as 'c("Species", "Status", "MaxT", "MinT")'
  #Site_info = a data frame with exact the same number of rows than the "data", containing all the information
  #site_coding = should the function return the dictionary of sites? If ==TRUE, is retrns a lsit with the data and all the information used by the function
  
  ########################################
  
  #checking input:
  var_acept <- c("Species", "Status", "MaxT", "MinT", "lng", "lat", "Site")
  if (sum(colnames(data) %in% var_acept)<6){
    stop("Please assign data with at least 6 columns with the following data in the format: \"Species\" \"Status\", \"MaxT\", \"MinT\", \"lat\", \"lng\"")
  }
  
  geog=data[,which(colnames(data) %in% c("lat", "lng"))]
  bio_dat=data[,-which(colnames(data) %in% c("lat", "lng"))]
  bio_dat=search_site(bio_dat)
  bio_dat$pseudo_cand=do.call(paste, c(bio_dat[,c(1,6)], sep=""))
  
  cases_pseudo=sort(unique(bio_dat$pseudo_cand))
  for (i in 1:length(cases_pseudo)){
    ids=which(bio_dat$pseudo_cand == cases_pseudo[i])
   
    
    
    cbind(bio_dat[ids,], geog[ids,])
    
    
    
    
  }
  
  
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

TPP_selection=function(data, intervals, plot.TPP=F, buffer=0){
  
  ########################################
  #function HELP
  
  #???    This function finds and retrns the fossil site using all the information provided by the user
  
  #Arguments:
  #????    data = a Pyrate input object. It should have its columns as 'c("Species", "Status", "MaxT", "MinT")'
  #????    Site_info = a data frame with exact the same number of rows than the "data", containing all the information
  #?????    site_coding = should the function return the dictionary of sites? If ==TRUE, is retrns a lsit with the data and all the information used by the function
  
  ########################################
  
  #cheking input:
  var_acept <- c("Species", "Status", "MaxT", "MinT")
  if (sum(colnames(data) %in% var_acept)<4){
    stop("Please assign data column names in the format: \"Species\" \"Status\", \"MaxT\", \"MinT\"")
  }
  
  TPP_calc=as.data.frame(matrix(data = NA, nrow = nrow(data), ncol = (length(intervals)-1) ) )
  names=rep(NA, times=ncol(TPP_calc))
  
  #generating names:
  for(i in 1:ncol(TPP_calc)){
    names[i]= paste(intervals[i], intervals[i+1], sep="-")
  }
  colnames(TPP_calc)=names
  
  
  
  for(j in 1:(length(intervals)-1)){
    TPP_calc[, j]=0
    ids<-which(data$MinT >= intervals[j+1]-buffer & data$MaxT <= intervals[j]+buffer)
    TPP_calc[ids, j]=1
  }
  
  score=sum(apply(TPP_calc, 2, sum))
  res=paste0("Total occurrences which cross time frames = ", nrow(data)-score, " (", round(100*(nrow(data)-score)/nrow(data), digits = 2), "%)")
  
  if(plot.TPP==F){
    return(res)
  }
  
  if(plot.TPP==T){
    plot(NA, xlim = c(max(c(data$MaxT, max(intervals)*1.03)), min(data$MinT)), ylim=c(0, nrow(data)), xlab="Time (Mya)", ylab= "Fossil Record", yaxt="n", main=res)
    
    for_plot=data[order(data$MaxT, data$MinT, decreasing = T),]
    for_plot$colors="black"
    
    for_plot$colors[which(apply(TPP_calc, 1, sum) == 0)]="red"
    
    for(i in 1:nrow(for_plot)){
      segments(y0=i, y1=i, x0 = for_plot$MinT[i], x1=for_plot$MaxT[i], col = for_plot$colors[i])
    }
    abline(v=intervals, col="red")
  }
  
  
}  
################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

find.pseudoreplicates=function(dtst1, dtst2, lat_threshold=0, long_threshold=0, distance_threshold=0, time_threshold=0, JW_threshold=1, print_progress=F){
  
  ########################################
  #function HELP
  
  #NOTE: this function searches for pseudoreplicates between two datasets. It may use the dating or the location to do so
  
  #Arguments:
  #dtst1 = dataset 1. Columns must be named in the format "Species", "MaxT", "MinT", "lng", "lat""
  
  #dtst2 = dataset 2. Columns must be named in the format "Species", "MaxT", "MinT", "lng", "lat""
  
  #lat_threshold = Threshold of maximum latitude (in degrees) in which occurrences may be considered pseudoreplicas. Default value is 0.
  
  #long_threshold = Threshold of maximum longitude (in degrees) in which occurrences may be considered pseudoreplicas. Default value is 0.
  
  #distance_threshold = Threshold of maximum time (in early and late bounds) diferrence in which occurrences may be considered pseudoreplicas (in million years). Default value is 0.
  
  #time_threshold = Threshold of maximum direct distance (in meters) in which occurrences may be considered pseudoreplicas. Default value is 0.
  
  #JW_threshold = Minimal distance of lineage name (measured in the jaro-winkler distance) in which occurrences may be considered pseudoreplicas. Default value is 1, which indicates perfect match between the names of the lineages. 
  
  #print_progress  = When equals TRUE, prints the progress already made. The function may take some seconds to run.
  
  ########################################
  
  
  if( sum(colnames(dtst1) %in% c("Species", "MaxT", "MinT", "lng", "lat")) < ncol(dtst1) ){
    stop("The columns in dtst1 must be named in the format \"Species\", \"MaxT\", \"MinT\", \"lng\", \"lat\"")
  }
  
  if( sum(colnames(dtst1) %in% c("Species", "MaxT", "MinT", "lng", "lat")) < ncol(dtst1) ){
    stop("The columns in dtst2 must be named in the format \"Species\", \"MaxT\", \"MinT\", \"lng\", \"lat\"")
  }
  
  if(JW_threshold <0 | JW_threshold >1){
    stop("JW_threshold must be between 0 and 1")
  }
  
  results=list()
  ids=vector()
  for(j in 1:nrow(dtst2)){
    JW=RecordLinkage::jarowinkler(str1 = dtst1$Species, str2= dtst2$Species[j])
    
    if(sum(JW>=JW_threshold)>0){
      aux=dtst1[which(JW>=JW_threshold),]
      #aux[3,1]="Eotragus_artenensis"
      #aux[3,2:5]=c(16.9, 15.97, 11.19, 48.9)
      
      aux_2=data.frame(a=abs(aux$MaxT-dtst2$MaxT[j]),
                       b=abs(aux$MinT-dtst2$MinT[j]),
                       c=abs(aux$lng-dtst2$lng[j]),
                       d=abs(aux$lat-dtst2$lat[j]))
      
      aux_2=cbind(aux_2[,1:2]<time_threshold, 
                  aux_2[,3]<long_threshold,
                  aux_2[,4]<lat_threshold,
                  apply(aux[,4:5], 1, geosphere::distGeo, p2=c(dtst2$lng[j], dtst2$lat[j])) < distance_threshold)
      
      if(dim(aux[which(apply(aux_2, 1, sum)>0),])[1]>5){
        ids=c(ids, j)
        res=rbind(dtst2[j,],aux[which(apply(aux_2, 1, sum)>0),])
        res$dataset=c(1, rep(2, times=nrow(res)-1))
        res$row=c(j, which(JW==1)[which(apply(aux_2, 1, sum)>0)])
        results[[j]]=res
      }
    }
    
    if(print_progress==T){
      print(paste0("Done ", round(j/nrow(dtst2), digits = 2)*100, "% at ", Sys.Date(), "and ", Sys.time()))  
    }
    
  }
  
  
  
  print(paste0(length(results)-sum(unlist(lapply(results, is.null))),
               " (",100-100*round(sum(unlist(lapply(results, is.null)))/length(results), digits = 2) ,
               "%) possible pseudoreplicates found" ))
  
  return(plyr::compact(results))
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################


no.extension <- function(filename) { 
  if (substr(filename, nchar(filename), nchar(filename))==".") { 
    return(substr(filename, 1, nchar(filename)-1)) 
  } else { 
    no.extension(substr(filename, 1, nchar(filename)-1)) 
  } 
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

extract.ages <- function(file = NULL, data=NULL, data.name=NULL, replicates = 1, cutoff = NULL, random = TRUE){
  
  if(is.null(file) + is.null(data) != 1){stop("Please provide a tab-separated file (\"file\" argument) OR and data.frame object (\"data\" argument)")}
  
  if (is.null(data)){
    dat1 <- read.table(file, header=T, stringsAsFactors=F, row.names=NULL, sep="\t", strip.white=T)
    fname <- no.extension(basename(file))
    outfile <- paste(dirname(file), "/", fname, "_PyRate.py", sep="")
    
  }else{
    if (is.null(data.name)){
      stop("When selecting a data.frame object, you must specify a name for the output file in the argument \"data.name\"")
    }
    dat1 <- data
    fname=data.name
    outfile=paste0(getwd(), "/", fname, "_PyRate.py")
  }

  rnd <- random
  q <- cutoff
  
  
  dat1[,1] <- gsub("[[:blank:]]{1,}","_", dat1[,1])
  
  if (replicates > 1){
    rnd <- TRUE
  }
  
  if (any(is.na(dat1[,1:4]))){
    print(c(which(is.na(dat1[,1])),which(is.na(dat1[,2])),which(is.na(dat1[,3])),which(is.na(dat1[,4]))  )   )
    stop("the input file contains missing data in species names, status or ages)\n")
  }
  
  if (!is.null(q)){
    dat <- dat1[!(dat1[,4] - dat1[,3] >= q),]
    cat("\n\nExcluded ", 100-round(100*dim(dat)[1]/dim(dat1)[1]), "% occurrences")
    hist(dat1[,4] - dat1[,3])
  } else { 
    dat <- dat1 
  }
  
  if (length(dat) == 5){
    colnames(dat) <- c("Species", "Status", "min_age", "max_age", "trait")	
  } else {
    colnames(dat) <- c("Species", "Status", "min_age", "max_age")
  }
  
  dat$new_age <- "NA"
  splist <- unique(dat[,c(1,2)])[order(unique(dat[,c(1,2)][,1])),]
  
  
  if (any(is.element(splist$Species[splist$Status == "extant"], splist$Species[splist$Status == "extinct"]))){
    print(intersect(splist$Species[splist$Status == "extant"], splist$Species[splist$Status == "extinct"]))
    stop("at least one species is listed as both extinct and extant\n")
  }
  
  cat("#!/usr/bin/env python", "from numpy import * ", "",  file=outfile, sep="\n")
  
  for (j in 1:replicates){
    times <- list()
    cat ("\nreplicate", j)
    
    dat[dat$min_age == 0,3] <- 0.001
    
    #if (any(dat[,4] < dat[,3])){
    #	cat("\nWarning: the min age is older than the max age for at least one record\n")
    #	cat ("\nlines:",1+as.numeric(which(dat[,4] < dat[,3])),sep=" ")
    #}
    
    if (isTRUE(rnd)){
      dat$new_age <- round(runif(length(dat[,1]), min=apply(dat[,3:4],FUN=min,1), max=apply(dat[,3:4],FUN=max,1)), digits=6)
    } else {
      for (i in 1:length(dat[,1])){
        dat$new_age[i] <- mean(c(dat[i,3], dat[i,4]))
      }				
    }
    
    dat2 <- subset(dat, select=c("Species","new_age"))
    taxa <- sort(unique(dat2$Species))
    
    for (n in 1:length(taxa)){
      times[[n]] <- dat2$new_age[dat2$Species == taxa[n]]
      if (toupper(splist$Status[splist$Species == taxa[n]]) == toupper("extant")){
        times[[n]] <- append(times[[n]], "0", after=length(times[[n]]))
      }
    }
    
    dat3 <- matrix(data=NA, nrow=length(times), ncol=max(sapply(times, length)))
    rownames(dat3) <- taxa
    
    for (p in 1:length(times)){
      dat3[p,1:length(times[[p]])] <- times[[p]]
    }
    
    cat(noquote(sprintf("\ndata_%s=[", j)), file=outfile, append=TRUE)
    
    for (n in 1:(length(taxa)-1)){
      rec <- paste(dat3[n,!is.na(dat3[n,])], collapse=",")
      cat(noquote(sprintf("array([%s]),", rec)), file=outfile, append=TRUE, sep="\n")
    }
    
    n <- n+1
    rec <- paste(dat3[n,!is.na(dat3[n,])], collapse=",")
    cat(noquote(sprintf("array([%s])", rec)), file=outfile, append=TRUE, sep="\n")
    
    cat("]", "", file=outfile, append=TRUE, sep="\n")
  }
  
  
  data_sets <- ""
  names <- ""
  
  if (replicates > 1){
    for (j in 1:(replicates-1)) {
      data_sets <- paste(data_sets, noquote(sprintf("data_%s,", j)))
      names <- paste(names, noquote(sprintf(" '%s_%s',", fname,j)))
    }
    
    data_sets <- paste(data_sets, noquote(sprintf("data_%s", j+1)))
    names <- paste(names, noquote(sprintf(" '%s_%s',", fname,j+1)))
  } else {
    data_sets <- "data_1"
    names <- noquote(sprintf(" '%s_1'", fname))	
  }
  
  cat(noquote(sprintf("d=[%s]", data_sets)), noquote(sprintf("names=[%s]", names)), "def get_data(i): return d[i]", "def get_out_name(i): return  names[i]", file=outfile, append=TRUE, sep="\n")
  
  
  tax_names <- paste(taxa, collapse="','")
  cat(noquote(sprintf("taxa_names=['%s']", tax_names)), "def get_taxa_names(): return taxa_names", file=outfile, append=TRUE, sep="\n")
  
  
  if ("trait" %in% colnames(dat)){
    datBM <- dat[,1]
    splist$Trait <- NA
    for (n in 1:length(splist[,1])){
      splist$Trait[n] <- mean(dat$trait[datBM == splist[n,1]], na.rm=T)
    }
    s1 <- "\ntrait1=array(["
    BM <- gsub("NaN|NA", "nan", toString(splist$Trait))
    s2 <- "])\ntraits=[trait1]\ndef get_continuous(i): return traits[i]"
    STR <- paste(s1,BM,s2)
    cat(STR, file=outfile, append=TRUE, sep="\n")
  }
  
  splistout <- outfile
  lookup <- as.data.frame(taxa)
  lookup$status  <- "extinct"
  
  write.table(splist, file=splistout, sep="\t", row.names=F, quote=F)
  cat("\n\nPyRate input file was saved in: ", sprintf("%s", outfile), "\n\n")
  
}

################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

fit.prior <- function(file = NULL, lineage = "root_age"){
  
  require(fitdistrplus)
  
  if (is.null(file)){
    stop("You must enter a valid filename.\n")
  }
  
  dat <- read.table(file, header=T, stringsAsFactors=F, row.names=NULL, sep="\t")
  fname <- no.extension(basename(file))
  outfile <- paste(dirname(file), "/", lineage, "_Prior.txt", sep="")
  
  lineage2 <- paste(lineage,"_TS", sep="")
  if (!is.element(lineage2, colnames(dat))){
    stop("Lineage not found, please check your input.\n")
  }
  
  time <- dat[,which(names(dat) == lineage2)]
  time2 <- time-(min(time)-0.01)
  gamm <- fitdist(time2, distr="gamma", method = "mle")$estimate 
  
  cat("Lineage: ", lineage, "; Shape: ", gamm[1], "; Scale: ", 1/gamm[2], "; Offset: ", min(time), sep="", file=outfile, append=FALSE)
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################


extract.ages.pbdb <- function(file = NULL,sep=",", extant_species = c(), replicates = 1, cutoff = NULL, random = TRUE){
  print("This function is currently being tested - caution with the results!")
  tbl = read.table(file=file,h=T,sep=sep,stringsAsFactors =F)
  new_tbl = NULL # ADD EXTANT SPECIES
  
  for (i in 1:dim(tbl)[1]){
    if (tbl$accepted_name[i] %in% extant_species){
      status="extant"
    }else{status="extinct"}
    species_name = gsub(" ", "_", tbl$accepted_name[i])
    new_tbl = rbind(new_tbl,c(species_name,status,tbl$min_ma[i],tbl$max_ma[i]))
  }
  colnames(new_tbl) = c("Species","Status","min_age","max_age")
  
  output_file = file.path(dirname(file),strsplit(basename(file), "\\.")[[1]][1])
  output_file = paste(output_file,".txt",sep="")
  write.table(file=output_file,new_tbl,quote=F,row.names = F,sep="\t")
  extract.ages(file=output_file,replicates = replicates, cutoff = cutoff, random = random)
}


################################################################################
#                                OTHER FUNCTION:                               #
################################################################################

extract.ages.tbl <- function(file = NULL,sep="\t", extant_species = c(), replicates = 1, cutoff = NULL, random = TRUE){
  tbl = read.table(file=file,h=T,sep=sep,stringsAsFactors =F)
  
  new_tbl = NULL # ADD EXTANT SPECIES
  
  for (i in 1:dim(tbl)[1]){
    if (tbl[i,1] %in% extant_species){
      status="extant"
    }else{status="extinct"}
    species_name = gsub(" ", "_", tbl[i,1])
    new_tbl = rbind(new_tbl,c(species_name,status,tbl[i,2:dim(tbl)[2]]))
  }
  if (dim(new_tbl)[2]==4){
    colnames(new_tbl) = c("Species","Status","min_age","max_age")
  }else{
    colnames(new_tbl) = c("Species","Status","min_age","max_age","trait")
  }
  
  
  output_file = file.path(dirname(file),strsplit(basename(file), "\\.")[[1]][1])
  output_file = paste(output_file,".txt",sep="")
  write.table(file=output_file,new_tbl,quote=F,row.names = F,sep="\t")
  extract.ages(file=output_file,replicates = replicates, cutoff = cutoff, random = random)
}


