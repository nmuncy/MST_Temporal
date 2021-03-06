
library(reshape2)
library(ez)


##### Set up
# Set working variables, toggles, and key bindings

parDir <- '/Volumes/Yorick/Temporal/'
workDir <- paste0(parDir,"Experiment3/Behavioral_Data/")
dataDir <- paste0(parDir,"Analyses/Exp3/behAnalysis/")

# Toggles
makeData <- 1
runStats <- 1
makeGraphs <- 1

# Variables, capture key-presses
res.Long <- " 1"
res.Same <- " 2"
res.Short <- " 3"


### Functions
dprime.Function <- function(df,string){
  
  x.df <- matrix(NA,nrow=dim(df)[1],ncol=5)
  x.df[,1] <- (df[,1]+df[,2])/df[,3]  # n.Hit/n.TargR + n.Miss/n.TargR
  x.df[,2] <- (df[,4]+df[,5])/df[,6]  # n.FA1/n.LureR + n.FA2/n.LureR
  
  # correct for ones (Hit) and zeros (FA)
  if(any(x.df[,1]==1)==T){
    for(a in 1:dim(df)[1]){
      if(x.df[a,1]==1){
        x.df[a,1]<-1-(1/(2*as.numeric(df[a,3])))
      }
    }
  }
  
  if(any(x.df[,2]==1)==T){
    for(a in 1:dim(df)[1]){
      if(x.df[a,2]==1){
        x.df[a,2]<-1-(1/(2*as.numeric(df[a,6])))
      }
    }
  }
  
  if(any(x.df[,2]==0)==T){
    for(a in 1:dim(df)[1]){
      if(x.df[a,2]==0){
        x.df[a,2]<-(1/(2*as.numeric(df[a,6])))
      }
    }
  }
  
  x.df[,3] <- qnorm(x.df[,1])     # z-score Hit
  x.df[,4] <- qnorm(x.df[,2])     # Z-score FA
  x.df[,5] <- x.df[,3]-x.df[,4]   # d'
  return(x.df[,5])
  
  # df.out <- matrix(NA,nrow=dim(df)[1],ncol=2)
  # df.out[,1] <- as.character(df.Master[,1])
  # df.out[,2] <- x.df[,5]
  # write.table(df.out,paste0(dataDir,paste0(string,".txt")),row.names = F, quote = F, sep = '\t')
}

ttest.Function <- function(a,b,sA,sB){
  
  h.stats <- list()
  h.stats[[1]] <- paste0(sA," d-prime")
  h.stats[[2]] <- mean(a)
  h.stats[[3]] <- paste0(sA," vs 0")
  h.stats[[4]] <- t.test(a, mu=0)
  h.stats[[5]] <- paste0(sB," d-prime")
  h.stats[[6]] <- mean(b)
  h.stats[[7]] <- paste0(sB," vs 0")
  h.stats[[8]] <- t.test(b, mu=0)
  h.stats[[9]] <- paste0(sA," vs ",sB)
  h.stats[[10]] <- t.test(a,b,paired=T)
  
  return(h.stats)
}




##### Step 1 Make Master dataframes
#
# Read in raw data, determine behavioral responses
# and bin counts for each behavior. 
# Make sure to remove training session. The experiment
# was 294 trials long with training, 282 without.
# Write one out to each subjects directory, and keep
# a master list of response bins for d' calcs later

if(makeData == 1){
  
  # Prepare group-level output
  master.mem <- matrix(NA,nrow=0,ncol=35)
  
  # Get subj list
  subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
  subjList <- t(subjList)

  
  ### Decode e/subj raw behavioral data  
  for(j in subjList){
    
    subjDir <- paste0(workDir,j,"/")
    df.subj <- read.csv(paste0(subjDir,j,"_timing_log.csv"))
    df.subj <- as.data.frame(df.subj)
    
    # demographics
    age <- as.numeric(as.character(df.subj[2,2]))
    sex <- gsub(" ","",as.character(df.subj[3,2]),fixed=T)
    
    # index memory - number of trials
    ind.mem <- NA
    for(i in 1:dim(df.subj)[1]){
      if(grepl("Trial Start", as.character(df.subj[i,1]))==T){
        ind.mem <- c(ind.mem,i)
      }
    }
    ind.mem <- ind.mem[-1]
    
    
    ### Decode memory output
    # set up
    df.mem <- as.data.frame(matrix(NA,nrow=length(ind.mem),ncol=7))
    colnames(df.mem) <- c("TrialNum","Type","Duration","Repeat","Lag","Response","Behavior")
    
    # loop through all trials
    for(i in 1:length(ind.mem)){
      
      # TrialNum, Type, Duration, Repeat, Lag
      df.mem[i,1] <- i
      df.mem[i,2] <- as.character(df.subj[ind.mem[i]+4,2])
      df.mem[i,3] <- as.character(df.subj[ind.mem[i]+3,2])
      df.mem[i,4] <- as.character(df.subj[ind.mem[i]+6,2])
      df.mem[i,5] <- as.character(df.subj[ind.mem[i]+5,2])
      
      # Get Response, only accept single responses, don't lose last response
      hold1 <- ind.mem[i]+9
      hold2 <- ind.mem[i]+11
      if(grepl("Response",df.subj[hold1,1])==T && (grepl("Trial Start",df.subj[hold2,1])==T || hold2 >= dim(df.subj)[1])){
        df.mem[i,6] <- as.character(df.subj[hold1,2])
      }else{
        df.mem[i,6] <- 999
      }
      
      # strip spaces
      for(k in 2:6){
        df.mem[i,k] <- gsub(" ","",df.mem[i,k],fixed=T)
      }
    }
    
    
    ### Strip off training trials
    # experiment lengths (294,282) are hardcoded
    if(dim(df.mem)[1]==294){
      df.mem <- tail(df.mem,-12)
      row.names(df.mem) <- 1:dim(df.mem)[1]
      df.mem[,1] <- 1:dim(df.mem)[1]
    }
    
    # Check
    stop = FALSE
    if(dim(df.mem)[1]!=282){
      stop = TRUE
      stop('The number of trials was not 294 or 282')
    }
    if(stop){break}
    
    
    ### Determine and write behaviors
    ind_HST1 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T1"); df.mem[ind_HST1,7] <- "Hit_Same_T1"
    ind_HST2 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T2"); df.mem[ind_HST2,7] <- "Hit_Same_T2"
    ind_MLT1 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T1"); df.mem[ind_MLT1,7] <- "Miss_Long_T1"
    ind_MLT2 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T2"); df.mem[ind_MLT2,7] <- "Miss_Long_T2"
    ind_MST1 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T1"); df.mem[ind_MST1,7] <- "Miss_Short_T1"
    ind_MST2 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T2"); df.mem[ind_MST2,7] <- "Miss_Short_T2"
    
    ind_CShT1 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T1"); df.mem[ind_CShT1,7] <- "CR_Short_T1"
    ind_CLT2 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T2"); df.mem[ind_CLT2,7] <- "CR_Long_T2"
    ind_FLT1 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T1"); df.mem[ind_FLT1,7] <- "FA_Long_T1"
    ind_FSaT1 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T1"); df.mem[ind_FSaT1,7] <- "FA_Same_T1"
    ind_FSaT2 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T2"); df.mem[ind_FSaT2,7] <- "FA_Same_T2"
    ind_FShT2 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T2"); df.mem[ind_FShT2,7] <- "FA_Short_T2"
    
    ind_EncR <- which(df.mem$Repeat=="No"); df.mem[ind_EncR,7] <- "EncR"
    
    # write subject data frame
    write.table(df.mem,paste0(subjDir,"Memory_data.txt"),row.names = F, quote = F, sep = '\t')

    
    ### Bin counts, for each lag
    for(i in c(4,12)){
      
      # num of behaviors
      assign(paste0("num_HST1_L",i), as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_HST2_L",i), as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      assign(paste0("num_MLT1_L",i),as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_MLT2_L",i),as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      assign(paste0("num_MST1_L",i),as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_MST2_L",i),as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      
      assign(paste0("num_CShT1_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_CLT2_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      assign(paste0("num_FLT1_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_FSaT1_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_FSaT2_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      assign(paste0("num_FShT2_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      
      # num of trials responded
      assign(paste0("num_TargRT1_L",i),as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response!="999" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_TargRT2_L",i),as.numeric(length(which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response!="999" & df.mem$Duration=="T2" & df.mem$Lag==i))))
      assign(paste0("num_LureRT1_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response!="999" & df.mem$Duration=="T1" & df.mem$Lag==i))))
      assign(paste0("num_LureRT2_L",i),as.numeric(length(which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response!="999" & df.mem$Duration=="T2" & df.mem$Lag==i))))
    }
    
     
    ### For writing group-level memory bins
    # write a line for each subject
    beh_hold <- c(j,age,sex)
    bin_list <- ls(pattern="num_")
    
    for(i in bin_list){
      beh_hold <- c(beh_hold,get(i))
    }
    master.mem <- rbind(master.mem,beh_hold)
  }
  
  # colnames correspond to order of bin_list
  name_list <- c("Subj","Age","Sex","CR_Long_T2_L12","CR_Long_T2_L4","CR_Short_T1_L12","CR_Short_T1_L4","FA_Long_T1_L12","FA_Long_T1_L4","FA_Same_T1_L12","FA_Same_T1_L4","FA_Same_T2_L12","FA_Same_T2_L4","FA_Short_T2_L12","FA_Short_T2_L4","Hit_Same_T1_L12","Hit_Same_T1_L4","Hit_Same_T2_L12","Hit_Same_T2_L4","LureR_T1_L12","LureR_T1_L4","LureR_T2_L12","LureR_T2_L4","Miss_Long_T1_L12","Miss_Long_T1_L4","Miss_Long_T2_L12","Miss_Long_T2_L4","Miss_Short_T1_L12","Miss_Short_T1_L4","Miss_Short_T2_L12","Miss_Short_T2_L4","TargR_T1_L12","TargR_T1_L4","TargR_T2_L12","TargR_T2_L4")
  colnames(master.mem) <- name_list
  write.table(master.mem,paste0(dataDir,"Master_Memory_bin_counts.txt"),row.names = F, quote = F, sep = '\t')
}




##### Step 2 Stats
#
# Test d's against zero and each other.
# Account for potential outliers.

if(runStats == 1){
  
  df.Master <- read.delim(paste0(dataDir,"Master_Memory_bin_counts.txt"))

  ### Get d' scores
  # d'T1
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T1_L4) + as.numeric(df.Master$Hit_Same_T1_L12)
  df.hold[,2] <- as.numeric(df.Master$Miss_Long_T1_L4) + as.numeric(df.Master$Miss_Long_T1_L12)
  df.hold[,3] <- as.numeric(df.Master$TargR_T1_L4) + as.numeric(df.Master$TargR_T1_L12)
  df.hold[,4] <- as.numeric(df.Master$FA_Long_T1_L4) + as.numeric(df.Master$FA_Long_T1_L12)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T1_L4) + as.numeric(df.Master$FA_Same_T1_L12)
  df.hold[,6] <- as.numeric(df.Master$LureR_T1_L4) + as.numeric(df.Master$LureR_T1_L12)
  T1 <- dprime.Function(df.hold)
  
  # d'T2
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T2_L4) + as.numeric(df.Master$Hit_Same_T2_L12)
  df.hold[,2] <- as.numeric(df.Master$Miss_Short_T2_L4) + as.numeric(df.Master$Miss_Short_T2_L12)
  df.hold[,3] <- as.numeric(df.Master$TargR_T2_L4) + as.numeric(df.Master$TargR_T2_L12)
  df.hold[,4] <- as.numeric(df.Master$FA_Short_T2_L4) + as.numeric(df.Master$FA_Short_T2_L12)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T2_L4) + as.numeric(df.Master$FA_Same_T2_L12)
  df.hold[,6] <- as.numeric(df.Master$LureR_T2_L4) + as.numeric(df.Master$LureR_T2_L12)
  T2 <- dprime.Function(df.hold)
  
  
  #ttest
  T1vT2 <- ttest.Function(T1,T2,"T1","T2")
  h.out <- capture.output(print(T1vT2))
  write.table(h.out,paste0(dataDir,"Stats_TTest_dprime_T1vT2.txt"),row.names = F, quote = F, sep = '\t')
  
  
  ## Correct for multiple comparisons on T-tests - FDR. No/outliers separately
  # outliers
  p.T1v0 <- T1vT2[[4]]$p.value
  p.T2v0 <- T1vT2[[8]]$p.value
  p.T1vT2 <- T1vT2[[10]]$p.value
  p.input <- c(p.T1v0,p.T2v0,p.T1vT2)
  p.output <- p.adjust(p.input, method="fdr", n=length(p.input))
  
  output <- matrix(NA,nrow=length(p.output),ncol=2)
  colnames(output) <- c("orig","adj")
  rownames(output) <- c("T1vO", "T2v0", "T1vT2")
  output[,1] <- p.input
  output[,2] <- p.output
  write.table(output,file=paste0(dataDir,"Stats_TTest_dprime_corrected.txt"),sep="\t",row.names=T,col.names=T)
  
  
  ## make D' table
  df.out <- matrix(NA,nrow=dim(df.Master)[1],ncol=3)
  df.out[,1] <- as.character(df.Master[,1])
  df.out[,2] <- round(T1,2)
  df.out[,3] <- round(T2,2)
  colnames(df.out) <- c("Subj","T1","T2")
  write.table(df.out,paste0(dataDir,"Master_dprime.txt"),row.names = F, quote = F, sep = '\t')
  
  
  ## test for outliers
  hold.list <-NA
  for(i in 2:dim(df.out)[2]){
    
    h.iqr <- IQR(df.out[,i]);
    h.15iqr <- h.iqr+(h.iqr/2)
    h.quant <- quantile(as.numeric(df.out[,i]),names=F)
    h.max <- h.15iqr+h.quant[4]
    h.min <- h.quant[2]-h.15iqr
    
    for(j in 1:dim(df.out)[1]){
      if(df.out[j,i] > h.max || df.out[j,i] < h.min){
        hold.out <- paste0(df.out[j,1],"-",colnames(df.out)[i])
        hold.list <- c(hold.list,hold.out)
      }
    }
  }
  out.list <- (hold.list[-1])
  write.table(out.list,paste0(dataDir,"Outlier_list.txt"),row.names = F, col.names=F,quote = F,sep='\t')
  
  
  ## remove outliers - replace with col median
  df.outlier <- matrix(NA,nrow=dim(df.Master)[1],ncol=3)
  colnames(df.outlier) <- c("Subj","T1","T2")
  for(i in 1:dim(df.outlier)[1]){
    
    subj <- df.out[i,1]
    df.outlier[i,1] <- df.out[i,1]
    
    for(j in 2:dim(df.outlier)[2]){
      
      h.name <- colnames(df.out)[j]
      h.string <- paste0(subj,"-",h.name)
      
      contain.outlier <- h.string %in% out.list
      if(contain.outlier == FALSE){
        df.outlier[i,j] <- df.out[i,j]
      }else{
        df.outlier[i,j] <- median(as.numeric(df.out[,2]))
      }
    }
  }
  
  ## rerun stats without outliers
  #ttest
  T1vT2 <- ttest.Function(as.numeric(df.outlier[,2]),as.numeric(df.outlier[,3]),"T1","T2")
  h.out <- capture.output(print(T1vT2))
  write.table(h.out,paste0(dataDir,"Stats_TTest_dprime_T1vT2_noOutlier.txt"),row.names = F, quote = F, sep = '\t')
  
  
  ## Correct for multiple comparisons on T-tests (ANOVA had no effects) - FDR. No/outliers separately
  # Xoutliers
  p.T1v0 <- T1vT2[[4]]$p.value
  p.T2v0 <- T1vT2[[8]]$p.value
  p.T1vT2 <- T1vT2[[10]]$p.value
  p.input <- c(p.T1v0,p.T2v0,p.T1vT2)
  p.output <- p.adjust(p.input, method="fdr", n=length(p.input))

  output <- matrix(NA,nrow=length(p.output),ncol=2)
  colnames(output) <- c("orig","adj")
  rownames(output) <- c("T1vO", "T2v0", "T1vT2")
  output[,1] <- p.input
  output[,2] <- p.output
  write.table(output,file=paste0(dataDir,"Stats_TTest_dprime_corrected_noOutlier.txt"),sep="\t",row.names=T,col.names=T)
}




##### Step 3 Graphs
#---------------------------------
if(makeGraphs == 1){
  
  df.dprime <- read.delim(paste0(dataDir,"Master_dprime.txt"))
  outDir <- paste0(parDir,"write/figures/")

  ### T1vT2 (T1 = L->S)
  df.graph <- matrix(NA,nrow=dim(df.dprime)[1],ncol=2)
  colnames(df.graph) <- c("L -> S","S -> L")
  df.graph[,1] <- df.dprime[,2]
  df.graph[,2] <- df.dprime[,3]

  tiff(paste0(outDir,"Exp3_Fig_T1vT2_dprime.tiff"), height = 5.5, width = 5.5, units = 'in', res=300)
  par(family="Times New Roman")
  hold.graph <- boxplot(df.graph, ylim=c(-1,4), ylab="d' scores", col="white", cex.lab=1.5, cex.axis=1)
  title(main=list("Sensitivity to Duration", cex=1.5))
  abline(h = 0)

  # par(xpd=TRUE)
  text(1,3.7,"***",cex=1)
  text(2,3.0,"***",cex=1)
  dev.off()
}

