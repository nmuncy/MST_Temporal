
library(reshape2)
library(ez)


##### Set up
parDir <- '/Volumes/Yorick/Temporal/Experiment3/'
workDir <- paste0(parDir,"Behavioral_Data/")
dataDir <- paste0(parDir,"Analyses/behAnalysis/")

# Toggles
makeData <- 1
runStats <- 1
makeGraphs <- 0

# Variables, capture key-presses
res.Long <- " 1"
res.Same <- " 2"
res.Short <- " 3"


### Functions
WLC.Function <- function(aa,bb,cc,dd){
  
  num.subj <- dim(df.Master)[1]
  df.long <- as.data.frame(matrix(NA,nrow=(num.subj*4),ncol=4))
  names(df.long) <- c("Subj","Dur","Lag","Dprime")
  
  #subject
  df.long[,1] <- rep(as.character(df.Master[,1]),4)
  
  #duration
  h.list <- c("T1","T2")
  for(ii in 1:length(h.list)){
    start=(ii*(2*num.subj))-((2*num.subj)-1)
    end=(ii*(2*num.subj))
    df.long[start:end,2] <- h.list[ii]
  }
  
  #Lag
  h.list <- c(rep(c("Lag4","Lag12"),2))
  for(ii in 1:length(h.list)){
    start=(ii*num.subj)-(num.subj-1)
    end=(ii*num.subj)
    df.long[start:end,3] <- h.list[ii]
  }
  
  #Data
  df.long[((1*num.subj)-(num.subj-1)):(1*num.subj),4] <- aa
  df.long[((2*num.subj)-(num.subj-1)):(2*num.subj),4] <- bb
  df.long[((3*num.subj)-(num.subj-1)):(3*num.subj),4] <- cc
  df.long[((4*num.subj)-(num.subj-1)):(4*num.subj),4] <- dd
  
  df.long <- as.data.frame(df.long)
  df.long$Dprime <- as.numeric(df.long$Dprime)
  return(df.long)
}

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
#---------------------------------
if(makeData == 1){
  
  # Get subj list
  subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
  subjList <- t(subjList)
  
  
  # Prepare output
  master.mem <- matrix(NA,nrow=0,ncol=35)


  ### Decode e/subj raw behavioral data  
  # j <- subjList[1]
  for(j in subjList){
    
    subjDir <- paste0(workDir,j,"/")
    df.subj <- read.csv(paste0(subjDir,j,"_timing_log.csv"))
    df.subj <- as.data.frame(df.subj)
    
    # demographics
    age <- as.numeric(as.character(df.subj[2,2]))
    sex <- gsub(" ","",as.character(df.subj[3,2]),fixed=T)
    
    # index memory
    ind.mem <- NA
    for(i in 1:dim(df.subj)[1]){
      if(grepl("Trial Start", as.character(df.subj[i,1]))==T){
        ind.mem <- c(ind.mem,i)
      }
    }
    ind.mem <- ind.mem[-1]
    
    
    
    ### Decode memory output
    df.mem <- as.data.frame(matrix(NA,nrow=length(ind.mem),ncol=7))
    colnames(df.mem) <- c("TrialNum","Type","Duration","Repeat","Lag","Response","Behavior")
    
    
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
    
    
    
    ### Strip of training trials
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
    
    
    
    ### Determine behaviors
    ind_HST1 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T1")
    ind_HST2 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T2")
    ind_MLT1 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T1")
    ind_MLT2 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T2")
    ind_MST1 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T1")
    ind_MST2 <- which(df.mem$Type=="Targ" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T2")
    
    ind_CShT1 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T1")
    ind_CLT2 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T2")
    ind_FLT1 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="1" & df.mem$Duration=="T1")
    ind_FSaT1 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T1")
    ind_FSaT2 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="2" & df.mem$Duration=="T2")
    ind_FShT2 <- which(df.mem$Type=="Lure" & df.mem$Repeat=="Yes" & df.mem$Response=="3" & df.mem$Duration=="T2")
    
    ind_EncR <- which(df.mem$Repeat=="No")
    
    
    # write behaviors
    df.mem[ind_HST1,7] <- "Hit_Same_T1"
    df.mem[ind_HST2,7] <- "Hit_Same_T2"
    df.mem[ind_MLT1,7] <- "Miss_Long_T1"
    df.mem[ind_MLT2,7] <- "Miss_Long_T2"
    df.mem[ind_MST1,7] <- "Miss_Short_T1"
    df.mem[ind_MST2,7] <- "Miss_Short_T2"
    
    df.mem[ind_CShT1,7] <- "CR_Short_T1"
    df.mem[ind_CLT2,7] <- "CR_Long_T2"
    df.mem[ind_FLT1,7] <- "FA_Long_T1"
    df.mem[ind_FSaT1,7] <- "FA_Same_T1"
    df.mem[ind_FSaT2,7] <- "FA_Same_T2"
    df.mem[ind_FShT2,7] <- "FA_Short_T2"
    
    df.mem[ind_EncR,7] <- "EncR"

      
    # write data frame
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
    beh_hold <- c(j,age,sex)
    
    bin_list <- ls(pattern="num_")
    for(i in bin_list){
      beh_hold <- c(beh_hold,get(i))
    }
    master.mem <- rbind(master.mem,beh_hold)
    
  }
  
  # names correspond to order of bin_list
  name_list <- c("Subj","Age","Sex","CR_Long_T2_L12","CR_Long_T2_L4","CR_Short_T1_L12","CR_Short_T1_L4","FA_Long_T1_L12","FA_Long_T1_L4","FA_Same_T1_L12","FA_Same_T1_L4","FA_Same_T2_L12","FA_Same_T2_L4","FA_Short_T2_L12","FA_Short_T2_L4","Hit_Same_T1_L12","Hit_Same_T1_L4","Hit_Same_T2_L12","Hit_Same_T2_L4","LureR_T1_L12","LureR_T1_L4","LureR_T2_L12","LureR_T2_L4","Miss_Long_T1_L12","Miss_Long_T1_L4","Miss_Long_T2_L12","Miss_Long_T2_L4","Miss_Short_T1_L12","Miss_Short_T1_L4","Miss_Short_T2_L12","Miss_Short_T2_L4","TargR_T1_L12","TargR_T1_L4","TargR_T2_L12","TargR_T2_L4")
  colnames(master.mem) <- name_list
  write.table(master.mem,paste0(dataDir,"Master_Memory_bin_counts.txt"),row.names = F, quote = F, sep = '\t')
}




##### Step 2 Stats
#---------------------------------
if(runStats == 1){
  
  df.Master <- read.delim(paste0(dataDir,"Master_Memory_bin_counts.txt"))

  ### Get d' scores
  ## d'T1 L4 (L->S)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T1_L4)
  df.hold[,2] <- as.numeric(df.Master$Miss_Long_T1_L4)
  df.hold[,3] <- as.numeric(df.Master$TargR_T1_L4)
  df.hold[,4] <- as.numeric(df.Master$FA_Long_T1_L4)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T1_L4)
  df.hold[,6] <- as.numeric(df.Master$LureR_T1_L4)
  T1L4 <- dprime.Function(df.hold)
  
  
  ## d'T1 L12
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T1_L12)
  df.hold[,2] <- as.numeric(df.Master$Miss_Long_T1_L12)
  df.hold[,3] <- as.numeric(df.Master$TargR_T1_L12)
  df.hold[,4] <- as.numeric(df.Master$FA_Long_T1_L12)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T1_L12)
  df.hold[,6] <- as.numeric(df.Master$LureR_T1_L12)
  T1L12 <- dprime.Function(df.hold)
  
  
  ## d'T1 (L4+12)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T1_L4) + as.numeric(df.Master$Hit_Same_T1_L12)
  df.hold[,2] <- as.numeric(df.Master$Miss_Long_T1_L4) + as.numeric(df.Master$Miss_Long_T1_L12)
  df.hold[,3] <- as.numeric(df.Master$TargR_T1_L4) + as.numeric(df.Master$TargR_T1_L12)
  df.hold[,4] <- as.numeric(df.Master$FA_Long_T1_L4) + as.numeric(df.Master$FA_Long_T1_L12)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T1_L4) + as.numeric(df.Master$FA_Same_T1_L12)
  df.hold[,6] <- as.numeric(df.Master$LureR_T1_L4) + as.numeric(df.Master$LureR_T1_L12)
  T1 <- dprime.Function(df.hold)
  
  
  ## d'T2 L4 (S->L)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T2_L4)
  df.hold[,2] <- as.numeric(df.Master$Miss_Short_T2_L4)
  df.hold[,3] <- as.numeric(df.Master$TargR_T2_L4)
  df.hold[,4] <- as.numeric(df.Master$FA_Short_T2_L4)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T2_L4)
  df.hold[,6] <- as.numeric(df.Master$LureR_T2_L4)
  T2L4 <- dprime.Function(df.hold)
  
  
  ## d'T2 L12
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T2_L12)
  df.hold[,2] <- as.numeric(df.Master$Miss_Short_T2_L12)
  df.hold[,3] <- as.numeric(df.Master$TargR_T2_L12)
  df.hold[,4] <- as.numeric(df.Master$FA_Short_T2_L12)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T2_L12)
  df.hold[,6] <- as.numeric(df.Master$LureR_T2_L12)
  T2L12 <- dprime.Function(df.hold)
  
  
  ## d'T2
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$Hit_Same_T2_L4) + as.numeric(df.Master$Hit_Same_T2_L12)
  df.hold[,2] <- as.numeric(df.Master$Miss_Short_T2_L4) + as.numeric(df.Master$Miss_Short_T2_L12)
  df.hold[,3] <- as.numeric(df.Master$TargR_T2_L4) + as.numeric(df.Master$TargR_T2_L12)
  df.hold[,4] <- as.numeric(df.Master$FA_Short_T2_L4) + as.numeric(df.Master$FA_Short_T2_L12)
  df.hold[,5] <- as.numeric(df.Master$FA_Same_T2_L4) + as.numeric(df.Master$FA_Same_T2_L12)
  df.hold[,6] <- as.numeric(df.Master$LureR_T2_L4) + as.numeric(df.Master$LureR_T2_L12)
  T2 <- dprime.Function(df.hold)
  
  
  ## Test d' scores
  df.anova <- WLC.Function(T1L4,T1L12,T2L4,T2L12)
  anova.out <- ezANOVA(df.anova,dv=Dprime,wid=Subj,within=c(Dur,Lag),type='III')
  h.out <- capture.output(print(anova.out))
  write.table(h.out,paste0(dataDir,"Stats_Anova_dprime_DurXLag.txt"),row.names = F, quote = F, sep = '\t')
  
  
  #ttest
  T1vT2 <- ttest.Function(T1,T2,"T1","T2")
  h.out <- capture.output(print(T1vT2))
  write.table(h.out,paste0(dataDir,"Stats_TTest_dprime_T1vT2.txt"),row.names = F, quote = F, sep = '\t')
  
  
  
  ## make D' table
  df.out <- matrix(NA,nrow=dim(df.Master)[1],ncol=7)
  df.out[,1] <- as.character(df.Master[,1])
  df.out[,2] <- round(T1,2)
  df.out[,3] <- round(T2,2)
  df.out[,4] <- round(T1L4,2)
  df.out[,5] <- round(T1L12,2)
  df.out[,6] <- round(T2L4,2)
  df.out[,7] <- round(T2L12,2)
  colnames(df.out) <- c("Subj","T1","T2","T1L4","T1L12","T2L4","T2L12")
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
  df.outlier <- matrix(NA,nrow=dim(df.Master)[1],ncol=7)
  colnames(df.outlier) <- c("Subj","T1","T2","T1L4","T1L12","T2L4","T2L12")
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
  #anova
  df.outlier_long <- WLC.Function(df.outlier[,4],df.outlier[,5],df.outlier[,6],df.outlier[,7])
  anova.no_out <- ezANOVA(df.outlier_long,dv=Dprime,wid=Subj,within=c(Dur,Lag),type='III')
  h.out <- capture.output(print(anova.no_out))
  write.table(h.out,paste0(dataDir,"Stats_Anova_dprime_DurXLag_noOutlier.txt"),row.names = F, quote = F, sep = '\t')
  
  #ttest
  T1vT2 <- ttest.Function(as.numeric(df.outlier[,2]),as.numeric(df.outlier[,3]),"T1","T2")
  h.out <- capture.output(print(T1vT2))
  write.table(h.out,paste0(dataDir,"Stats_TTest_dprime_T1vT2_noOutlier.txt"),row.names = F, quote = F, sep = '\t')

  
  
}




##### Step 3 Graphs
#---------------------------------
if(makeGraphs == 1){
  
  df.dprime <- read.delim(paste0(dataDir,"Master_dprime.txt"))


  ### T1vT2 (T1 = L->S)
  df.graph <- matrix(NA,nrow=dim(df.dprime)[1],ncol=2)
  colnames(df.graph) <- c("L -> S","S -> L")
  df.graph[,1] <- df.dprime[,2]
  df.graph[,2] <- df.dprime[,3]

  tiff(paste0(dataDir,"Fig_T1vT2_dprime.tiff"), height = 5.5, width = 5.5, units = 'in', res=300)
  par(family="Times New Roman")
  hold.graph <- boxplot(df.graph, ylim=c(-1,3), ylab="d' scores", col="white", cex.lab=1.5, cex.axis=1)
  title(main=list("Sensitivity to Duration", cex=1.5))
  abline(h = 0)

  # par(xpd=TRUE)
  # text(1,2.9,"***",cex=1)
  # text(2,2.7,"***",cex=1)
  dev.off()


  # ### T1vT2 (T1 = L->S)
  # df.graph <- matrix(NA,nrow=dim(df.dprime)[1],ncol=4)
  # colnames(df.graph) <- c("Lag 4 L -> S","Lag 12 L -> S","Lag 4 S -> L","Lag 12 S -> L")
  # df.graph[,1] <- df.dprime[,4]
  # df.graph[,2] <- df.dprime[,5]
  # df.graph[,3] <- df.dprime[,6]
  # df.graph[,4] <- df.dprime[,7]
  # 
  # tiff(paste0(dataDir,"Fig_LagXDur_dprime.tiff"), height = 7, width = 7, units = 'in', res=300)
  # par(family="Times New Roman")
  # hold.graph <- boxplot(df.graph, ylim=c(-1,3), ylab="d' scores", col="white", cex.lab=1.5, cex.axis=1)
  # title(main=list("Sensitivity to Duration, per lag", cex=1.5))
  # abline(h = 0)
  # dev.off()
  
  
  
  # ### plotting the SNR curve (based on Ari code)
  # 
  # # parameters
  # par(mar=c(0.1,0.1,0.1,0.1))
  # d1 <- dnorm(seq(-5,5,0.01),sd=0.5)
  # d2 <- dnorm(seq(-5,5,0.01),mean=1.2,sd=0.5)
  # 
  # alpha = 0.7
  # col1 <- "DarkBlue"
  # col2 <- "DarkRed"
  # 
  # cord.x1 <- c(alpha,seq(alpha,5,0.01),5)
  # cord.y1 <- c(0,dnorm(seq(alpha,5,0.01),sd=0.5),0)
  # cord.x2 <- c(-4,seq(-4,alpha,0.01),alpha)
  # cord.y2 <- c(0,dnorm(seq(-4,alpha,0.01),mean=1.2,sd=0.5),0)
  # 
  # # plot
  # tiff(paste0(dataDir,"Fig_dprime_calc.tiff"), height = 7, width = 7, units = 'in', res=300)
  # par(family="Times New Roman")
  # plot(seq(-5,5,0.01),d1,type='l',ylab="",xlab="",xlim=c(-2,3),axes=F,lwd=3,col=col1,ylim=c(0,1))
  # title(expression("d'"[" 1.5s"] ~ "= z(pHit) - z(pFA)"),adj=0,cex.main=1.5)
  # lines(seq(-5,5,0.01),d2,lwd=3,col=col2)
  # abline(h=0)
  # 
  # # text
  # text(0,0.9,"T=1",cex=1.5,col=col1); text(0,0.85,"Lure",cex=1.2,col=col1)
  # text(1.2,0.9,"T=1.5",cex=1.5,col=col2); text(1.2,0.85,"Target",cex=1.2,col=col2)
  # 
  # # beh
  # segments(0,0.5,-0.9,0.5,col=col1); text(-1.1,0.5,"CRS",col=col1)
  # segments(1.2,0.5,2.0,0.5,col=col2); text(2.2,0.5,"HitSa",col=col2)
  # segments(1.8,0.25,2.4,0.25,col=col2); text(2.6,0.25,"MissL",col=col2)
  # segments(0.5,0.1,-0.2,0.1,col=col2); text(-0.4,0.1,"MissS",col=col2)
  # segments(0.6,0.25,1.1,0.25,col=col1); text(1.3,0.25,"FASa",col=col1)
  # segments(0.9,0.1,1.5,0.1,col=col1); text(1.65,0.1,"FAL",col=col1)
  # 
  # # formula
  # text(-1.37,1,"pHit = HitSa + MissL",cex=0.8)
  # segments(-1.6,0.98,-0.8,0.98)
  # text(-1.3,0.95,"1.5s Targets",cex=0.8)
  # text(-1.4,0.9,"pFA = FAL + FASa",cex=0.8)
  # segments(-1.6,0.88,-0.8,0.88)
  # text(-1.3,0.85,"1s Lures",cex=0.8)
  # 
  # dev.off()
}

