
library(reshape2)
library(ez)




### This is the same script as Exp1_Beh_step1_Stats.R, just
# simplified since we are not investigating Lag/Dur/Intx.




##### Set up
parDir <- '/Volumes/Yorick/Temporal/'
workDir <- paste0(parDir,"Experiment2/")
dataDir <- paste0(parDir,"Analyses/Exp2/behAnalysis/")

# Toggles
makeData <- 1
runStats <- 1
makeGraphs <- 1


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
  
  subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
  subjList <- t(subjList)
  
  # Prepare output
  master.mem <- matrix(NA,nrow=0,ncol=35)
  colnames(master.mem) <- c("Subj","Age","Sex","n.TargR4_T1","n.TargR12_T1","n.TargR4_T2","n.TargR12_T2","n.LureR4_T1","n.LureR12_T1","n.LureR4_T2","n.LureR12_T2","n.Hit4_Same_T1","n.Hit12_Same_T1","n.Hit4_Same_T2","n.Hit12_Same_T2","n.Miss4_Long_T1","n.Miss12_Long_T1","n.Miss4_Short_T1","n.Miss12_Short_T1","n.Miss4_Long_T2","n.Miss12_Long_T2","n.Miss4_Short_T2","n.Miss12_Short_T2","n.CR4_Short_T1","n.CR12_Short_T1","n.CR4_Long_T2","n.CR12_Long_T2","n.FA4_Long_T1","n.FA12_Long_T1","n.FA4_Same_T1","n.FA12_Same_T1","n.FA4_Short_T2","n.FA12_Short_T2","n.FA4_Same_T2","n.FA12_Same_T2")

  for(j in subjList){
    
    subjDir <- paste0(workDir,j,"/")
    df.subj <- read.csv(paste0(subjDir,j,"_timing_log.csv"))
    df.subj <- as.data.frame(df.subj)
    
    
    # demographics
    age <- as.numeric(as.character(df.subj[2,2]))
    sex <- gsub("\\","",as.character(df.subj[3,2]),fixed=T)

    
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
      
      # Behavior
      # b = long, n = same, m = short
      h.typ <- df.mem[i,2]
      h.dur <- df.mem[i,3]
      h.rep <- df.mem[i,4]
      h.lag <- df.mem[i,5]
      h.res <- df.mem[i,6]
      
      if(h.rep == " Yes"){
        if(h.lag == " 4"){
          if(h.dur == " T1"){
            if(h.typ == " Targ"){
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "Miss_Long_T1_4"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "Hit_Same_T1_4"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "Miss_Short_T1_4"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "FA_Long_T1_4"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "FA_Same_T1_4"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "CR_Short_T1_4"
              }else{df.mem[i,7] <- 999}
            }
          }else{
            if(h.typ == " Targ"){
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "Miss_Long_T2_4"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "Hit_Same_T2_4"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "Miss_Short_T2_4"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "CR_Long_T2_4"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "FA_Same_T2_4"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "FA_Short_T2_4"
              }else{df.mem[i,7] <- 999}
            }
          }
        }else if(h.lag == " 12"){
          if(h.dur == " T1"){
            if(h.typ == " Targ"){
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "Miss_Long_T1_12"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "Hit_Same_T1_12"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "Miss_Short_T1_12"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "FA_Long_T1_12"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "FA_Same_T1_12"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "CR_Short_T1_12"
              }else{df.mem[i,7] <- 999}
            }
          }else{
            if(h.typ == " Targ"){
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "Miss_Long_T2_12"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "Hit_Same_T2_12"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "Miss_Short_T2_12"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == " b" || h.res == " B"){
                df.mem[i,7] <- "CR_Long_T2_12"
              }else if(h.res == " n" || h.res == " N"){
                df.mem[i,7] <- "FA_Same_T2_12"
              }else if(h.res == " m" || h.res == " M"){
                df.mem[i,7] <- "FA_Short_T2_12"
              }else{df.mem[i,7] <- 999}
            }
          }
        }
      }else{df.mem[i,7] <- "EncR"}
    }
    
    # write 
    write.table(df.mem,paste0(subjDir,"Memory_data.txt"),row.names = F, quote = F, sep = '\t')
    
    
    ### Behavioral counts - surely there is a better way?

    n.TargR4_T1<-0; n.TargR12_T1<-0
    n.TargR4_T2<-0; n.TargR12_T2<-0

    n.LureR4_T1<-0; n.LureR12_T1<-0
    n.LureR4_T2<-0; n.LureR12_T2<-0

    n.Hit4_Same_T1<-0; n.Hit12_Same_T1<-0
    n.Hit4_Same_T2<-0; n.Hit12_Same_T2<-0

    n.Miss4_Long_T1<-0; n.Miss12_Long_T1<-0; n.Miss4_Short_T1<-0; n.Miss12_Short_T1<-0
    n.Miss4_Long_T2<-0; n.Miss12_Long_T2<-0; n.Miss4_Short_T2<-0; n.Miss12_Short_T2<-0

    n.CR4_Short_T1<-0; n.CR12_Short_T1<-0
    n.CR4_Long_T2<-0; n.CR12_Long_T2<-0

    n.FA4_Long_T1<-0; n.FA12_Long_T1<-0; n.FA4_Same_T1<-0; n.FA12_Same_T1<-0
    n.FA4_Short_T2<-0; n.FA12_Short_T2<-0; n.FA4_Same_T2<-0; n.FA12_Same_T2<-0

    for(i in 1:dim(df.mem)[1]){
      if(grepl("999",df.mem[i,6])==F){
        if(grepl("T1",df.mem[i,3])==T){
          if(grepl("Targ",df.mem[i,2])==T && grepl("Yes",df.mem[i,4])==T){
            if(grepl("Hit_Same",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.TargR4_T1<-n.TargR4_T1+1
                n.Hit4_Same_T1<-n.Hit4_Same_T1+1
              }else{
                n.TargR12_T1<-n.TargR12_T1+1
                n.Hit12_Same_T1<-n.Hit12_Same_T1+1
              }
            }else if(grepl("Miss_Long",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.TargR4_T1<-n.TargR4_T1+1
                n.Miss4_Long_T1<-n.Miss4_Long_T1+1
              }else{
                n.TargR12_T1<-n.TargR12_T1+1
                n.Miss12_Long_T1<-n.Miss12_Long_T1+1
              }
            }else if(grepl("Miss_Short",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.TargR4_T1<-n.TargR4_T1+1
                n.Miss4_Short_T1<-n.Miss4_Short_T1+1
              }else{
                n.TargR12_T1<-n.TargR12_T1+1
                n.Miss12_Short_T1<-n.Miss12_Short_T1+1
              }
            }
          }else if(grepl("Lure",df.mem[i,2])==T && grepl("Yes",df.mem[i,4])==T){
            if(grepl("CR_Short",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.LureR4_T1<-n.LureR4_T1+1
                n.CR4_Short_T1<-n.CR4_Short_T1+1
              }else{
                n.LureR12_T1<-n.LureR12_T1+1
                n.CR12_Short_T1<-n.CR12_Short_T1+1
              }
            }else if(grepl("FA_Long",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.LureR4_T1<-n.LureR4_T1+1
                n.FA4_Long_T1<-n.FA4_Long_T1+1
              }else{
                n.LureR12_T1<-n.LureR12_T1+1
                n.FA12_Long_T1<-n.FA12_Long_T1+1
              }
            }else if(grepl("FA_Same",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.LureR4_T1<-n.LureR4_T1+1
                n.FA4_Same_T1<-n.FA4_Same_T1+1
              }else{
                n.LureR12_T1<-n.LureR12_T1+1
                n.FA12_Same_T1<-n.FA12_Same_T1+1
              }
            }
          }
        }else{ 
          if(grepl("Targ",df.mem[i,2])==T && grepl("Yes",df.mem[i,4])==T){
            if(grepl("Hit_Same",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.TargR4_T2<-n.TargR4_T2+1
                n.Hit4_Same_T2<-n.Hit4_Same_T2+1
              }else{
                n.TargR12_T2<-n.TargR12_T2+1
                n.Hit12_Same_T2<-n.Hit12_Same_T2+1
              }
            }else if(grepl("Miss_Long",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.TargR4_T2<-n.TargR4_T2+1
                n.Miss4_Long_T2<-n.Miss4_Long_T2+1
              }else{
                n.TargR12_T2<-n.TargR12_T2+1
                n.Miss12_Long_T2<-n.Miss12_Long_T2+1
              }
            }else if(grepl("Miss_Short",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.TargR4_T2<-n.TargR4_T2+1
                n.Miss4_Short_T2<-n.Miss4_Short_T2+1
              }else{
                n.TargR12_T2<-n.TargR12_T2+1
                n.Miss12_Short_T2<-n.Miss12_Short_T2+1
              }
            }
          }else if(grepl("Lure",df.mem[i,2])==T && grepl("Yes",df.mem[i,4])==T){
            if(grepl("CR_Long",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.LureR4_T2<-n.LureR4_T2+1
                n.CR4_Long_T2<-n.CR4_Long_T2+1
              }else{
                n.LureR12_T2<-n.LureR12_T2+1
                n.CR12_Long_T2<-n.CR12_Long_T2+1
              }
            }else if(grepl("FA_Short",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.LureR4_T2<-n.LureR4_T2+1
                n.FA4_Short_T2<-n.FA4_Short_T2+1
              }else{
                n.LureR12_T2<-n.LureR12_T2+1
                n.FA12_Short_T2<-n.FA12_Short_T2+1
              }
            }else if(grepl("FA_Same",df.mem[i,7])==T){
              if(grepl("4",df.mem[i,5])==T){
                n.LureR4_T2<-n.LureR4_T2+1
                n.FA4_Same_T2<-n.FA4_Same_T2+1
              }else{
                n.LureR12_T2<-n.LureR12_T2+1
                n.FA12_Same_T2<-n.FA12_Same_T2+1
              }
            }
          }
        }
      }
    }
    master.mem <- rbind(master.mem, c(j,age,sex,n.TargR4_T1,n.TargR12_T1,n.TargR4_T2,n.TargR12_T2,n.LureR4_T1,n.LureR12_T1,n.LureR4_T2,n.LureR12_T2,n.Hit4_Same_T1,n.Hit12_Same_T1,n.Hit4_Same_T2,n.Hit12_Same_T2,n.Miss4_Long_T1,n.Miss12_Long_T1,n.Miss4_Short_T1,n.Miss12_Short_T1,n.Miss4_Long_T2,n.Miss12_Long_T2,n.Miss4_Short_T2,n.Miss12_Short_T2,n.CR4_Short_T1,n.CR12_Short_T1,n.CR4_Long_T2,n.CR12_Long_T2,n.FA4_Long_T1,n.FA12_Long_T1,n.FA4_Same_T1,n.FA12_Same_T1,n.FA4_Short_T2,n.FA12_Short_T2,n.FA4_Same_T2,n.FA12_Same_T2))
  }
  write.table(master.mem,paste0(dataDir,"Master_Memory.txt"),row.names = F, quote = F, sep = '\t')
}



##### Step 2 Stats
#---------------------------------
if(runStats == 1){
  
  df.Master <- read.delim(paste0(dataDir,"Master_Memory.txt"))

  ## d'T1 (L4+12)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit4_Same_T1) + as.numeric(df.Master$n.Hit12_Same_T1)
  df.hold[,2] <- as.numeric(df.Master$n.Miss4_Long_T1) + as.numeric(df.Master$n.Miss12_Long_T1)
  df.hold[,3] <- as.numeric(df.Master$n.TargR4_T1) + as.numeric(df.Master$n.TargR12_T1)
  df.hold[,4] <- as.numeric(df.Master$n.FA4_Long_T1) + as.numeric(df.Master$n.FA12_Long_T1)
  df.hold[,5] <- as.numeric(df.Master$n.FA4_Same_T1) + as.numeric(df.Master$n.FA12_Same_T1)
  df.hold[,6] <- as.numeric(df.Master$n.LureR4_T1) + as.numeric(df.Master$n.LureR12_T1)
  T1 <- dprime.Function(df.hold)
  
  ## d'T2
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit4_Same_T2) + as.numeric(df.Master$n.Hit12_Same_T2)
  df.hold[,2] <- as.numeric(df.Master$n.Miss4_Short_T2) + as.numeric(df.Master$n.Miss12_Short_T2)
  df.hold[,3] <- as.numeric(df.Master$n.TargR4_T2) + as.numeric(df.Master$n.TargR12_T2)
  df.hold[,4] <- as.numeric(df.Master$n.FA4_Short_T2) + as.numeric(df.Master$n.FA12_Short_T2)
  df.hold[,5] <- as.numeric(df.Master$n.FA4_Same_T2) + as.numeric(df.Master$n.FA12_Same_T2)
  df.hold[,6] <- as.numeric(df.Master$n.LureR4_T2) + as.numeric(df.Master$n.LureR12_T2)
  T2 <- dprime.Function(df.hold)

  
  #ttest
  T1vT2 <- ttest.Function(T1,T2,"T1","T2")
  h.out <- capture.output(print(T1vT2))
  write.table(h.out,paste0(dataDir,"Stats_TTest_dprime_T1vT2.txt"),row.names = F, quote = F, sep = '\t')
  
  
  
  ## make D' table
  df.out <- matrix(NA,nrow=dim(df.Master)[1],ncol=3)
  df.out[,1] <- as.character(df.Master[,1])
  df.out[,2] <- T1
  df.out[,3] <- T2
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

  tiff(paste0(outDir,"Fig_T1vT2_dprime.tiff"), height = 5.5, width = 5.5, units = 'in', res=300)
  par(family="Times New Roman")
  hold.graph <- boxplot(df.graph, ylim=c(-1,3), ylab="d' scores", col="white", cex.lab=1.5, cex.axis=1)
  title(main=list("Sensitivity to Duration", cex=1.5))
  abline(h = 0)

  par(xpd=TRUE)
  text(1,2.7,"***",cex=1)
  text(2,2.7,"***",cex=1)
  dev.off()
}
