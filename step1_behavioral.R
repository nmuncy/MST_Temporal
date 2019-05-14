
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

# Variables
res.Old <- " 1"
res.Sim <- " 2"
res.New <- " 3"


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
  # master.pos <- matrix(NA,nrow=length(subjList),ncol=15)
  

  # j<-"p002"
  
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
              if(h.res == res.Old){
                df.mem[i,7] <- "Miss_Long_T1_4"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "Hit_Same_T1_4"
              }else if(h.res == res.New){
                df.mem[i,7] <- "Miss_Short_T1_4"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == res.Old){
                df.mem[i,7] <- "FA_Long_T1_4"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "FA_Same_T1_4"
              }else if(h.res == res.New){
                df.mem[i,7] <- "CR_Short_T1_4"
              }else{df.mem[i,7] <- 999}
            }
          }else{
            if(h.typ == " Targ"){
              if(h.res == res.Old){
                df.mem[i,7] <- "Miss_Long_T2_4"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "Hit_Same_T2_4"
              }else if(h.res == res.New){
                df.mem[i,7] <- "Miss_Short_T2_4"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == res.Old){
                df.mem[i,7] <- "CR_Long_T2_4"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "FA_Same_T2_4"
              }else if(h.res == res.New){
                df.mem[i,7] <- "FA_Short_T2_4"
              }else{df.mem[i,7] <- 999}
            }
          }
        }else if(h.lag == " 12"){
          if(h.dur == " T1"){
            if(h.typ == " Targ"){
              if(h.res == res.Old){
                df.mem[i,7] <- "Miss_Long_T1_12"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "Hit_Same_T1_12"
              }else if(h.res == res.New){
                df.mem[i,7] <- "Miss_Short_T1_12"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == res.Old){
                df.mem[i,7] <- "FA_Long_T1_12"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "FA_Same_T1_12"
              }else if(h.res == res.New){
                df.mem[i,7] <- "CR_Short_T1_12"
              }else{df.mem[i,7] <- 999}
            }
          }else{
            if(h.typ == " Targ"){
              if(h.res == res.Old){
                df.mem[i,7] <- "Miss_Long_T2_12"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "Hit_Same_T2_12"
              }else if(h.res == res.New){
                df.mem[i,7] <- "Miss_Short_T2_12"
              }else{df.mem[i,7] <- 999}
            }else{
              if(h.res == res.Old){
                df.mem[i,7] <- "CR_Long_T2_12"
              }else if(h.res == res.Sim){
                df.mem[i,7] <- "FA_Same_T2_12"
              }else if(h.res == res.New){
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

  ### Get d' scores
  ## d'T1 L4 (L->S)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit4_Same_T1)
  df.hold[,2] <- as.numeric(df.Master$n.Miss4_Long_T1)
  df.hold[,3] <- as.numeric(df.Master$n.TargR4_T1)
  df.hold[,4] <- as.numeric(df.Master$n.FA4_Long_T1)
  df.hold[,5] <- as.numeric(df.Master$n.FA4_Same_T1)
  df.hold[,6] <- as.numeric(df.Master$n.LureR4_T1)
  T1L4 <- dprime.Function(df.hold)
  
  
  ## d'T1 L12
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit12_Same_T1)
  df.hold[,2] <- as.numeric(df.Master$n.Miss12_Long_T1)
  df.hold[,3] <- as.numeric(df.Master$n.TargR12_T1)
  df.hold[,4] <- as.numeric(df.Master$n.FA12_Long_T1)
  df.hold[,5] <- as.numeric(df.Master$n.FA12_Same_T1)
  df.hold[,6] <- as.numeric(df.Master$n.LureR12_T1)
  T1L12 <- dprime.Function(df.hold)
  
  
  ## d'T1 (L4+12)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit4_Same_T1) + as.numeric(df.Master$n.Hit12_Same_T1)
  df.hold[,2] <- as.numeric(df.Master$n.Miss4_Long_T1) + as.numeric(df.Master$n.Miss12_Long_T1)
  df.hold[,3] <- as.numeric(df.Master$n.TargR4_T1) + as.numeric(df.Master$n.TargR12_T1)
  df.hold[,4] <- as.numeric(df.Master$n.FA4_Long_T1) + as.numeric(df.Master$n.FA12_Long_T1)
  df.hold[,5] <- as.numeric(df.Master$n.FA4_Same_T1) + as.numeric(df.Master$n.FA12_Same_T1)
  df.hold[,6] <- as.numeric(df.Master$n.LureR4_T1) + as.numeric(df.Master$n.LureR12_T1)
  T1 <- dprime.Function(df.hold)
  
  
  ## d'T2 L4 (S->L)
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit4_Same_T2)
  df.hold[,2] <- as.numeric(df.Master$n.Miss4_Short_T2)
  df.hold[,3] <- as.numeric(df.Master$n.TargR4_T2)
  df.hold[,4] <- as.numeric(df.Master$n.FA4_Short_T2)
  df.hold[,5] <- as.numeric(df.Master$n.FA4_Same_T2)
  df.hold[,6] <- as.numeric(df.Master$n.LureR4_T2)
  T2L4 <- dprime.Function(df.hold)
  
  
  ## d'T2 L12
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit12_Same_T2)
  df.hold[,2] <- as.numeric(df.Master$n.Miss12_Short_T2)
  df.hold[,3] <- as.numeric(df.Master$n.TargR12_T2)
  df.hold[,4] <- as.numeric(df.Master$n.FA12_Short_T2)
  df.hold[,5] <- as.numeric(df.Master$n.FA12_Same_T2)
  df.hold[,6] <- as.numeric(df.Master$n.LureR12_T2)
  T2L12 <- dprime.Function(df.hold)
  
  
  ## d'T2
  df.hold <- matrix(NA, nrow=dim(df.Master)[1],ncol=6)
  df.hold[,1] <- as.numeric(df.Master$n.Hit4_Same_T2) + as.numeric(df.Master$n.Hit12_Same_T2)
  df.hold[,2] <- as.numeric(df.Master$n.Miss4_Short_T2) + as.numeric(df.Master$n.Miss12_Short_T2)
  df.hold[,3] <- as.numeric(df.Master$n.TargR4_T2) + as.numeric(df.Master$n.TargR12_T2)
  df.hold[,4] <- as.numeric(df.Master$n.FA4_Short_T2) + as.numeric(df.Master$n.FA12_Short_T2)
  df.hold[,5] <- as.numeric(df.Master$n.FA4_Same_T2) + as.numeric(df.Master$n.FA12_Same_T2)
  df.hold[,6] <- as.numeric(df.Master$n.LureR4_T2) + as.numeric(df.Master$n.LureR12_T2)
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
  df.out[,2] <- T1
  df.out[,3] <- T2
  df.out[,4] <- T1L4
  df.out[,5] <- T1L12
  df.out[,6] <- T2L4
  df.out[,7] <- T2L12
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













##### Old/extra code
#--------------------------#



# if(h.rep == " Yes"){
#   if(h.typ == " Targ"){
#     if(h.res == " b" || h.res == " B"){
#       df.mem[i,7] <- "Miss_Long"
#     }else if(h.res == " n" || h.res == " N"){
#       df.mem[i,7] <- "Hit_Same"
#     }else if(h.res == " m" || h.res == " M"){
#       df.mem[i,7] <- "Miss_Short"
#     }else{
#       df.mem[i,7] <- 999
#     }
#   }else{
#     if(h.dur == " T1"){
#       if(h.res == " b" || h.res == " B"){
#         df.mem[i,7] <- "FA_Long"
#       }else if(h.res == " n" || h.res == " N"){
#         df.mem[i,7] <- "FA_Same_L"
#       }else if(h.res == " m" || h.res == " M"){
#         df.mem[i,7] <- "CR_Short"
#       }else{
#         df.mem[i,7] <- 999
#       }
#     }else{
#       if(h.res == " b" || h.res == " B"){
#         df.mem[i,7] <- "CR_Long"
#       }else if(h.res == " n" || h.res == " N"){
#         df.mem[i,7] <- "FA_Same_S"
#       }else if(h.res ==  " m" || h.res == " M"){
#         df.mem[i,7] <- "FA_Short"
#       }else{
#         df.mem[i,7] <- 999
#       }
#     }
#   }
# }else{
#   df.mem[i,7] <- "EncR"
# }



# 
# for(i in 1:dim(df.mem)[1]){
#   if(grepl("Targ",df.mem[i,2])==T && grepl("Yes",df.mem[i,4])==T){
#     if(grepl("999",df.mem[i,6])==F){
#       ind.TargR <- c(ind.TargR,i)
#     }
#   }else if(grepl("Lure",df.mem[i,2])==T && grepl("Yes",df.mem[i,4])==T){
#     if(grepl("999",df.mem[i,6])==F){
#       ind.LureR <- c(ind.LureR,i)
#       if(grepl("T1",df.mem[i,3])==T){
#         n.LureR_short <- n.LureR_short+1
#       }else if(grepl("T2",df.mem[i,3])==T){
#         n.LureR_long <- n.LureR_long+1
#       }
#     }
#   }
# }




## Targ
# df.Targ <- matrix(0,nrow=n.TargR,ncol=2)
# for(i in 1:length(ind.TargR)){
#   df.Targ[i,1] <- df.mem[ind.TargR[i],5]
#   df.Targ[i,2] <- df.mem[ind.TargR[i],7]
# }

# Targ Behaviors




# for(i in 1:dim(df.Targ)[1]){
#   if(grepl("Hit",df.Targ[i,2])==T){
#     n.Hit <- n.Hit+1
#     if(grepl("4",df.Targ[i,1])==T){
#       n.Hit4 <- n.Hit4+1
#     }else{
#       n.Hit12 <- n.Hit12+1
#     }
#   }else{
#     n.Miss <- n.Miss+1
#     if(grepl("4",df.Targ[i,1])==T){
#       n.Miss4 <- n.Miss4+1
#       if(grepl("Short",df.Targ[i,2])==T){
#         n.Miss4_S <- n.Miss4_S+1
#       }else{
#         n.Miss4_L <- n.Miss4_L+1
#       }
#     }else{
#       n.Miss12 <- n.Miss12+1
#       if(grepl("Short",df.Targ[i,2])==T){
#         n.Miss12_S <- n.Miss12_S+1
#       }else{
#         n.Miss12_L <- n.Miss12_L+1
#       }
#     }
#     if(grepl("Short",df.Targ[i,2])==T){
#       n.Miss_S <- n.Miss_S+1
#     }else{
#       n.Miss_L <- n.Miss_L+1
#     }
#   }
# }
# 
# 
# ## Lure
# df.Lure <- matrix(0,nrow=n.LureR,ncol=2)
# for(i in 1:length(ind.LureR)){
#   df.Lure[i,1] <- df.mem[ind.LureR[i],5]
#   df.Lure[i,2] <- df.mem[ind.LureR[i],7]
# }
# 
# # FAs
# n.FA<-0; n.FA4<-0; n.FA12<-0
# n.FA_Short<-0; n.FA_Long<-0; n.FA_Same_L<-0; n.FA_Same_S<-0
# n.FA4_Short<-0; n.FA4_Long<-0; n.FA4_Same_L<-0; n.FA4_Same_S<-0
# n.FA12_Short<-0; n.FA12_Long<-0; n.FA12_Same_L<-0; n.FA12_Same_S<-0
# 
# for(i in 1:dim(df.Lure)[1]){
#   if(grepl("FA",df.Lure[i,2])==T){
#     n.FA <- n.FA+1
#     if(grepl("4",df.Lure[i,1])==T){
#       n.FA4 <- n.FA4+1
#       if(grepl("Short",df.Lure[i,2])==T){
#         n.FA4_Short <- n.FA4_Short+1
#       }else if(grepl("Same_L",df.Lure[i,2])==T){
#         n.FA4_Same_L <- n.FA4_Same_L+1
#       }else if(grepl("Same_S",df.Lure[i,2])==T){
#         n.FA4_Same_S <- n.FA4_Same_S+1
#       }else{
#         n.FA4_Long <- n.FA4_Long+1
#       }
#     }else{
#       n.FA12 <- n.FA12+1
#       if(grepl("Short",df.Lure[i,2])==T){
#         n.FA12_Short <- n.FA12_Short+1
#       }else if(grepl("Same_L",df.Lure[i,2])==T){
#         n.FA12_Same_L <- n.FA12_Same_L+1
#       }else if(grepl("Same_S",df.Lure[i,2])==T){
#         n.FA12_Same_S <- n.FA12_Same_S+1
#       }else{
#         n.FA12_Long <- n.FA12_Long+1
#       }
#     }
#     if(grepl("Short",df.Lure[i,2])==T){
#       n.FA_Short <- n.FA_Short+1
#     }else if(grepl("Same_L",df.Lure[i,2])==T){
#       n.FA_Same_L <- n.FA_Same_L+1
#     }else if(grepl("Same_S",df.Lure[i,2])==T){
#       n.FA_Same_S <- n.FA_Same_S+1
#     }else{
#       n.FA_Long <- n.FA_Long+1
#     }
#   }
# }
# 
# # CRs
# n.CR<-0; n.CR4<-0; n.CR12<-0
# n.CR_Short<-0; n.CR_Long<-0
# n.CR4_Short<-0; n.CR4_Long<-0
# n.CR12_Short<-0; n.CR12_Long<-0
# 
# for(i in 1:dim(df.Lure)[1]){
#   if(grepl("CR",df.Lure[i,2])==T){
#     n.CR <- n.CR+1
#     if(grepl("4",df.Lure[i,1])==T){
#       n.CR4 <- n.CR4+1
#       if(grepl("Short",df.Lure[i,2])==T){
#         n.CR4_Short <- n.CR4_Short+1
#       }else{
#         n.CR4_Long <- n.CR4_Long+1
#       }
#     }else{
#       n.CR12 <- n.CR12+1
#       if(grepl("Short",df.Lure[i,2])==T){
#         n.CR12_Short <- n.CR12_Short+1
#       }else{
#         n.CR12_Long <- n.CR12_Long+1
#       }
#     }
#     if(grepl("Short",df.Lure[i,2])==T){
#       n.CR_Short <- n.CR_Short+1
#     }else{
#       n.CR_Long <- n.CR_Long+1
#     }
#   }
# }






#### Posner
# # posner responses, no missed/multiple responses
# df.pos <- matrix(NA,nrow=length(ind.pos),ncol=1)
# for(i in 1:length(ind.pos)){
#   hold1 <- ind.pos[i]+5
#   hold2 <- ind.pos[i]+7
#   if(grepl("Response",df.subj[hold1,1])==T && grepl("Trial Start",df.subj[hold2,1])==T){
#     df.pos[i,1] <- df.subj[hold1,2]
#   }else{
#     df.pos[i,1] <- 999
#   }
# }
# 

# ## Posner
# for(i in 1:2){
#   data.hold <- read.table(paste0(subjDir,as.character(j),"_P",i,"_stimuli.txt"))
#   assign(paste0("posStim",i),data.hold[-1,])
# }
# hold.pos <- rbind(posStim1,posStim2)
# 
# 
# # get other data
# df.pos <- cbind(df.pos,as.character(hold.pos[,3]))
# df.pos <- cbind(df.pos,as.character(hold.pos[,4]))
# df.pos <- cbind(df.pos, NA)
# 
# 
# # determine behavior, left/right = cue direction
# for(i in 1:dim(df.pos)[1]){
#   if(df.pos[i,1]!="999"){
#     if(grepl("ICon",df.pos[i,3])==T){
#       if(grepl("Left",df.pos[i,3])==T){
#         if(df.pos[i,1]==" n"){
#           df.pos[i,4] <- "IC-Miss"
#         }else{
#           df.pos[i,4] <- "IC-Hit"
#         }
#       }else{
#         if(df.pos[i,1]==" n"){
#           df.pos[i,4] <- "IC-Hit"
#         }else{
#           df.pos[i,4] <- "IC-Miss"
#         }
#       }
#     }else{
#       if(grepl("Left",df.pos[i,3])==T){
#         if(df.pos[i,1]==" n"){
#           df.pos[i,4] <- "C-Hit"
#         }else{
#           df.pos[i,4] <- "C-Miss"
#         }
#       }else{
#         if(df.pos[i,1]==" n"){
#           df.pos[i,4] <- "C-Miss"
#         }else{
#           df.pos[i,4] <- "C-Hit"
#         }
#       }
#     }
#   }else{
#     df.pos[i,4] <- "--"
#   }
# }
# colnames(df.pos) <- c("Response","ISI","StimType","Behavior")
# 
# 
# # Determine counts
# # aggregate
# n.ch<-0; n.cm<-0; n.ich<-0; n.icm<-0
# 
# for(i in 1:dim(df.pos)[1]){
#   if(grepl("IC",df.pos[i,4])==T){
#     if(grepl("Miss",df.pos[i,4])==T){
#       n.icm<-n.icm+1
#     }else{
#       n.ich<-n.ich+1
#     }
#   }else{
#     if(grepl("Miss",df.pos[i,4])==T){
#       n.cm<-n.cm+1
#     }else{
#       n.ch<-n.ch+1
#     }
#   }
# }
# 
# 
# # per ISI
# n.ch2<-0; n.cm2<-0; n.ich2<-0; n.icm2<-0
# n.ch4<-0; n.cm4<-0; n.ich4<-0; n.icm4<-0
# 
# for(i in 1:dim(df.pos)[1]){
#   if(df.pos[i,2]=="0.2"){
#     if(grepl("IC",df.pos[i,4])==T){
#       if(grepl("Miss",df.pos[i,4])==T){
#         n.icm2<-n.icm2+1
#       }else{
#         n.ich2<-n.ich2+1
#       }
#     }else{
#       if(grepl("Miss",df.pos[i,4])==T){
#         n.cm2<-n.cm2+1
#       }else{
#         n.ch2<-n.ch2+1
#       }
#     }
#   }else{
#     if(grepl("IC",df.pos[i,4])==T){
#       if(grepl("Miss",df.pos[i,4])==T){
#         n.icm4<-n.icm4+1
#       }else{
#         n.ich4<-n.ich4+1
#       }
#     }else{
#       if(grepl("Miss",df.pos[i,4])==T){
#         n.cm4<-n.cm4+1
#       }else{
#         n.ch4<-n.ch4+1
#       }
#     }
#   }
# }
# 
# 
# # make output
# df.posBeh <- matrix(NA,nrow=3,ncol=4)
# colnames(df.posBeh) <- c("CH","CM","ICH","ICM")
# rownames(df.posBeh) <- c("A",".2",".4")
# 
# df.posBeh[1,] <- c(n.ch,n.cm,n.ich,n.icm)
# df.posBeh[2,] <- c(n.ch2,n.cm2,n.ich2,n.icm2)
# df.posBeh[3,] <- c(n.ch4,n.cm4,n.ich4,n.icm4)
# 
# 
# 
# 
# ### write out
# # participant
# write.table(df.mem,paste0(subjDir,"Memory_data.txt"),row.names = F, quote = F, sep = '\t')
# write.table(df.pos,paste0(subjDir,"Posner_data.txt"),row.names = F, quote = F, sep = '\t')
# write.table(df.memBeh,paste0(subjDir,"Memory_count.txt"),row.names = T, quote = F, sep = '\t')
# write.table(df.posBeh,paste0(subjDir,"Posner_count.txt"),row.names = T, quote = F, sep = '\t')
# 
# # master
# master.mem <- rbind(master.mem, c(j,age,sex,n.hit,n.fa,n.miss,n.cr,n.4hit,n.4fa,n.4miss,n.4cr,n.8hit,n.8fa,n.8miss,n.8cr,n.12hit,n.12fa,n.12miss,n.12cr,n.16hit,n.16fa,n.16miss,n.16cr))
# master.pos <- rbind(master.pos, c(j,age,sex,n.ch,n.cm,n.ich,n.icm,n.ch2,n.cm2,n.ich2,n.icm2,n.ch4,n.cm4,n.ich4,n.icm4))


# master.pos <- na.omit(master.pos)
# write.table(master.pos,paste0(dataDir,"Master_Posner.txt"),row.names = F, quote = F, sep = '\t')
