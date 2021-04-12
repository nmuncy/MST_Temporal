


# Written by Nathan Muncy and Ariana Hedges on 2/16/18

### Notes
#
# 1) Run this script from terminal via "Rscript <scriptName>.R s1234"
#   where s1234 is the intended subject number 
#
# 2) This script will create a subject dir, and place a randomized
#   stimulus file in that dir to be read by python script
# 
# 3) Randomization occurs at:
#   - selection of stimuli
#   - assignment to output df
#   - seed for NA search in output df




### Set variables
workDir <- "/Volumes/Yorick/Temporal/Experiment1/"
stimDir <- paste0(workDir,"Stimuli/StimSet/")




### Import terminal argument, make subjDir
args <- commandArgs(TRUE)
subj <- as.character(args[1])
subjDir <- paste0(workDir, subj)
dir.create(file.path(subjDir))




### Functions
# This function will iterate until it detects the needed two positions or timeouts
SeedNA.Function <- function(x,y){
  max.Iter <- 200; count <- 1
  while(count <= max.Iter){
    seed <- sample(which(is.na(y[,1]),arr.ind=T),1)
    lag.pos <- seed + x
    if(lag.pos <= dim(y)[1]){
      if(is.na(y[seed,1])==T && is.na(y[lag.pos,1])==T){
        return(list(seed,lag.pos))
        break 
      }
    }
    count <- count+1
  }
  return("Fail")
}


# Generate random list
List.Function <- function(Targ,Lure,Foil){
  hold.df <- matrix(NA,nrow=54,ncol=5)
  hold.df[,1] <- c(Targ,Lure,Foil)
  hold.df[,2] <- sample(c(rep("T1",27), rep("T2",27)), 54)
  hold.df[,3] <- sample(c(rep("E1",18), rep("E2",18), rep("E3",18)), 54)
  hold.df[1:40,4] <- sample(c(rep("Targ",20), rep("Lure",20)),40)
  hold.df[41:54,4] <- "Foil"
  hold.df[1:40,5] <- sample(c(rep(4,20), rep(12,20)),40)
  hold.df[41:54,5] <- 0
  return(hold.df)
}




### Make lists of random stimuli
all.stim <- list.files(stimDir, pattern = ".jpg$")
v.Stim <- sample(1:length(all.stim), 324)
v.Targ <- v.Stim[1:120]
v.Lure <- v.Stim[121:240]
v.Foil <- v.Stim[241:324]




### Build Blocks
# 6 Blocks of 20 Targ + 20 Lure + 14 Foil = 324 stimuli (120 Targ + 120 Lure + 84)
a<-1; aa<-20; b<-1; bb<-14
for(j in 1:6){
  
  # stim list
  hold.T <- all.stim[v.Targ[a:aa]]
  hold.L <- all.stim[v.Lure[a:aa]]
  hold.F <- all.stim[v.Foil[b:bb]]
  a<-a+20; aa<-aa+20; b<-b+14; bb<-bb+14
  
  # iterate till success
  status <- 0; while(status != 1){
    
    # construct stimulus list - this randomization is what makes or breaks the DF construction
    hold.input <- List.Function(hold.T,hold.L,hold.F)
    
    ## fill output file
    hold.output <- matrix(NA,nrow=94,ncol=6)
    hold.rand <- sample(1:dim(hold.output)[1],dim(hold.output)[1])
    
    for(i in 1:dim(hold.input)[1]){
      
      # find first NA, lag, positions 1 & 2
      hold.FNA <- as.numeric(min(which(is.na(hold.output[,1]))))
      
      hold.lag <- as.numeric(hold.input[i,5])
      hold.pos1 <- as.numeric(hold.rand[i])
      hold.pos2 <- hold.lag + hold.pos1
      
      # if lag
      if(hold.lag != 0){
        
        # if pos2 is in df
        if(hold.pos2 <= as.numeric(dim(hold.output)[1])){
          
          # if pos1 & pos2 are empty
          if(is.na(hold.output[hold.pos1,1])==T && is.na(hold.output[hold.pos2,1])==T){
            hold.output[hold.pos1,] <- c(hold.input[i,], "No")
            hold.output[hold.pos2,] <- c(hold.input[i,], "Yes")
            
            # find suitable empty rows  
          }else{
            func.out <- SeedNA.Function(hold.lag,hold.output)
            if(length(func.out) > 1){
              hold.output[as.numeric(func.out[1]),] <- c(hold.input[i,],"No")
              hold.output[as.numeric(func.out[2]),] <- c(hold.input[i,],"Yes")
            }
          }
          
          # find empty rows in df     
        }else{
          func.out <- SeedNA.Function(hold.lag,hold.output)
          if(length(func.out) > 1){
            hold.output[as.numeric(func.out[1]),] <- c(hold.input[i,],"No")
            hold.output[as.numeric(func.out[2]),] <- c(hold.input[i,],"Yes")
          }
        }
        
        # if no lag
      }else{
        
        # if pos1 is empty
        if(is.na(hold.output[hold.pos1,1])==T){
          hold.output[hold.pos1,] <- c(hold.input[i,],"No")
        }else{
          hold.output[hold.FNA,] <- c(hold.input[i,],"No")
        }
      }
    }
    
    ## Exit while-loop if DF is constructed successfully
    n.na<-0 
    for(i in 1:dim(hold.output)[1]){
      if(is.na(hold.output[i,6])==T){
        n.na <- n.na+1
      }
    }
    if(n.na == 0 ){
      status <- 1
    }
  }
  
  # write out
  trialNum <- 1:94
  blockTyp <- c(rep("Memory",94))
  hold.output <- cbind(trialNum,blockTyp,hold.output)
  
  fileName <- paste0(subj,"_B",j,"_stimuli.txt")
  colnames(hold.output) <- c("Trial","Block","File","Duration","Encoding","StimType","Lag","Repeat")
  write.table(hold.output,paste0(subjDir,"/",fileName), quote = F, row.names = F, sep = '\t', eol = "\r\n")
}
  


### Posner Paradigm
# 2 blocks, 80 trial/block
posDir <- paste0(workDir,"Stimuli/Posner_stimuli/")

posList <- sample(c(rep("CL",20), rep("CR",20), rep("IL",20), rep("IR",20)),80)
isiList <- sample(c(rep(200,40), rep(400,40)),80)
posStim <- paste0(posList,"_",isiList)

for(j in 1:2){
  
  pos.rand <- sample(1:length(posStim),length(posStim))
  
  pos.output <- matrix(NA,nrow=80,ncol=4)
  pos.output[,1] <- 1:80
  pos.output[,2] <- c(rep("Posner",80))
 
  for(i in 1:dim(pos.output)[1]){
    
    hold.input <- posStim[pos.rand[i]]
    pos.output[i,3] <- hold.input
    
    if(grepl("200",hold.input)==T){
      pos.output[i,3] <- 0.2
    }else if(grepl("400",hold.input)==T){
      pos.output[i,3] <- 0.4
    }
    
    if(grepl("IL",hold.input)==T){
      pos.output[i,4] <- "IConLeft"
    }else if(grepl("IR",hold.input)==T){
      pos.output[i,4] <- "IConRight"
    }else if(grepl("CL",hold.input)==T){
      pos.output[i,4] <- "ConLeft"
    }else if(grepl("CR",hold.input)==T){
      pos.output[i,4] <- "ConRight"
    }
  }
  
  fileName <- paste0(subj,"_P",j,"_stimuli.txt")
  colnames(pos.output) <- c("Trial","Block","Duration","StimType")
  write.table(pos.output,paste0(subjDir,"/",fileName), quote = F, row.names = F, sep = '\t', eol = "\r\n")
}
