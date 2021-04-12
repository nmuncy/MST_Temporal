


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
#
# 4) New as of 9/21/18
#   - Removing Posner Task
#   - Cutting number of Temporal blocks in half
#
# 5) Adding a practice block (10/17/18)



### Set variables
workDir <- "/Volumes/Yorick/Temporal/Experiment3/"
stimDir <- "/Volumes/Yorick/Temporal/Stimuli/StimSet/"




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
v.Targ <- v.Stim[1:60]
v.Lure <- v.Stim[61:120]
v.Foil <- v.Stim[121:162]
v.Train <- v.Stim[163:172]



### Build Blocks
# 3 Blocks of 20 Targ + 20 Lure + 14 Foil = 162 stimuli (60 Targ + 60 Lure + 42 Foil)
a<-1; aa<-20; b<-1; bb<-14
for(j in 1:3){
  
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
  


### Create short practice block manually
hold.df <- matrix(NA,nrow=10,ncol=2)
hold.df[,1] <- sample(c(rep("T1",5), rep("T2",5)), 10)
hold.df[,2] <- sample(c(rep("E1",3), rep("E2",3), rep("E3",4)), 10)

stim.train <- all.stim[v.Train[1:10]]
hold.train <- matrix(NA,nrow=12,ncol=8)
hold.train[,1] <- 1:12
hold.train[,2] <- "Memory"

hold.train[1:7,3] <- stim.train[1:7]
hold.train[8,3] <- stim.train[4]
hold.train[9:11,3] <- stim.train[8:10]
hold.train[12,3] <- stim.train[1]

hold.train[1:7,4] <- hold.df[1:7,1]
hold.train[8,4] <- hold.df[4,1]
hold.train[9:11,4] <- hold.df[8:10,1]
hold.train[12,4] <- hold.df[1,1]

hold.train[1:7,5] <- hold.df[1:7,2]
hold.train[8,5] <- hold.df[4,2]
hold.train[9:11,5] <- hold.df[8:10,2]
hold.train[12,5] <- hold.df[1,2]

hold.train[1:12,6] <- "Foil"
hold.train[1,6] <- "Targ"
hold.train[4,6] <- "Lure"
hold.train[8,6] <- "Lure"
hold.train[12,6] <- "Targ"

hold.train[1:12,7] <- 0
hold.train[1,7] <- 12
hold.train[4,7] <- 4
hold.train[8,7] <- 4
hold.train[12,7] <- 12

hold.train[1:12,8] <- "No"
hold.train[8,8] <- "Yes"
hold.train[12,8] <- "Yes"

fileName <- paste0(subj,"_Train_stimuli.txt")
colnames(hold.train) <- c("Trial","Block","File","Duration","Encoding","StimType","Lag","Repeat")
write.table(hold.train,paste0(subjDir,"/",fileName), quote = F, row.names = F, sep = '\t', eol = "\r\n")