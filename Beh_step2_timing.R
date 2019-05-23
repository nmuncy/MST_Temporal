



##### Set up
parDir <- '/Volumes/Yorick/Temporal/Experiment3/'
workDir <- paste0(parDir,"Behavioral_Data/")
outDir <- paste0(parDir,"Analyses/behAnalysis/timing_files/")

subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
subjList <- t(subjList)

# I didn't have PsychoPy separate out put according to training session or blocks
trainLength <- 12
numBlock <- 3



# j<- subjList[1]
for(j in subjList){
  
  # load data
  subjDir <- paste0(workDir,j,"/")
  df.subj <- read.csv(paste0(subjDir,j,"_timing_log.csv"))
  df.subj <- as.data.frame(df.subj)
  
  
  ### Determine positions, account for missing/multiple responses
  # Find start of each trial, pull info from known positions following trial start.
  # Multiple/no responses will have a different count following trial start, so
  # account for that to not get off count.
  # Clean up data, remove training session, determine length of e/block.
  
  # set variables
  hold_list <- c("stim_onset","stat_onset","stim_dur","stim_typ","stim_lag","stim_rep","resp_onset","response")
  for(a in hold_list){
    assign(paste0("hold_",a),NA)
  }
  
  # determine start/end positions
  ind_trial_start <- grep("Trial Start",df.subj[,1])
  ind_trial_end <- grep("Trial End",df.subj[,1])
  
  # fill variables from known positions
  for(i in 1:length(ind_trial_start)){
    
    hold_stim_onset <- c(hold_stim_onset,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+1),2])),1))
    hold_stat_onset <- c(hold_stat_onset,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+2),2])),1))
    hold_stim_dur <- c(hold_stim_dur,as.character(df.subj[(ind_trial_start[i]+3),2]))
    hold_stim_typ <- c(hold_stim_typ,as.character(df.subj[(ind_trial_start[i]+4),2]))
    hold_stim_lag <- c(hold_stim_lag,as.character(df.subj[(ind_trial_start[i]+5),2]))
    hold_stim_rep <- c(hold_stim_rep,as.character(df.subj[(ind_trial_start[i]+6),2]))
    hold_resp_onset <- c(hold_resp_onset,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+7),2])),1))

    # determine single/multiple/no response, fill accordingly (account for last trial)
    hold1 <- ind_trial_start[i]+9; hold2 <- ind_trial_start[i]+11
    if(grepl("Response",df.subj[hold1,1])==T && (grepl("Trial Start",df.subj[hold2,1])==T || hold2 >= dim(df.subj)[1])){
      hold_response <- c(hold_response,as.character(df.subj[hold1,2]))
    }else{
      hold_response <- c(hold_response,999)
    }
  }
  
  # remove NA
  for(a in hold_list){
    assign(paste0("hold_",a),(get(paste0("hold_",a))[-1]))
  }
  
  # remove spaces
  for(b in 1:length(hold_stim_dur)){
    hold_response[b] <- gsub(" ","",hold_response[b],fixed=T)
    hold_stim_dur[b] <- gsub(" ","",hold_stim_dur[b],fixed=T)
    hold_stim_lag[b] <- gsub(" ","",hold_stim_lag[b],fixed=T)
    hold_stim_rep[b] <- gsub(" ","",hold_stim_rep[b],fixed=T)
    hold_stim_typ[b] <- gsub(" ","",hold_stim_typ[b],fixed=T)
  }
  
  # check
  for(a in hold_list){
    h.test <- length(get(paste0("hold_",a)))
    stopifnot(h.test == length(ind_trial_start))
  }
  
  # remove training
  for(a in hold_list){
    assign(paste0("hold_",a),(get(paste0("hold_",a))[-(1:trainLength)]))
  }
  
  # get start/end times
  ind_trial_start <- ind_trial_start[-(1:trainLength)]
  ind_trial_end <- ind_trial_end[-(1:trainLength)]
  hold_trial_start <- round(as.numeric(as.character(df.subj[ind_trial_start,2])),1)
  hold_trial_end <- round(as.numeric(as.character(df.subj[ind_trial_end,2])),1)
  
  # block length
  blockLength <- length(ind_trial_start)/numBlock
  

  ### Behaviors 
  # This is hardcoded to avoid a conditional nightmare.
  # Have the variable name be called ind_beh_Foo.
  # T1=1.5s, T2=1s
  # Combine behaviors since some bins are very small
  
  # Targets
  # ind_beh_Miss_Long_T1 <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & hold_stim_dur=="T1" & hold_response==1)
  # ind_beh_Miss_Long_T2 <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & hold_stim_dur=="T2" & hold_response==1)
  # ind_beh_Hit_Same_T1 <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & hold_stim_dur=="T1" & hold_response==2)
  # ind_beh_Hit_Same_T2 <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & hold_stim_dur=="T2" & hold_response==2)
  # ind_beh_Miss_Short_T1 <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & hold_stim_dur=="T1" & hold_response==3)
  # ind_beh_Miss_Short_T2 <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & hold_stim_dur=="T2" & hold_response==3)

  ind_beh_Miss_All <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & (hold_stim_dur=="T1" | hold_stim_dur=="T2") & (hold_response==1 | hold_response==3))
  ind_beh_Hit_All <- which(hold_stim_typ=="Targ" & hold_stim_rep=="Yes" & (hold_stim_dur=="T1" | hold_stim_dur=="T2") & hold_response==2)

  # Lures
  ind_beh_FA_Long_L1 <- which(hold_stim_typ=="Lure" & hold_stim_rep=="Yes" & hold_stim_dur=="T1" & hold_response==1)
  ind_beh_CR_Long_L2 <- which(hold_stim_typ=="Lure" & hold_stim_rep=="Yes" & hold_stim_dur=="T2" & hold_response==1)
  ind_beh_FA_Same_L1 <- which(hold_stim_typ=="Lure" & hold_stim_rep=="Yes" & hold_stim_dur=="T1" & hold_response==2)
  ind_beh_FA_Same_L2 <- which(hold_stim_typ=="Lure" & hold_stim_rep=="Yes" & hold_stim_dur=="T2" & hold_response==2)
  ind_beh_CR_Short_L1 <- which(hold_stim_typ=="Lure" & hold_stim_rep=="Yes" & hold_stim_dur=="T1" & hold_response==3)
  ind_beh_FA_Short_L2 <- which(hold_stim_typ=="Lure" & hold_stim_rep=="Yes" & hold_stim_dur=="T2" & hold_response==3)
  
  ind_beh_FA_All <- sort(c(ind_beh_FA_Long_L1,ind_beh_FA_Same_L1,ind_beh_FA_Same_L2,ind_beh_FA_Short_L2))
  ind_beh_CR_All <- sort(c(ind_beh_CR_Long_L2,ind_beh_CR_Short_L1))
  
  
  ### Write Timing Files
  # Separate for each block by determining if index is within range,
  # use indices to keep things straight
  # Make sure the beginning of each block has t=0.
  # Determine start:duration of each behavior coded above.
  
  beh_list <- gsub("ind_beh_","",ls(pattern="ind_beh"))
  
  for(a in beh_list){
    
    # behavior index
    ind_a <- get(paste0("ind_beh_",a))

    # separate by block
    for(block in 1:numBlock){
      
      # range, start of block
      LB <- (block*blockLength+1)-blockLength
      UB <- block*blockLength
      time_zero <- round(as.numeric(as.character(df.subj[ind_trial_start[LB],2])),1)
      
      # set output
      hold_block <- paste0("TF_line",block,"_",a)
      assign(hold_block,NA)
      
      for(b in ind_a){
        
        # start, duration
        hold_start <- round((hold_resp_onset[b]-time_zero),1)
        hold_dur <- round(((hold_trial_end[b]-time_zero)-hold_start),1)
        TF_input <- paste0(hold_start,":",hold_dur)
        
        # write block timing
        if(b %in% seq(LB,UB)==T){
          assign(hold_block,c(get(hold_block),TF_input))
        }
      }
      
      # remove NA
      assign(hold_block,get(hold_block)[-1])
    }
    
    # fill empty variables with "*"
    for(e in 1:numBlock){
      hold_tf <- get(paste0("TF_line",e,"_",a))
      if(length(hold_tf)==0){
        assign(paste0("TF_line",e,"_",a),"*")
      }
    }
  }
  
  
  ### Write Timing Files
  # Only write out "All" TFs
  
  for (out in beh_list) {
    if(grepl("All",out)==T){
      
      hold1 <- get(paste0("TF_line1_",out))
      hold2 <- get(paste0("TF_line2_",out))
      hold3 <- get(paste0("TF_line3_",out))
      
      outFile <- paste0(outDir,j,"_TF_",out,".txt")
      cat(hold1, "\n", file=outFile, append=F, sep='\t')
      cat(hold2, "\n", file=outFile, append=T, sep='\t')
      cat(hold3, "\n", file=outFile, append=T, sep='\t')
    }
  }
}
