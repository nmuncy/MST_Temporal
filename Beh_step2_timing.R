


##### Set up
parDir <- '/Volumes/Yorick/Temporal/Experiment3/'
workDir <- paste0(parDir,"Behavioral_Data/")
outDir <- paste0(parDir,"Analyses/behAnalysis/timing_files/")

subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
subjList <- t(subjList)

# I didn't have PsychoPy separate output according to training session or blocks
trainLength <- 12
numBlock <- 3



# j<- subjList[2]
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
  
  # remove training, if training happened - this is hard coded
  stop = FALSE
  if(length(hold_response)==294){
    
    for(a in hold_list){
      assign(paste0("hold_",a),(get(paste0("hold_",a))[-(1:trainLength)]))
    }
    ind_trial_start <- ind_trial_start[-(1:trainLength)]
    ind_trial_end <- ind_trial_end[-(1:trainLength)]

  }else if(length(hold_response)!=282){
    stop = TRUE
    stop('The number of trials was not 294 or 282')
  }
  if(stop){break}

  # get start/end times
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
  
  
  # ### Determine encoding time that precedes behavior
  # 
  # # read in block stim sets
  # for (i in 1:numBlock){
  #   
  #   hold_df_stim <- read.delim(paste0(subjDir,"/",j,"_B",i,"_stimuli.txt"), header=T, sep="\t")
  #   df_1st_present <- subset(hold_df_stim, hold_df_stim$Repeat == "No" & (hold_df_stim$StimType == "Targ" | hold_df_stim$StimType == "Lure"))
  #   
  #   # subset ind_beh by block
  #   ind_block <- ind_beh_Hit_All[ind_beh_Hit_All<94]
  
  #   # pull images of behavior for block
  #   hold_images <- hold_df_stim[ind_block,3]
  
  #   #match images to 1st presentation
  #   hold_match <- match(hold_image, df_1st_present[,3])
  # 
  # }
  # 
  
  
  
  ### Write Timing Files
  # Separate for each block by determining if index is within range,
  # Use indices to keep things straight
  # Make sure the beginning of each block has t=0.
  # Determine start:duration of each behavior coded above.
  
  beh_list <- gsub("ind_beh_","",ls(pattern="ind_beh"))
  
  # a <- beh_list[1]
  for(a in beh_list){
    
    # behavior indices
    ind_a <- get(paste0("ind_beh_",a))

    # separate by block
    # block <- 1
    for(block in 1:numBlock){
      
      # range, start of block
      LB <- (block*blockLength+1)-blockLength
      UB <- block*blockLength
      time_zero <- round(as.numeric(as.character(df.subj[ind_trial_start[LB],2])),1)
      
      # import block stim set, determine 1st presentations
      hold_df_stim <- read.delim(paste0(subjDir,"/",j,"_B",block,"_stimuli.txt"), header=T, sep="\t")
      df_1st_present <- subset(hold_df_stim, hold_df_stim$Repeat == "No" & (hold_df_stim$StimType == "Targ" | hold_df_stim$StimType == "Lure"))
      row.names(df_1st_present) <- 1:dim(df_1st_present)[1]
      
      # set output (which line of TF to write), for encoding (Response preceding) and behavior trials
      hold_block <- paste0("TF_line",block,"_",a)
      hold_block_precede <- paste0("TF_line",block,"_Rp",a)
      assign(hold_block,NA); assign(hold_block_precede,NA)
      
      ## Split behavior into blocks
      # The first-presentation info is read in from stimulus text file.
      # It is already broken into blocks, but the behaviors are not, so
      # these have to be matched up.
      
      # b <- ind_a[1]
      for(b in ind_a){
        if(b %in% seq(LB,UB)==T){
        
          # Determine the the position within block from "b" which is in total
          # experiment time. Extract the image string with which the behavior occured 
          # from stimulus file, then find the index and trial number of the first 
          # presentation of image.
          block_ind <- b-((block-1)*blockLength)
          hold_image <- as.character(hold_df_stim[block_ind,3])
          ind_match <- match(hold_image, df_1st_present[,3])
          trial_num <- df_1st_present[ind_match,1]
        
          # determine start, duration of behavior
          hold_start <- round((hold_resp_onset[b]-time_zero),1)
          hold_dur <- round(((hold_trial_end[b]-time_zero)-hold_start),1)
          TF_input <- paste0(hold_start,":",hold_dur)
          
          # determine start, duration of encoding (Response preceding behavior)
          total_trial_num <- trial_num+LB-1
          hold_start_enc <- round((hold_resp_onset[total_trial_num]-time_zero),1)
          hold_dur_enc <- round(((hold_trial_end[total_trial_num]-time_zero)-hold_start_enc),1)
          TF_input_enc <- paste0(hold_start_enc,":",hold_dur_enc)
        
          # write block timing
          assign(hold_block,c(get(hold_block),TF_input))
          assign(hold_block_precede,c(get(hold_block_precede),TF_input_enc))
        }
      }
      
      # remove NA
      assign(hold_block,get(hold_block)[-1])
      assign(hold_block_precede,get(hold_block_precede)[-1])
    }
    
    # fill empty lines with "*"
    for(e in 1:numBlock){
      
      hold_tf <- get(paste0("TF_line",e,"_",a))
      hold_tf_enc <- get(paste0("TF_line",e,"_Rp",a))
      
      if(length(hold_tf)==0){
        assign(paste0("TF_line",e,"_",a),"*")
      }
      
      if(length(hold_tf_enc)==0){
        assign(paste0("TF_line",e,"_Rp",a),"*")
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
      
      outFile1 <- paste0(outDir,j,"_TF_",out,".txt")
      cat(hold1, "\n", file=outFile1, append=F, sep='\t')
      cat(hold2, "\n", file=outFile1, append=T, sep='\t')
      cat(hold3, "\n", file=outFile1, append=T, sep='\t')
      
      hold4 <- get(paste0("TF_line1_Rp",out))
      hold5 <- get(paste0("TF_line2_Rp",out))
      hold6 <- get(paste0("TF_line3_Rp",out))
      
      outFile2 <- paste0(outDir,j,"_TF_Rp",out,".txt")
      cat(hold4, "\n", file=outFile2, append=F, sep='\t')
      cat(hold5, "\n", file=outFile2, append=T, sep='\t')
      cat(hold6, "\n", file=outFile2, append=T, sep='\t')
    }
  }
}
