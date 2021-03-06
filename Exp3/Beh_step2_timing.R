


### Set up
# Orienting variables and paths for the location of the raw
# data and output dir.
# References output made by Beh_step0_mkList.sh

parDir <- '/Volumes/Yorick/Temporal/'
workDir <- paste0(parDir,"Experiment3/Behavioral_Data/")
outDir <- paste0(parDir,"Analyses/Exp3/behAnalysis/timing_files/")

subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
subjList <- t(subjList)

# Hardcoded - forgot to have psychopy write the length of 
# training session and block numbers
trainLength <- 12
numBlock <- 3




### Get data for e/subject
for(j in subjList){
  
  # load data
  subjDir <- paste0(workDir,j,"/")
  df.subj <- read.csv(paste0(subjDir,j,"_timing_log.csv"))
  df.subj <- as.data.frame(df.subj)

  # determine start/end positions
  ind_trial_start <- grep("Trial Start",df.subj[,1])
  ind_trial_end <- grep("Trial End",df.subj[,1])
  
  # get desired variables ready
  hold_list <- c("stim_onset","stat_onset","stim_dur","stim_typ","stim_lag","stim_rep","resp_onset","response","resp_time")
  for(a in hold_list){
    assign(paste0("hold_",a),NA)
  }
  
  
  ## Fill variables from known positions.
  # This is based on the order of the psychopy output,
  # referencing the start position for each trial
  for(i in 1:length(ind_trial_start)){
    
    hold_stim_onset <- c(hold_stim_onset,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+1),2])),1))
    hold_stat_onset <- c(hold_stat_onset,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+2),2])),1))
    hold_stim_dur <- c(hold_stim_dur,as.character(df.subj[(ind_trial_start[i]+3),2]))
    hold_stim_typ <- c(hold_stim_typ,as.character(df.subj[(ind_trial_start[i]+4),2]))
    hold_stim_lag <- c(hold_stim_lag,as.character(df.subj[(ind_trial_start[i]+5),2]))
    hold_stim_rep <- c(hold_stim_rep,as.character(df.subj[(ind_trial_start[i]+6),2]))
    hold_resp_onset <- c(hold_resp_onset,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+7),2])),1))
    hold_resp_time <- c(hold_resp_time,round(as.numeric(as.character(df.subj[(ind_trial_start[i]+8),2])),1))

    
    ## Determine single/multiple/no response, fill accordingly, & account for last trial
    # Only 11 lines should be written for each trial if participant responded once
    hold1 <- ind_trial_start[i]+9; hold2 <- ind_trial_start[i]+11
    if(grepl("Response",df.subj[hold1,1])==T && (grepl("Trial Start",df.subj[hold2,1])==T || hold2 >= dim(df.subj)[1])){
      hold_response <- c(hold_response,as.character(df.subj[hold1,2]))
    }else{
      hold_response <- c(hold_response,999)
    }
  }
  
  ## clean up
  # remove first NAs
  for(a in hold_list){
    assign(paste0("hold_",a),(get(paste0("hold_",a))[-1]))
  }
  
  # remove spaces from some variables
  for(b in 1:length(hold_stim_dur)){
    hold_response[b] <- gsub(" ","",hold_response[b],fixed=T)
    hold_stim_dur[b] <- gsub(" ","",hold_stim_dur[b],fixed=T)
    hold_stim_lag[b] <- gsub(" ","",hold_stim_lag[b],fixed=T)
    hold_stim_rep[b] <- gsub(" ","",hold_stim_rep[b],fixed=T)
    hold_stim_typ[b] <- gsub(" ","",hold_stim_typ[b],fixed=T)
  }

  
  ## Remove training responses if training happened 
  # Experiment length with (294) and without (282) training is hardcoded
  # Break of wrong number of trials
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

  # get start/end times - hold_trial_end was used in duration calc but now hold_resp_time is
  hold_trial_start <- round(as.numeric(as.character(df.subj[ind_trial_start,2])),1)
  hold_trial_end <- round(as.numeric(as.character(df.subj[ind_trial_end,2])),1)
  
  # block length
  blockLength <- length(ind_trial_start)/numBlock
  

  
  
  ### Behaviors 
  # This is hardcoded to avoid a conditional nightmare - hooray for "which"!
  # Name variables ind_beh_Foo for later catch
  # T1=1.5s, T2=1s
  # Combine behaviors (ind_beh_Foo_ALL) since some bins are very small
  
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
  
  # Foils, Non/multiple responses
  ind_beh_Foil_All <- which(hold_stim_typ=="Foil" & hold_response!=999)
  ind_beh_NR_All <- which(hold_response==999)
  
  
  
  
  ### Determine start:dur for each behavior
  # Separate for each block by determining if index is within range
  # Use indices to keep trials straight
  # Make sure the beginning of each block has t=0

  # get behavior variables
  beh_list <- gsub("ind_beh_","",ls(pattern="ind_beh"))
  for(a in beh_list){
    
    # behavior positions (index)
    ind_a <- get(paste0("ind_beh_",a))
    
    # set model_status to avoid NR/Foils
    model_status = TRUE
    if(a == "NR_All" | a == "Foil_All"){
      model_status = FALSE
    }

    # separate by block
    for(block in 1:numBlock){
      
      # range, start of block
      LB <- (block*blockLength+1)-blockLength
      UB <- block*blockLength
      time_zero <- round(as.numeric(as.character(df.subj[ind_trial_start[LB],2])),1)
      
      # pull raw block stim set, determine 1st presentations. subset command references df colnames
      hold_df_stim <- read.delim(paste0(subjDir,"/",j,"_B",block,"_stimuli.txt"), header=T, sep="\t")
      df_1st_present <- subset(hold_df_stim, hold_df_stim$Repeat == "No" & (hold_df_stim$StimType == "Targ" | hold_df_stim$StimType == "Lure"))
      row.names(df_1st_present) <- 1:dim(df_1st_present)[1]
        
      # set output (which line of TF to write), for encoding (Response preceding) and behavior trials. No NRs.
      hold_block <- paste0("TF_line",block,"_",a); assign(hold_block,NA); 
      if(model_status){
        hold_block_precede <- paste0("TF_line",block,"_Rp",a)
        assign(hold_block_precede,NA)
      }

      
      ## Split behavior into blocks
      # The first-presentation info was read in from stimulus text file.
      # It is already broken into blocks, but the behaviors are not so
      # these have to be matched up.
      for(b in ind_a){
        if(b %in% seq(LB,UB)==T){
        
          # Determine the position within block from "b", which is in total
          # experiment time. Extract the image name with which behavior occured, 
          # then find the index and trial number of the first presentation of image.
          block_ind <- b-((block-1)*blockLength)
          hold_image <- as.character(hold_df_stim[block_ind,3])
          ind_match <- match(hold_image, df_1st_present[,3])
          trial_num <- df_1st_present[ind_match,1]
        
          # determine start, duration of behavior (dur = start to decision, not entire available response time)
          # account for NR response issues
          hold_start <- round((hold_resp_onset[b]-time_zero),1)
          if(model_status){
            hold_dur <- round(((hold_resp_time[b]-time_zero)-hold_start),1)
          }else{
            hold_dur <- round(((hold_trial_end[b]-time_zero)-hold_start),1)
          }
          TF_input <- paste0(hold_start,":",hold_dur)
          assign(hold_block,c(get(hold_block),TF_input))
          
          # Determine start, duration of encoding (Response preceding behavior). No NRs
          # Model stimulus presentation, not response, of 1st presentation for encoding
          if(model_status){
            total_trial_num <- trial_num+LB-1
            hold_start_enc <- round((hold_stim_onset[total_trial_num]-time_zero),1)
            hold_dur_enc <- round(((hold_stat_onset[total_trial_num]-time_zero)-hold_start_enc),1)
            TF_input_enc <- paste0(hold_start_enc,":",hold_dur_enc)
            assign(hold_block_precede,c(get(hold_block_precede),TF_input_enc))
          }
        }
      }
      
      # remove NAs. No NRs for encoding
      assign(hold_block,get(hold_block)[-1])
      
      if(model_status){
        assign(hold_block_precede,get(hold_block_precede)[-1])
      }
    }
    
    # fill empty lines with "*". No NRs for encoding
    for(e in 1:numBlock){
      
      hold_tf <- get(paste0("TF_line",e,"_",a))
      if(length(hold_tf)==0){
        assign(paste0("TF_line",e,"_",a),"*")
      }

      if(model_status){
        hold_tf_enc <- get(paste0("TF_line",e,"_Rp",a))      
        if(length(hold_tf_enc)==0){
          assign(paste0("TF_line",e,"_Rp",a),"*")
        }
      }
    }
  }
  
  
  
  
  ### Write Timing Files
  # Only write out "All" TFs due to small bins
  # Write one line per fMRI run/block

  for (out in beh_list) {
    if(grepl("All",out)==T){
      
      # behavior TFs
      hold1 <- get(paste0("TF_line1_",out))
      hold2 <- get(paste0("TF_line2_",out))
      hold3 <- get(paste0("TF_line3_",out))
      
      outFile1 <- paste0(outDir,j,"_TF_",out,".txt")
      cat(hold1, "\n", file=outFile1, append=F, sep='\t')
      cat(hold2, "\n", file=outFile1, append=T, sep='\t')
      cat(hold3, "\n", file=outFile1, append=T, sep='\t')
      
      # encoding preceding behavior TFs. No NRs.
      if(out != "NR_All" & out != "Foil_All"){
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
}
