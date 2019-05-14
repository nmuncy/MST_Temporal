



##### Set up
workDir <- '/Volumes/Yorick/Temporal/Experiment3/'
subjList <- read.delim(paste0(workDir,"Subj_List.txt"),header = F)
subjList <- t(subjList)


# j<-"p002"
for(j in subjList){
  
  # load data
  subjDir <- paste0(workDir,j,"/")
  df.subj <- read.csv(paste0(subjDir,j,"_timing_log.csv"))
  df.subj <- as.data.frame(df.subj)
  
  
  ### Determine positions, account for missing/multiple responses
  # Find start of each trial, pull info from known positions following trial start.
  # Multiple/no responses will have a different count following trial start, so
  # account for that to not get off count
  
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


  ### Make a vector for stimulus and behavior
  # one vector will hold stimulus info, another the corresponding
  # behavior. This will be used to build timing files down stream
  
  # vec_stimulus <- vec_response <- NA
  # for(i in 1:length(hold_stim_dur)){
  #   
  # }
  
  
  
}