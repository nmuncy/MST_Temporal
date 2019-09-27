library(tiff)
library(reshape2)
library(ez)



### --- Notes
#
# This script will run stats on ROI and MVM betas



###################
# Set up
###################

parDir <- "/Volumes/Yorick/Temporal/Experiment3/Analyses/"

doWrite <- 1
doGraphs <- 1


### ROI variables
roiDir <- paste0(parDir,"roiAnalysis/")
subDir <- paste0(roiDir,"sub_betas/")
hc_outDir <- paste0(roiDir,"sub_stats/")



### MVM variables
mvmDir <- paste0(parDir,"grpAnalysis/")
mvmData_location <- paste0(mvmDir,"mvm_betas/")
mvmData_list <- read.table(paste0(mvmData_location,"All_List.txt"))
mvm_outDir <- mvmData_location
mvm_statsDir <- paste0(mvmDir,"mvm_stats/")




###################
# Functions
###################
# GraphNames.Function <- function(dataString){
#   if(dataString=="SpT1"){return(list(n1="RpHit", n2="RpFA", n3="RpCR"))}
#   else if(dataString=="SpT1pT2"){return(list(n1="RpFpH", n2="RpFpF"))}
#   else if(dataString=="T1"){return(list(n1="Hit", n2="FA", n3="CR"))}
#   else if(dataString=="T1pT2"){return(list(n1="FpH", n2="FpF", n3="CpH"))}
#   else if(dataString=="T2"){return(list(n1="Hit", n2="FA"))}
#   # else if(dataString=="T2fT1"){return(list(n1="HfH", n2="HfF", n3="HfC"))}
#   else if(dataString=="T2fT1"){return(list(n1="HfF", n2="FfF", n3="HfC"))}
# }

GraphMvmNames.Function <- function(dataString1,dataString2){
  if(dataString1=="Encoding"){
    if(dataString2=="lErc"){
      return(list(n1="RpHit", n2="RpMiss"))
    }else if(dataString2=="rHip"){
      return(list(n1="RpCR", n2="RpFA"))
    }
  }else if(dataString1=="Response"){
    if(dataString2=="rErc"){
      return(list(n1="CR", n2="Hit"))
    }
  }
}

BehNames.Function <- function(dataString){
  if(grepl("Encoding",dataString)==T){out<-c("EpCR","EpFA","EpHit","EpMiss"); return(out)}
  else if(grepl("Response",dataString)==T){out<-c("CR","FA","Hit","Miss"); return(out)}
}

MvmNames.Function <- function(x,y){
  if(grepl("Encoding",x)==T){
    if(y=="lErc"){
      return("L. Entorhinal Cortex")
    }else if(y=="rHip"){
      return("R. Hippocampus")
    }
  }else if(grepl("Response",x)==T){
    if(y=="rErc"){
      return("R. Entorhinal Cortex")
    }
  }
}

SE.Function <- function(x,plot_data){
  SD <- sd(plot_data[,x])/sqrt(length(plot_data[,x]))
  return(SD)
}

Graph.Function <- function(DF,output_name,maskN,out_place){
  
  TITLE <- maskN
  MEANS <- colMeans(DF)
  E.BARS<-NA
  for(a in 1:dim(DF)[2]){
    E.BARS[a] <- SE.Function(a,DF)
  }
  RANGE <- range(c(MEANS,MEANS-E.BARS,MEANS+E.BARS,0))
  
  if(grepl("mvm_stats",out_place)==T){
    ROI <- MvmNames.Function(output_name,maskN)
    XNAMES <- GraphMvmNames.Function(output_name,maskN)
  }#else if(grepl("sub_stats",out_place)==T){
  #   ROI <- maskN
  #   XNAMES <- GraphNames.Function(output_name)
  # }
  
  plotable <- matrix(0,nrow=2,ncol=num.betas)
  plotable[1,] <- MEANS
  plotable[2,] <- E.BARS
  
  if(doWrite == 1){
    graphOut <- paste0(out_place,"Graph_",output_name,"_",TITLE,".tiff")
    bitmap(graphOut, width = 6.5, units = 'in', type="tiff24nc", res=1200)
  }
  barCenters <- barplot(plotable[1,], names.arg = c(XNAMES), main=ROI, ylab="Beta Coefficient",ylim=RANGE)
  segments(barCenters, MEANS-E.BARS, barCenters, MEANS+E.BARS)
  arrows(barCenters, MEANS-E.BARS, barCenters, MEANS+E.BARS, lwd = 1, angle = 90, code = 3, length = 0.05)
  set.pos <- rowMeans(plotable); if(set.pos[1]>0){POS<-3}else{POS<-1}
  text(barCenters,0,round(plotable[1,],4),cex=1,pos=POS,font=2)
  if(doWrite == 1){
    dev.off()
  }
}

TT.Function <- function(x, y, dataN, maskN, out_place){
  ttest_out <- t.test(x,y,paired=T)
  meanX <- mean(x)
  meanY <- mean(y)
  if(doWrite == 1){
    output <- c(meanX, meanY)
    output <- c(output, capture.output(print(ttest_out)))
    writeLines(output,paste0(out_place,"Stats_TT_",dataN,"_",maskN,".txt"))
    return(ttest_out)
  }else{
    print(ttest_out)
  }
}

LWC.Function <- function(w,x,y,z){
  
  list.A <- w
  list.B <- x
  N <- as.numeric(y)
  data.raw <- z
  
  n.A <- as.numeric(length(list.A))
  n.B <- as.numeric(length(list.B))
  range1<-n.B*N 
  
  df.long <- as.data.frame(matrix(NA,nrow=N*n.A*n.B,ncol=5))
  names(df.long) <- c("Mask","Hemisphere","Behavior","Subj","Value")
  
  # col 1 = Mask
  a<-1; aa<-range1
  for(b in list.A){
    df.long[a:aa,1] <- b
    a<-aa+1; aa<-aa+range1
  }
  
  # col 2 = Hemisphere

  # if(grepl("_",list.A[1])==TRUE){
  #   a<-1; aa<-range1
  #   for(b in list.A){
  #     df.long[a:aa,2] <- gsub("_.*","",b)
  #     gsub("_.*$","",b)
  #     a<-aa+1; aa<-aa+range1
  #   }
  # }else{
  #   df.long[,2] <- "NA"
  # }
  a<-1; aa<-range1
  for(b in list.A){
    if(grepl("L",b)==T){
      df.long[a:aa,2] <- "L"
    }else{
      df.long[a:aa,2] <- "R"
    }
    a<-aa+1; aa<-aa+range1
  }
  
  # col 3 = Behavior
  a<-1; aa<-N
  while(aa <= dim(df.long)[1]){
    for(b in 1:length(list.B)){
      df.long[a:aa,3] <- rep(list.B[b],N)
      a<-aa+1; aa<-aa+N
    }
  }

  # col 4 = subj
  a<-1; aa<-N
  while(aa <= dim(df.long)[1]){
    df.long[a:aa,4] <- 1:N
    a<-aa+1; aa<-aa+N
  }
  
  # col 5 = data
  data.hold <- melt(data.raw)
  df.long[,5] <- data.hold[,3]
  
  #write
  return(df.long)
}

Mdata.Function <- function(x){
  
  #masks
  DF <- x
  ind.mask <- grep("Mask", DF[,1])
  assign("num.mask", length(ind.mask), envir = .GlobalEnv)
  # num.mask <- length(ind.mask)
  
  #subjects
  ind.subj <- grep("File", DF[,1])
  len.subj <- length(ind.subj)
  assign("num.subj", len.subj/num.mask, envir = .GlobalEnv)
  # num.subj <- len.subj/num.mask
  
  #betas
  ind.betas <- grep("+tlrc", DF[,1])
  len.betas <- length(ind.betas)
  assign("num.betas", (len.betas/num.mask)/num.subj, envir = .GlobalEnv)
  # num.betas <- (len.betas/num.mask)/num.subj
  
  # organize data
  ind.data <- matrix(as.numeric(as.character(DF[grep("+tlrc",DF[,1]),3])),ncol=num.betas,byrow=T)
  df.hold <- matrix(0,nrow=num.subj, ncol=num.mask*num.betas)
  for(i in 1:num.mask){
    df.hold[,(num.betas*i-(num.betas-1)):(num.betas*i)] <- ind.data[(num.subj*i-(num.subj-1)):(num.subj*i),1:num.betas]
  }
  colnames(df.hold) <- c(as.character(rep(DF[ind.mask,1],each=num.betas)))
  h.mask.names <- sub("Mask ","",DF[ind.mask,1])
  assign("mask.names",h.mask.names, envir = .GlobalEnv)
  
  return(df.hold)
}

MkTable.Function <- function(x,y){
  
  DF <- x
  DF.perm <- y
  hold.post <- matrix(NA,nrow=1,ncol=1+(num.comp*6))
  hold.post[1,1] <- hold.mask
  
  d<-2; for(k in 1:dim(DF.perm)[1]){
    
    colA <- DF.perm[k,1]; colB <- DF.perm[k,2]
    t.hold <- TT.Function(DF[,colA],DF[,colB],comp,paste0(hold.mask,"_",beh[colA],"-",beh[colB]),ns_outDir)

    t.hold.comp <- paste0(beh[colA],"-",beh[colB])
    t.hold.t <- as.numeric(t.hold$statistic)
    t.hold.df <- as.numeric(t.hold$parameter)
    t.hold.p <- as.numeric(t.hold$p.value)
    t.hold.lb <- as.numeric(t.hold$conf.int[1])
    t.hold.ub <- as.numeric(t.hold$conf.int[2])

    t.hold.capture <- c(t.hold.comp,t.hold.t,t.hold.df,t.hold.p,t.hold.lb,t.hold.ub)
    dd <- d+5
    hold.post[1,d:dd]<-t.hold.capture
    d <- dd+1
  }
  return(hold.post)
}




###################
# HC Sub Analysis
###################
# # For testing
# j <- "Betas_Response_Head_sub.txt"

HCmaster_list <- read.table(paste0(subDir,"Master_list.txt"))

df.p <- matrix(NA,nrow=dim(HCmaster_list)[1],ncol=1)
colnames(df.p) <- "orig"
rownames(df.p) <- HCmaster_list[,1]

df.pgg <- matrix(NA,nrow = dim(HCmaster_list)[1],ncol=1)
colnames(df.pgg) <- "orig"
rownames(df.pgg) <- HCmaster_list[,1]

count<-1; for(j in t(HCmaster_list)){
  
  # get info
  hold <- read.delim(paste0(subDir,j),header=F)
  Mdata <- Mdata.Function(hold)

  hold <- gsub("^.*?_","",j)
  comp <- gsub("_sub.*$","",hold)
  beh <- BehNames.Function(comp)

  ### Convert to Long, run ANOVA  
  Mdata_long <- LWC.Function(mask.names,beh,num.subj,Mdata)
  stats <- ezANOVA(Mdata_long,dv=Value,wid=Subj,within=c(Mask,Behavior),type='III')
  
  # save p-intx
  df.p[count,1] <- print(stats$ANOVA$p[3])
  df.pgg[count,1] <- print(stats$`Sphericity Corrections`$`p[GG]`[3])
  
  if(doWrite == 1){
    output <- capture.output(stats)
    writeLines(output,paste0(hc_outDir,"Stats_Sub_AN_",comp,".txt"))
  }
  
  if(doGraphs == 1){
    if(is.na(df.pgg[count,1])==F && df.pgg[count,1] < 0.05){
      c<-1; while(c < dim(Mdata)[2]){
        cc<-c+(num.betas-1)
        hold.df <- matrix(NA,nrow=dim(Mdata)[1],ncol=num.betas)
        hold.df[,1:num.betas] <- Mdata[,c:cc]
        hold <- colnames(Mdata)[c]; hold.mask <- substring(hold,6)
        Graph.Function(hold.df,comp,hold.mask,hc_outDir)
        c<-c+num.betas
      }
    }
  }
  count <- count+1
}



###################
# MVM
###################
# # For testing
# i <- "All_Betas_Encoding.txt"
# j <- "Betas_Encoding_lErc.txt"

for(i in t(mvmData_list)){
  
  beta_list <- read.table(paste0(mvmData_location,i))
  
  df.post <- matrix(NA,nrow = dim(beta_list)[1],ncol = 6)
  colnames(df.post) <- c("ROI","T","df","p","LB","RB")
  
  count<-1
  for(j in t(beta_list)){
    
    ### Get, clean data
    raw_data <- read.delim2(paste0(mvmData_location,j),header=F)
    
    # num subjects
    ind.subj <- grep("File", raw_data[,1])
    num.subj <- as.numeric(length(ind.subj))
    
    # num betas
    ind.beta <- grep("beh", raw_data[,2])
    num.betas <- as.numeric(length(ind.beta)/num.subj)
    
    ind.beh1 <- ind.beta[c(TRUE, FALSE)]
    ind.beh2 <- ind.beta[c(FALSE, TRUE)]
    
    # fill df
    df <- matrix(0,ncol=num.betas,nrow=num.subj)
    df[,1] <- as.numeric(as.character(raw_data[ind.beh1,3]))
    df[,2] <- as.numeric(as.character(raw_data[ind.beh2,3]))
    
    # write out
    hold<-gsub("^.*_","",i); comp<-gsub("\\..*","",hold)
    hold1<-gsub("^.*_","",j); anat<-gsub("\\..*","",hold1)
    
    if(doWrite == 1){
      write.table(df,paste0(mvmData_location,"Avg_Betas_",comp,"_",anat,".txt"),col.names=F, row.names=F,sep = "\t")
    }
    
    
    ### Stats
    if(num.betas == 2){
      t.out <- TT.Function(df[,1],df[,2],comp,anat,mvm_statsDir)
      
      hold.t <- round(t.out$statistic,digits=2)
      hold.df <- t.out$parameter
      hold.p <- round(t.out$p.value,digits=6)
      hold.ci <- round(t.out$conf.int,digits=3)
      hold.write <- c(anat,hold.t,hold.df,hold.p,hold.ci)
      df.post[count,] <- hold.write
    }

    
    ### Graphs
    if(doGraphs == 1){
      Graph.Function(df,comp,anat,mvm_statsDir)
    }
    count <- count+1
  }
  
  if(doWrite==1){
    write.table(df.post,paste0(mvm_statsDir,"Table_",comp,".txt"),row.names = F, quote = F, sep = "\t")
  }
}

