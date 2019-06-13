### meaningful 
## hit a target and cr on a lure

pHit1.5 <- length(which(df.mem$Type==" Targ" & df.mem$Repeat==" Yes" & df.mem$Response==" 2" & df.mem$Duration==" T1"))/length(which(df.mem$Type==" Targ" & df.mem$Repeat==" Yes"& df.mem$Duration==" T1"))

pHit1 <- length(which(df.mem$Type==" Targ" & df.mem$Repeat==" Yes" & df.mem$Response==" 2" & df.mem$Duration==" T2"))/length(which(df.mem$Type==" Targ" & df.mem$Repeat==" Yes"& df.mem$Duration==" T2"))


#### 0

pHit1 - length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & df.mem$Response==" 2" & df.mem$Duration==" T1"))/length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes"& df.mem$Duration==" T1"))


length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & df.mem$Response==" 2" ))/length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes"))


pHit1.5 - length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & df.mem$Response==" 2" & df.mem$Duration==" T2"))/length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes"& df.mem$Duration==" T2"))


length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & df.mem$Response==" 2"))/length(which(df.mem$Type==" Lure" & df.mem$Repeat==" Yes"))


length(which(df.mem$Repeat==" Yes" & df.mem$Response==" 2"))/length(which(df.mem$Repeat==" Yes"))

length(which(df.mem$Repeat==" Yes" & df.mem$Response==" 3"))/length(which(df.mem$Repeat==" Yes"))

length(which(df.mem$Repeat==" Yes" & df.mem$Response==" 1"))/length(which(df.mem$Repeat==" Yes"))





sum(df.mem$Response==" 2")/294
sum(df.mem$Response==" 1")/294




#############
cr1 <- length(which(df.mem$Duration==" T1" & df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & df.mem$Response==' 3'))/length(which(df.mem$Duration==" T1" & df.mem$Type==" Lure" & df.mem$Repeat==" Yes" ))

cr1 - sum(df.mem$Duration==" T2" & df.mem$Repeat==" Yes" & df.mem$Type==" Targ" & df.mem$Response==" 3")/sum(df.mem$Duration==" T2" & df.mem$Repeat==" Yes" & df.mem$Type==" Targ")


cr1.5 <- sum(df.mem$Duration==" T2" & df.mem$Repeat==" Yes" & df.mem$Type==" Lure" & df.mem$Response==" 1")/sum(df.mem$Duration==" T2" & df.mem$Repeat==" Yes" & df.mem$Type==" Lure")


cr1.5 - sum(df.mem$Duration==" T1" & df.mem$Repeat==" Yes" & df.mem$Type==" Targ" & df.mem$Response==" 1")/sum(df.mem$Duration==" T2" & df.mem$Repeat==" Yes" & df.mem$Type==" Targ")


### adjusted d prime

pHit1.5 <- (sum(df.mem$Type==" Targ" & df.mem$Repeat==" Yes" & (df.mem$Response==" 2" | df.mem$Response==" 1") & df.mem$Duration==" T1") )/ sum(df.mem$Type==" Targ" & df.mem$Repeat==" Yes"&df.mem$Duration==" T1" & df.mem$Response!= "999")

pFA1.5 <- (sum(df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & (df.mem$Response==" 1" | df.mem$Response==" 2") & df.mem$Duration==" T1"))/ sum(df.mem$Type==" Lure" & df.mem$Repeat==" Yes"&df.mem$Duration==" T1" & df.mem$Response!= "999")

d1.5 <- qnorm(pHit1.5) - qnorm(pFA1.5)



pHit1.5 <- (sum(df.mem$Type==" Targ" & df.mem$Repeat==" Yes" & (df.mem$Response==" 2") & df.mem$Duration==" T1") )/ sum(df.mem$Type==" Targ" & df.mem$Repeat==" Yes"&df.mem$Duration==" T1" & df.mem$Response!= "999")

pFA1.5 <- (sum(df.mem$Type==" Lure" & df.mem$Repeat==" Yes" & (df.mem$Response==" 1" | df.mem$Response==" 2") & df.mem$Duration==" T1"))/ sum(df.mem$Type==" Lure" & df.mem$Repeat==" Yes"&df.mem$Duration==" T1" & df.mem$Response!= "999")*0.5

d1.5 <- qnorm(pHit1.5) - qnorm(pFA1.5)


#####



