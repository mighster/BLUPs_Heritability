library(KRIS)
library(dplyr)
GWAS_GD[1:5,1:5]


names(GWAS_GD)[2]<-"Name.x"

together <- inner_join(Day9_data,Day9_metadata[,c(1:ncol(Day9_metadata))], "Entry")
rowindex <- c(1:292)
together <- cbind(rowindex,together)
SNP1.list <- together %>% filter(snp35k.8 == '1') %>% select(rowindex)
C1 <- unlist(SNP1.list) 
SNP2.list <- together %>% filter(snp35k.8 == "2") %>% select(rowindex)
C2 <- unlist(SNP2.list)
SNP3.list <- together %>% filter(snp35k.8 == "3") %>% select(rowindex)
C3 <- unlist(SNP3.list)
SNP4.list <- together %>% filter(snp35k.8 == "4") %>% select(rowindex)
C4 <- unlist(SNP4.list)
SNP5.list <- together %>% filter(snp35k.8 == "5") %>% select(rowindex)
C5 <- unlist(SNP5.list)
SNP6.list <- together %>% filter(snp35k.8 == "6") %>% select(rowindex)
C6 <- unlist(SNP6.list)
SNP7.list <- together %>% filter(snp35k.8 == "7") %>% select(rowindex)
C7 <- unlist(SNP7.list)
SNP8.list <- together %>% filter(snp35k.8 == "8") %>% select(rowindex)
C8 <- unlist(SNP8.list)

Clist <- list(C1,C2,C3,C4,C5,C6,C7,C8)
combos <- as.data.frame((combn(c("1","2","3","4","5","6","7","8"), 2)))

m <- matrix(0, ncol = 1, nrow = 0)
datalist <- as.data.frame(m)

i = 1
for (i in 1:28){
  xtemp = as.numeric(as.character(combos[1,i]))
  x <- unlist(Clist[xtemp])
  ytemp = as.numeric(as.character(combos[2,i]))
  y <- unlist(Clist[ytemp])
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  datalist <- rbind(datalist,dat)
}
colnames(datalist) <- "Combo"
ttt <- as.data.frame(as.factor(t(datalist)))
transposed_datalist<- t(ttt)
FST_betweenGBCmean <- rbind(combos,transposed_datalist)
write.csv(FST_betweenGBCmean,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/FST_betweenGBCmeans.csv")



GD_list <- GWAS_GD[,3:35550]
GD_list[1:10,1:20]

#C1combos <- as.data.frame(combn((C1),2))
C1 <- unlist(SNP1.list)
m <- matrix(0, ncol = 1, nrow = 0)
C1combolist <- as.data.frame(m)
i=1
for (i in 1:29){
  #x = as.numeric(as.character(C1combos[1,i]))
  #y = as.numeric(as.character(C1combos[2,i]))
  x = C1[i]
  y = C1
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C1combolist <- rbind(C1combolist,dat)
}
colnames(C1combolist) <- "Combo"
print(C1combolist)
C1mean <- colmean(C1combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C2combolist <- as.data.frame(m)
i=1
for (i in 1:96){
  #x = as.numeric(as.character(C2combos[1,i]))
  #y = as.numeric(as.character(C2combos[2,i]))
  x = C2[i]
  y = C2
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C2combolist <- rbind(C2combolist,dat)
}
colnames(C2combolist) <- "Combo"
print(C2combolist)
C2mean <-colmean(C2combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C3combolist <- as.data.frame(m)
i=1
for (i in 1:20){
  #x = as.numeric(as.character(C3combos[1,i]))
  #y = as.numeric(as.character(C3combos[2,i]))
  x = C3[i]
  y = C3
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C3combolist <- rbind(C3combolist,dat)
}
colnames(C3combolist) <- "Combo"
print(C3combolist)
C3mean <-colmean(C3combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C4combolist <- as.data.frame(m)
i=1
for (i in 1:41){
  #x = as.numeric(as.character(C4combos[1,i]))
  #y = as.numeric(as.character(C4combos[2,i]))
  x = C4[i]
  y = C4
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C4combolist <- rbind(C4combolist,dat)
}
colnames(C4combolist) <- "Combo"
print(C4combolist)
C4mean <-colmean(C4combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C5combolist <- as.data.frame(m)
i=1
for (i in 1:40){
  #x = as.numeric(as.character(C5combos[1,i]))
  #y = as.numeric(as.character(C5combos[2,i]))
  x = C5[i]
  y = C5
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C5combolist <- rbind(C5combolist,dat)
}
colnames(C5combolist) <- "Combo"
print(C5combolist)
C5mean <-colmean(C5combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C6combolist <- as.data.frame(m)
i=1
for (i in 1:8){
  #x = as.numeric(as.character(C6combos[1,i]))
  #y = as.numeric(as.character(C6combos[2,i]))
  x = C6[i]
  y = C6
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C6combolist <- rbind(C6combolist,dat)
}
colnames(C6combolist) <- "Combo"
print(C6combolist)
C6mean <-colmean(C6combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C7combolist <- as.data.frame(m)
i=1
for (i in 1:38){
  #x = as.numeric(as.character(C7combos[1,i]))
  #y = as.numeric(as.character(C7combos[2,i]))
  x = C7[i]
  y = C7
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C7combolist <- rbind(C7combolist,dat)
}
colnames(C7combolist) <- "Combo"
print(C7combolist)
C7mean <-colmean(C7combolist)
##########################################################################
m <- matrix(0, ncol = 1, nrow = 0)
C8combolist <- as.data.frame(m)
i=1
for (i in 1:38){
  #x = as.numeric(as.character(C8combos[1,i]))
  #y = as.numeric(as.character(C8combos[2,i]))
  x = C8[i]
  y = C8
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  C8combolist <- rbind(C8combolist,dat)
}
colnames(C8combolist) <- "Combo"
print(C8combolist)
C8mean <-colmean(C8combolist)
##########################################################################
withinGBCmeans <- rbind(C1mean,C2mean,C3mean,C4mean,C5mean,C6mean,C7mean,C8mean)
colnames(withinGBCmeans) <- "Mean"
write.csv(withinGBCmeans,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Genetic Distance/FST_withinGBCmeans.csv")












C2combos <- as.data.frame(combn((C2),2))
m <- matrix(0, ncol = 1, nrow = 0)
C2combolist <- as.data.frame(m)
i=1
for (i in 1:406){
  x = as.numeric(as.character(C2combos[1,i]))
  y = as.numeric(as.character(C2combos[2,i]))
  y = C2
  fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],x,y) %>% as.data.frame()
  dat <- fst1[complete.cases(fst1), ] %>% mean()
  datalist <- rbind(datalist,dat)
}
colnames(datalist) <- "Combo"
print(datalist)

fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C1) %>% as.data.frame()
dat <- fst1[complete.cases(fst1), ] %>% mean()

fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C2) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C3) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C4) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C5) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C6) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C7) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C1,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C2,C3) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C2,C4) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
st1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C2,C5) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C2,C6) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C2,C7) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C2,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C3,C4) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C3,C5) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C3,C6) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C3,C7) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C3,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C4,C5) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C4,C6) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C4,C7) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C4,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C5,C6) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C5,C7) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C5,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C6,C7) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C6,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
fst1 <- fst.each.snp.hudson(GWAS_GD[,3:35550],C7,C8) %>% as.data.frame()
fst1[complete.cases(fst1), ] %>% mean()
