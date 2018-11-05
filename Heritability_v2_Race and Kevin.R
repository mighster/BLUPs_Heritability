# Description: online tutorial for repeatabilities
# lme4 package
# by Race Higgins
# March 2018

###############################################################################################

#https://www.slideshare.net/AvjinderSingh/tutorial-for-estimating-broad-and-narrow-sense-heritability-using-r

#Tutorial for Estimating Broad and Narrow Sense Heritability using R -  Avjinder Singh Kaler
#This tutorial is to estimate broad and narrow sense heritability using R package "sommer". You have any question, you can contact me on this email askaler@uark.edu Download and Install software. 
#1. R program https://cran.r-project.org/bin/windows/base/ 
#2. R Studio https://www.rstudio.com/products/rstudio/download/
#3. Steps in Heritability

#Step 1: File Formatting 
#You need only one phenotypic file if you are estimating broad sense heritability. 
#If you are estimating narrow sense heritability, then you also need genotypic file. 
#Phenotype File Format: Make your phenotype using this format. 
#In this file, you have genotype name "Name", environment "Env" (combination of Location and Year), Location "Loc", year "Year", "Block" (replication), and response variable "y".

#4. Genotype File Format: You only need this file if you are estimating narrow sense heritability. 
#You need numeric format for genotype file Column should be markers and row should be Plant ID same as phenotype file. 
#Save both file as ".txt".

#5. Step 2: Install and Load the packages. 
#install these packages 
install.packages("bigmemory") 
install.packages("biganalytics") 
install.packages("sommer") 
install.packages("rrBLUP")
# load the packages 
library("bigmemory") 
library("biganalytics") 
library("sommer") 
library("rrBLUP")

#Step 3: Set working directory and import data 
#Set your working directory where you have your data files.
#set working directory
setwd("C:/Users/falk/Google Drive/PhD/")
#6. Step 4: Read your ".txt" file 

Total=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/BlueSteelData04_17_2018.csv")

summary(df)
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)
colnames(df)
colnum=c(6:39)
names=colnames(df)

#Split data by timepoint (total of five, missing one)
mylist <- split(df, df$Day)
length(mylist) #2




#########################################################################

#Broad sense heritability - outliers removed with lme4 package (much faster)

########################################################################

library(lme4)

df=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/BlueSteelData05_11_2018_NoGR_CHECKSTOP.csv")

summary(df)
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)
colnames(df)
colnum=c(18:53)
names=colnames(df)


#Split data by timepoint (total of five, missing one)
mylist <- split(df, df$Day)
length(mylist) #2

str(df)


k=3 #check loop
i=1
dflist=list() #empty list for loop

for (k in 1:3){
  df1=mylist[[k]]
  #df1=subset(df1,Family != "Parent" )
  vec=c()
  for (i in 1:length(colnum)){
    x=colnum[i]
    trait=colnames(df1)[x]
    print(trait)
    colnames(df1)[x]="y"
    re.mod1=lmer(y~(1|Rep)+(1|SB/Rep)+(1|Entry)+(1|Entry:Rep),df1) #BLUP
    ans=as.data.frame(print(VarCorr(re.mod1),comp="Variance"))
    ans[i]=anova(re.mod1) #run the ANOVA on the fixed model
     #ANOVA output
    V_GE <- ans[which(ans$grp == "Entry:Rep"),4] 
    V_G <- ans[which(ans$grp == "Entry"),4]
    V_E <- ans[which(ans$grp == "Rep"),4]
    Ve <- ans[which(ans$grp == "Residual"),4]
    
    n.env <- length(levels(df1$Rep)) 
    h2 <- V_G/(V_G + V_GE/n.env +Ve/(2*n.env)) #2 = number of plants
    print(h2)
    vec[i]=h2
    names(vec)[i]=trait
    colnames(df1)[x] = trait
  }
  dflist[[k]]=ans
}

final=rbind(dflist[[1]],dflist[[2]],dflist[[3]]) #bind both timepoint heritabilites
final[1:3,1:34]
write.csv(final,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/Repeatabilities_v2.csv", row.names = F)

#the 2 is a reference for block. 
#You need to mention here how many blocks or replications you have. 

#this will give you broad sense heritability 

##############################################################

#yield model, including package to get r

#######################################################################

install.packages("MuMIn")
library(MuMIn)

yield=lmer(buacMM6~(1|CAI)+(1|NMDI)+(1|NWIB)+(1|PRI)+(1|PSRI)+(1|RARSa),df1) #BLUP
anova(yield)
summary(yield)

r.squaredGLMM(yield)
#marginal is fixed effects
#conditional is with random effects

##################################################################
