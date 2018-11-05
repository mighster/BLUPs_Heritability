
setwd("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/")
#Install/library packages
library(lme4)
library(ggplot2)
library(dplyr)
library(nlme)
library(plyr)
########################################
#Begin analysis with new df   #

#Import new df
df<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/Aug_Data_NoOutliers_Imputed_300.csv")

str(df)
#set columns as categorical variables
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)
df$Plant=as.factor(df$Plant)
df$MG=as.factor(df$MG)
df$GC=as.factor(df$GC)
df$Day=as.factor(df$Day)
df$Cluster=as.factor(df$Cluster)

nloc <- 14 #number of locations - use this to calculate heritability

#take only MG 2
#df <- subset(df, df$MG == "2")


#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")

#create function to identify maximum value in a column
colMax <- function(data) sapply(data, max, na.rm = TRUE)
#create function to identify minimum value in a column
colMin <- function(data) sapply(data, min, na.rm = TRUE)
#create function to identify mean value in a column
colMean <- function(data) sapply(data, mean, na.rm = TRUE)
#create function to identify median value in a column
colMedian <- function(data) sapply(data, median, na.rm = TRUE)
#create function to identify standard dev value in a column
colStdev <- function(data) sapply(data, sd, na.rm = TRUE)

######################################## Day 6 ############################################

#Create empty data frame for BLUP output
Day6DataOutput <- data.frame(matrix(vector(),300,1, dimnames=list(c(), c("Entry"))))
#Day6DataOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
#Day6DataOutput$Entry <- c(1:300)
Day6DataOutput$Entry <- c(1:300)
#Day6DataOutput$Entry <- c(1:116)

Day6DataOutput$Day <- c(6)

#this empty dataframe is for variance components
Day6VarComp <- data.frame()
Day6VarCompOutput <- data.frame()
HeritabilityDay6 <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(11:52)
52-11
str(colnum)
for (i in 1:42){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #model2 <- lme(y~SB+Entry,data=Day6_data1) #BLUP random effects
  
  summary(model)
  anova(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  blup = coef(model)$Entry
 
  #LINEBLUP = blup[,1]
  colnames(blup) <- trait
  #add columns to existing dataframe   
  Day6DataOutput <- cbind(Day6DataOutput,blup)
  
  #Modify variance component df
  #delete columns in variance component dataframe (we don't need it)
  varComp<-varComp[ , !(names(varComp) %in% drops)]
  #set the trait name from the loop
  varComp$Trait<-trait
  #add columns to existing dataframe
  Day6VarComp <- rbind(Day6VarComp,varComp) 
}

#reshape our variance components dataframe so that we can run the heritability script
Day6VarCompOutput <- reshape(Day6VarComp, idvar = "Trait", timevar = "grp", direction = "wide")

#the broad sense heritability script
#Taken from Gioia et al. 2017
HeritabilityDay6 <- ((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc)))

Day6ColMax <- colMax(Day6_data[,colnum])
Day6ColMin <- colMin(Day6_data[,colnum])
Day6ColMean <- colMean(Day6_data[,colnum])
Day6ColMedian <- colMean(Day6_data[,colnum])
Day6ColStdev <- colStdev(Day6_data[,colnum])

#CVg <- ((Day6VarCompOutput[,3])) / ((Day6ColMean)^2)
CVg <- (sqrt(Day6VarCompOutput[,3])) / ((Day6ColMean))
#out <- LSD.test(model,"Entry", p.adj="bonferroni")

#bind the heritability to the variance component data frame

Day6VarCompOutput <- cbind(Day6VarCompOutput,HeritabilityDay6,CVg,Day6ColMin,Day6ColMax,Day6ColMean,Day6ColMedian,Day6ColStdev)

#output that beast
write.csv(Day6DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/BLUPsDay6_Sept18.csv")
write.csv(Day6VarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/VarianceComponents_Day6_Sept18.csv")
    

######################################## Day 9 ############################################

#Create empty data frame for BLUP output
Day9DataOutput <- data.frame(matrix(vector(),300,1, dimnames=list(c(), c("Entry"))))
#Day9DataOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
#Day9DataOutput$Entry <- c(1:300)
Day9DataOutput$Entry <- c(1:300)
#Day9DataOutput$Entry <- c(1:116)

Day9DataOutput$Day <- c(9)

#this empty dataframe is for variance components
Day9VarComp <- data.frame()
Day9VarCompOutput <- data.frame()
HeritabilityDay9 <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(12:52)
str(Day9_data)
str(colnum)

#########################
#Error in fn(x, ...) : Downdated VtV is not positive definite
#So I removed seed weight and it worked
#########################

for (i in 1:41){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #model2 <- lme(y~SB+Entry,data=Day9_data1) #BLUP random effects
  
  summary(model)
  anova(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  colnames(blup) <- trait
  #add columns to existing dataframe   
  Day9DataOutput <- cbind(Day9DataOutput,blup)
  
  #Modify variance component df
  #delete columns in variance component dataframe (we don't need it)
  varComp<-varComp[ , !(names(varComp) %in% drops)]
  #set the trait name from the loop
  varComp$Trait<-trait
  #add columns to existing dataframe
  Day9VarComp <- rbind(Day9VarComp,varComp) 
}

#reshape our variance components dataframe so that we can run the heritability script
Day9VarCompOutput <- reshape(Day9VarComp, idvar = "Trait", timevar = "grp", direction = "wide")

#the broad sense heritability script
#Taken from Gioia et al. 2017
HeritabilityDay9 <- ((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc)))

Day9ColMax <- colMax(Day9_data[,colnum])
Day9ColMin <- colMin(Day9_data[,colnum])
Day9ColMean <- colMean(Day9_data[,colnum])
Day9ColMedian <- colMean(Day9_data[,colnum])
Day9ColStdev <- colStdev(Day9_data[,colnum])

#CVg <- ((Day9VarCompOutput[,3])) / ((Day9ColMean)^2)
CVg <- (sqrt(Day9VarCompOutput[,3])) / ((Day9ColMean))
#out <- LSD.test(model,"Entry", p.adj="bonferroni")

#bind the heritability to the variance component data frame

Day9VarCompOutput <- cbind(Day9VarCompOutput,HeritabilityDay9,CVg,Day9ColMin,Day9ColMax,Day9ColMean,Day9ColMedian,Day9ColStdev)

#output that beast
write.csv(Day9DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/BLUPsDay9_Sept18.csv")
write.csv(Day9VarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/VarianceComponents_Day9_Sept18.csv")
######################################## Day 12 ############################################


#Create empty data frame for BLUP output
Day12DataOutput <- data.frame(matrix(vector(),300,1, dimnames=list(c(), c("Entry"))))
#Day12DataOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
#Day12DataOutput$Entry <- c(1:300)
Day12DataOutput$Entry <- c(1:300)
#Day12DataOutput$Entry <- c(1:116)

Day12DataOutput$Day <- c(12)

#this empty dataframe is for variance components
Day12VarComp <- data.frame()
Day12VarCompOutput <- data.frame()
HeritabilityDay12 <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(12:52)
str(Day12_data)
str(colnum)

#########################
#Error in fn(x, ...) : Downdated VtV is not positive definite
#So I removed seed weight and it worked
#########################

for (i in 1:41){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #model2 <- lme(y~SB+Entry,data=Day12_data1) #BLUP random effects
  
  summary(model)
  anova(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  colnames(blup) <- trait
  #add columns to existing dataframe   
  Day12DataOutput <- cbind(Day12DataOutput,blup)
  
  #Modify variance component df
  #delete columns in variance component dataframe (we don't need it)
  varComp<-varComp[ , !(names(varComp) %in% drops)]
  #set the trait name from the loop
  varComp$Trait<-trait
  #add columns to existing dataframe
  Day12VarComp <- rbind(Day12VarComp,varComp) 
}


#reshape our variance components dataframe so that we can run the heritability script
Day12VarCompOutput <- reshape(Day12VarComp, idvar = "Trait", timevar = "grp", direction = "wide")

#the broad sense heritability script
#Taken from Gioia et al. 2017
HeritabilityDay12 <- ((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc)))

Day12ColMax <- colMax(Day12_data[,colnum])
Day12ColMin <- colMin(Day12_data[,colnum])
Day12ColMean <- colMean(Day12_data[,colnum])
Day12ColMedian <- colMean(Day12_data[,colnum])
Day12ColStdev <- colStdev(Day12_data[,colnum])

#CVg <- ((Day12VarCompOutput[,3])) / ((Day12ColMean)^2)
CVg <- (sqrt(Day12VarCompOutput[,3])) / ((Day12ColMean))
#out <- LSD.test(model,"Entry", p.adj="bonferroni")

#bind the heritability to the variance component data frame

Day12VarCompOutput <- cbind(Day12VarCompOutput,HeritabilityDay12,CVg,Day12ColMin,Day12ColMax,Day12ColMean,Day12ColMedian,Day12ColStdev)

#output that beast
write.csv(Day12DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/BLUPsDay12_Sept18.csv")
write.csv(Day12VarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/VarianceComponents_Day12_Sept18.csv")

#################################################################################################
#All days output
VarCompOutputAllDays <- data.frame()
VarCompOutputAllDays <- cbind(Day6VarCompOutput,Day9VarCompOutput,Day12VarCompOutput)
write.csv(VarCompOutputAllDays,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/VarCompOutputAllDays.csv")

rm(DataOutputAllDays)
DataOutputAllDays <- rbind(Day6DataOutput,Day9DataOutput,Day12DataOutput)
write.csv(DataOutputAllDays, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/MG2_AdjustedBLUPsAllDays.csv")

