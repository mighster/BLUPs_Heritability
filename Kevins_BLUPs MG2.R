
setwd("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/")
#Install/library packages
library(lme4)
library(ggplot2)
library(dplyr)
library(nlme)
library(plyr)
library(agricolae)
library(emmeans)
library(pbkrtest)
########################################
#Begin analysis with new df   #

#Import new df
df<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_Data_NoOutliers_Imputed.csv")

#set columns as categorical variables
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)

nloc <- 14 #number of locations - use this to calculate heritability

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")

#create function to identify maximum value in a column
colMax <- function(data) sapply(data, max, na.rm = TRUE)

#create function to identify minimum value in a column
colMin <- function(data) sapply(data, min, na.rm = TRUE)

#create function to identify minimum value in a column
colMean <- function(data) sapply(data, mean, na.rm = TRUE)

######################################## Day 6 ############################################


#Create empty data frame for BLUP output
Day6DataOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))
#fill empty dataframe with 1-116 so that the cbind will work later on
Day6DataOutput$Entry <- c(1:116)


#this empty dataframe is for variance components
Day6VarComp <- data.frame()
Day6VarCompOutput <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(18:56) 

for (i in 1:39){  #this second loop runs through each TRAIT, one at a time
    x=colnum[i]  #set the current [i] column as x
    trait=colnames(Day6_data)[x] #sets the current column header as the trait name
    #print(trait)  #prints the trait name using the column header from previous line
    Day6_data1 <- Day6_data
    colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
    model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
    summary(model)
    varComp<-as.data.frame(VarCorr(model,comp="vcov"))
    blup = ranef(model)
    blup = blup$Entry
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
CVA <- ((Day6VarCompOutput[,3])) / ((Day6ColMean)^2)
#out <- LSD.test(model,"Entry", p.adj="bonferroni")

#bind the heritability to the variance component data frame
Day6VarCompOutput <- cbind(Day6VarCompOutput,HeritabilityDay6,CVA,Day6ColMin,Day6ColMax,Day6ColMean)

#output that beast
write.csv(Day6DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_BLUPsDay6.csv")
write.csv(Day6VarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_VarianceComponents_Day6.csv")
    




######################################## Day 9 ############################################

#Create empty data frame for BLUP output
Day9DataOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))
#fill empty dataframe with 1-116 so that the cbind will work later on
Day9DataOutput$Entry <- c(1:116)

#this empty dataframe is for variance components
Day9VarComp <- data.frame()
Day9VarCompOutput <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(18:56) 

for (i in 1:39){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  varComp<-as.data.frame(VarCorr(model))
  blup = ranef(model)
  blup = blup$Entry
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
HeritabilityDay9 <- (Day9VarCompOutput[,3]) / ((Day9VarCompOutput[,3]) + ((Day9VarCompOutput[,5]) / (nloc)))

Day9ColMax <- colMax(Day9_data[,colnum])
Day9ColMin <- colMin(Day9_data[,colnum])
Day9ColMean <- colMean(Day9_data[,colnum])
CVA <- ((Day9VarCompOutput[,3])) / ((Day9ColMean)^2)

#bind the heritability to the variance component data frame
Day9VarCompOutput <- cbind(Day9VarCompOutput,HeritabilityDay9,CVA,Day9ColMin,Day9ColMax,Day9ColMean)

#output that beast
write.csv(Day9DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_BLUPsDay9.csv")
write.csv(Day9VarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_VarianceComponents_Day9.csv")


######################################## Day 12 ############################################

#Create empty data frame for BLUP output
Day12DataOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))
#fill empty dataframe with 1-116 so that the cbind will work later on
Day12DataOutput$Entry <- c(1:116)

#this empty dataframe is for variance components
Day12VarComp <- data.frame()
Day12VarCompOutput <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(18:56) 

for (i in 1:39){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  varComp<-as.data.frame(VarCorr(model))
  blup = ranef(model)
  blup = blup$Entry
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
HeritabilityDay12 <- (Day12VarCompOutput[,3]) / ((Day12VarCompOutput[,3]) + ((Day12VarCompOutput[,5]) / (nloc)))

Day12ColMax <- colMax(Day12_data[,colnum])
Day12ColMin <- colMin(Day12_data[,colnum])
Day12ColMean <- colMean(Day12_data[,colnum])
CVA <- ((Day12VarCompOutput[,3])) / ((Day12ColMean)^2)

#bind the heritability to the variance component data frame
Day12VarCompOutput <- cbind(Day12VarCompOutput,HeritabilityDay12,CVA,Day12ColMin,Day12ColMax,Day12ColMean)

#output that beast
write.csv(Day12DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_BLUPsDay12.csv")
write.csv(Day12VarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Data4Paper/MG2_VarianceComponents_Day12.csv")


BigVarCompOutput <- rbind(Day6VarCompOutput,Day9VarCompOutput,Day12VarCompOutput)

#           out <- LSD.test(FixedModel,"Entry", p.adj="bonferroni")
