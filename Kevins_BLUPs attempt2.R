
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
df<-read.csv("KF_Imputed_DF.csv")

#set columns as categorical variables
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)

nloc <- 7 #number of locations - use this to calculate heritability

i=3 #check TRAIT loop by setting i to a random number between 1 and 36

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")



######################################## Day 6 ############################################


#Create empty data frame for BLUP output
Day6DataOutput <- data.frame(matrix(vector(),300,1, dimnames=list(c(), c("Entry"))))
#fill empty dataframe with 1-300 so that the cbind will work later on
Day6DataOutput$Entry <- c(1:300)

#this empty dataframe is for variance components
Day6VarComp <- data.frame()
#this empty dataframe is for dropped variance components
drops <- c("var1","var2","vcov") 

#take only the columns with numerical data
colnum=c(17:52) 

for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
    x=colnum[i]  #set the current [i] column as x
    trait=colnames(day6_data)[x] #sets the current column header as the trait name
    #print(trait)  #prints the trait name using the column header from previous line
    day6_data1 <- day6_data
    colnames(day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
    model <- lmer(y~(1|Rep)+(1|SB%in%Rep)+(1|Entry)+(1|Entry:Rep),day6_data1) #BLUP random effects
    varComp<-as.data.frame(VarCorr(model))
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
HeritabilityDay6 <- (Day6VarCompOutput[,3]) / (Day6VarCompOutput[,3] + (Day6VarCompOutput[,2] / nloc) + (Day6VarCompOutput[,6]/nloc))

#bind the heritability to the variance component data frame
Day6VarCompOutput <- cbind(Day6VarCompOutput,HeritabilityDay6)

#output that beast
write.csv(Day6DataOutput,"BLUPsDay6.csv")
write.csv(Day6VarCompOutput,"VarianceComponents_Day6.csv")
    




######################################## Day 9 ############################################

#Create empty data frame for BLUP output
Day9DataOutput <- data.frame(matrix(vector(),300,1, dimnames=list(c(), c("Entry"))))
#fill empty dataframe with 1-300 so that the cbind will work later on
Day9DataOutput$Entry <- c(1:300)

#this empty dataframe is for variance components
Day9VarComp <- data.frame()
#this empty dataframe is for dropped variance components
drops <- c("var1","var2","vcov") 

#take only the columns with numerical data
colnum=c(17:52) 

for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|Rep)+(1|SB%in%Rep)+(1|Entry)+(1|Entry:Rep),Day9_data1) #BLUP random effects
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
HeritabilityDay9 <- (Day9VarCompOutput[,3]) / (Day9VarCompOutput[,3] + (Day9VarCompOutput[,2] / nloc) + (Day9VarCompOutput[,6]/nloc))

#bind the heritability to the variance component data frame
Day9VarCompOutput <- cbind(Day9VarCompOutput,HeritabilityDay9)

#output that beast
write.csv(Day9DataOutput,"BLUPsDay9.csv")
write.csv(Day9VarCompOutput,"VarianceComponents_Day9.csv")


######################################## Day 12 ############################################

#Create empty data frame for BLUP output
Day12DataOutput <- data.frame(matrix(vector(),300,1, dimnames=list(c(), c("Entry"))))
#fill empty dataframe with 1-300 so that the cbind will work later on
Day12DataOutput$Entry <- c(1:300)

#this empty dataframe is for variance components
Day12VarComp <- data.frame()
#this empty dataframe is for dropped variance components
drops <- c("var1","var2","vcov") 

#take only the columns with numerical data
colnum=c(17:52) 

for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|Rep)+(1|SB%in%Rep)+(1|Entry)+(1|Entry:Rep),Day12_data1) #BLUP random effects
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
HeritabilityDay12 <- (Day12VarCompOutput[,3]) / (Day12VarCompOutput[,3] + (Day12VarCompOutput[,2] / nloc) + (Day12VarCompOutput[,6]/nloc))

#bind the heritability to the variance component data frame
Day12VarCompOutput <- cbind(Day12VarCompOutput,HeritabilityDay12)

#output that beast
write.csv(Day12DataOutput,"BLUPsDay12.csv")
write.csv(Day12VarCompOutput,"VarianceComponents_Day12.csv")
