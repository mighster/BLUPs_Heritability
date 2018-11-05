setwd("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/")
#Install/library packages
library(lme4)
library(ggplot2)
library(dplyr)
library(nlme)
library(plyr)

#######################################################################################################

#Import new df
data<-read.csv("August_Extracted_Data.csv")

#set columns as categorical variables
data$Rep=as.factor(data$Rep)
data$SB=as.factor(data$SB)
data$Entry=as.factor(data$Entry)
data$Plant=as.factor(data$Plant)
data$MG=as.factor(data$MG)

#take only MG 2
data <- subset(data, data$MG == "2")
df <- subset(data, data$Rep == "1" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#this empty dataframe is for variance components
Day6VarComp <- data.frame()
Day6VarCompOutput <- data.frame()
HeritabilityDay6 <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data
colnum=c(20:56)

for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 2 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(Day6VarCompOutput$Trait, HeritabilityDay6)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 4 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(HeritabilityDay6output, HeritabilityDay6)


#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 6 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(HeritabilityDay6output, HeritabilityDay6)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 8 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(HeritabilityDay6output, HeritabilityDay6)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 10 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(HeritabilityDay6output, HeritabilityDay6)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" | data$Rep == "6" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 12 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(HeritabilityDay6output, HeritabilityDay6)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" | data$Rep == "6"  | data$Rep == "7" )

#split data into df Days
Day6_data <- subset(df, df$Day == "6")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day6_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day6DataOutput <- cbind(blup)
  
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
nloc <- 14 #number of locations - use this to calculate heritability

HeritabilityDay6 <- (((Day6VarCompOutput[,3])) / (((Day6VarCompOutput[,3])) + (((Day6VarCompOutput[,5])) / (nloc))))
HeritabilityDay6output <- cbind(HeritabilityDay6output, HeritabilityDay6)

colnames(HeritabilityDay6output) <- c("Trait", "Rep = 2", "Rep = 4", "Rep = 6", "Rep = 8", "Rep = 10", "Rep = 12", "Rep = 14")

#output that beast
write.csv(HeritabilityDay6output,"HeritabilityDay6output_test.csv")


#DAY 9 =============== Day9 ==============DAY 9 =============== Day9 ==============DAY 9 =============== Day9 ==============DAY 9 =============== Day9 ==============

#take only MG 2
data <- subset(data, data$MG == "2")
df <- subset(data, data$Rep == "1" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#this empty dataframe is for variance components
Day9VarComp <- data.frame()
Day9VarCompOutput <- data.frame()
HeritabilityDay9 <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data

for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 2 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(Day9VarCompOutput$Trait, HeritabilityDay9)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 4 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(HeritabilityDay9output, HeritabilityDay9)


#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 6 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(HeritabilityDay9output, HeritabilityDay9)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 8 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(HeritabilityDay9output, HeritabilityDay9)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 10 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(HeritabilityDay9output, HeritabilityDay9)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" | data$Rep == "6" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 12 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(HeritabilityDay9output, HeritabilityDay9)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" | data$Rep == "6"  | data$Rep == "7" )

#split data into df Days
Day9_data <- subset(df, df$Day == "9")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day9_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day9DataOutput <- cbind(blup)
  
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
nloc <- 14 #number of locations - use this to calculate heritability

HeritabilityDay9 <- (((Day9VarCompOutput[,3])) / (((Day9VarCompOutput[,3])) + (((Day9VarCompOutput[,5])) / (nloc))))
HeritabilityDay9output <- cbind(HeritabilityDay9output, HeritabilityDay9)

colnames(HeritabilityDay9output) <- c("Trait", "Rep = 2", "Rep = 4", "Rep = 6", "Rep = 8", "Rep = 10", "Rep = 12", "Rep = 14")

#output that beast
write.csv(HeritabilityDay9output,"HeritabilityDay9output_test.csv")

#===============================================================================================================================

#take only MG 2
data <- subset(data, data$MG == "2")
df <- subset(data, data$Rep == "1" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#this empty dataframe is for variance components
Day12VarComp <- data.frame()
Day12VarCompOutput <- data.frame()
HeritabilityDay12 <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

#take only the columns with numerical data

for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 2 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(Day12VarCompOutput$Trait, HeritabilityDay12)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 4 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(HeritabilityDay12output, HeritabilityDay12)


#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 6 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(HeritabilityDay12output, HeritabilityDay12)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 8 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(HeritabilityDay12output, HeritabilityDay12)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 10 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(HeritabilityDay12output, HeritabilityDay12)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" | data$Rep == "6" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 12 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(HeritabilityDay12output, HeritabilityDay12)

#######################################################################################################


df <- subset(data, data$Rep == "1" | data$Rep == "2" | data$Rep == "3" | data$Rep == "4" | data$Rep == "5" | data$Rep == "6"  | data$Rep == "7" )

#split data into df Days
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
for (i in 1:36){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),Day12_data1) #BLUP random effects
  #  summary(model)
  varComp<-as.data.frame(VarCorr(model,comp="vcov"))
  #  blup = coef(model)$Entry
  
  #LINEBLUP = blup[,1]
  #  colnames(blup) <- trait
  #add columns to existing dataframe   
  #  Day12DataOutput <- cbind(blup)
  
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
nloc <- 14 #number of locations - use this to calculate heritability

HeritabilityDay12 <- (((Day12VarCompOutput[,3])) / (((Day12VarCompOutput[,3])) + (((Day12VarCompOutput[,5])) / (nloc))))
HeritabilityDay12output <- cbind(HeritabilityDay12output, HeritabilityDay12)

colnames(HeritabilityDay12output) <- c("Trait", "Rep = 2", "Rep = 4", "Rep = 6", "Rep = 8", "Rep = 10", "Rep = 12", "Rep = 14")

#output that beast
write.csv(HeritabilityDay12output,"HeritabilityDay12output_test.csv")
















































