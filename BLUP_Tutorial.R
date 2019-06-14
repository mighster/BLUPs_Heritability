library(lme4)
library(dplyr)

#Import new df
dataReadin<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Data/BlueSteelData_Sept25_thinned.csv")
dataReadin <- dataReadin %>%
                    filter(!((Entry == "273")|(Entry == "202")|(Entry  == "182")|(Entry  == "190")|(Entry  == "204")|(Entry  == "167" )|(Entry  == "9" )|(Entry  == "98" )))
count(dataReadin$Entry)
newSubset <- subset(dataReadin, dataReadin$Day == "9")
df <- newSubset[,c(1:7,20:24,32:39)]

colnames(df)
hist(df$TRL)
hist(df$Root_weight)

#set columns as categorical variables (factors)
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)
df$Plant=as.factor(df$Plant)
df$MG=as.factor(df$MG)
df$GC=as.factor(df$GC)
df$Day=as.factor(df$Day)

#Create empty data frame for BLUP output
DataOutput <- data.frame(matrix(vector(),292,1, dimnames=list(c(), c("Entry"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
DataOutput$Entry <- unique(df[,3]) #fill in Entry numbers
DataOutput$Row <- c(1:292)
DataOutput$Day <- c(9) #fill in Day of data collection

#this empty dataframe is for variance components
DataVarComp <- data.frame()
DataVarCompOutput <- data.frame()
HeritabilityData <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

colnames(df)
str(df[,8:ncol(df)])
#take only the columns with numerical data
colnum=c(8:ncol(df))

for (i in 1:13){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(df)[x] #sets the current column header as the trait name
  df1 <- df #make a copy that we can use for our analysis
  colnames(df1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  
  #We are interested in random effects, which estimates the proportion of variation and not fixed effects. 
  #Knowing variability components allows us to calculate Heritability.
  #Random-effects terms are distinguished by vertical bars or pipes (|) 
  model <- lmer(y~(1|SB)+(1|Entry)+(1|SB:Entry),df1) #our random effects mixed model
  
  summary(model)
  
  varComp<-as.data.frame(VarCorr(model,comp="vcov")) #function calculates estimated variances between random-effects terms in a mixed-effects model  blup = coef(model)$Entry
  blup = coef(model)$Entry #coef extracts model coefficients from lmer objects returned by modeling functions
  hist(blup[,1]) #plot it out
  colnames(blup) <- trait #rename the BLUP column by the trait in our loop
  #add columns to existing dataframe   
  DataOutput <- cbind(DataOutput,blup) #ammends our dataframe with the new BLUP column for the new trait.
  
  #Modify variance component df by
  #deleting columns in variance component dataframe (we don't need it)
  varComp<-varComp[ , !(names(varComp) %in% drops)]
  #set the trait name from the loop
  varComp$Trait<-trait
  #add columns to existing dataframe
  DataVarComp <- rbind(DataVarComp,varComp) 
}

#reshape our variance components dataframe so that we can run the heritability script
DataVarCompOutput <- reshape(DataVarComp, idvar = "Trait", timevar = "grp", direction = "wide")

#the broad sense heritability script
#Taken from Gioia et al. 2017
nloc = 14
HeritabilityData <- ((DataVarCompOutput[,3])) / (((DataVarCompOutput[,3])) + (((DataVarCompOutput[,5])) / (nloc)))

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

#summary statistics
DataColMax <- colMax(df[,colnum])
DataColMin <- colMin(df[,colnum])
DataColMean <- colMean(df[,colnum])
DataColMedian <- colMean(df[,colnum])
DataColStdev <- colStdev(df[,colnum])

#CVg <- ((DataVarCompOutput[,3])) / ((DataColMean)^2)
CVg <- (sqrt(DataVarCompOutput[,3])) / ((DataColMean))
#out <- LSD.test(model,"Entry", p.adj="bonferroni")

#bind the heritability to the variance component data frame

DataVarCompOutput <- cbind(DataVarCompOutput,HeritabilityData,CVg,DataColMin,DataColMax,DataColMean,DataColMedian,DataColStdev)
DataVarCompOutput[1:10,]

DataVarCompOutput %>% filter(Trait == "TRL") %>% select(HeritabilityData) 

#output that beast
write.csv(Day6DataOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/BLUPsDay6_thinned_Nov14.csv")
write.csv(DataVarCompOutput,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/VarianceComponents_thinned_Nov14.csv")
