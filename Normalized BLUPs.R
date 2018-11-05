#create function to identify maximum value in a column
colMax <- function(data) sapply(data, max, na.rm = TRUE)
#create function to identify minimum value in a column
colMin <- function(data) sapply(data, min, na.rm = TRUE)

#Import new df
df <-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/KGF_BLUPS_alldays_sept7.csv")


#split data into df Days
df <- subset(df, df$MG == "2")

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")

#take only the columns with numerical data
colnum=c(5:42) 

#Create empty data frame for BLUP output
Day6NormalizedOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry")))) 
Entry <- Day6_data$Entry #take entry list from original data
Day6NormalizedOutput$Entry <- Entry #add entry list to new dataframe
Day6NormalizedOutput$Day <- c(6) #add day to new dataframe

#Normalize function = (mean - min) / (max - min)
Day6ColMax <- colMax(Day6_data[,colnum])
Day6ColMin <- colMin(Day6_data[,colnum])
Day6ColSpread <- Day6ColMax-Day6ColMin

#remove row information
Day6_numbers <- Day6_data[5:42]

for (i in 1:34){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_numbers)[x] #sets the current column header as the trait name
  Day6ColMax <- colMax(Day6_numbers[x]) #find column max for specific trait column
  Day6ColMin <- colMin(Day6_numbers[x]) #find column min for specific trait column
  Day6ColSpread <- Day6ColMax-Day6ColMin #spread = max - min
  newcol <- (Day6_numbers[x] - Day6ColMin) / Day6ColSpread #THIS IS THE NORMALIZE FORMULA, Normalize function = (mean - min) / (max - min)
  Day6NormalizedOutput <- cbind(Day6NormalizedOutput,newcol) #add it to the dataframe
}

#Create empty data frame for BLUP output
Day9NormalizedOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))
Entry <- Day9_data$Entry
Day9NormalizedOutput$Entry <- Entry
Day9NormalizedOutput$Day <- c(9)

Day9ColMax <- colMax(Day9_data[,colnum])
Day9ColMin <- colMin(Day9_data[,colnum])
Day9ColSpread <- Day9ColMax-Day9ColMin

#remove row information
Day9_numbers <- Day9_data[5:42]

for (i in 1:34){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_numbers)[x] #sets the current column header as the trait name
  Day9ColMax <- colMax(Day9_numbers[x])
  Day9ColMin <- colMin(Day9_numbers[x])
  Day9ColSpread <- Day9ColMax-Day9ColMin
  newcol <- (Day9_numbers[x] - Day9ColMin) / Day9ColSpread
  Day9NormalizedOutput <- cbind(Day9NormalizedOutput,newcol)
}

#Create empty data frame for BLUP output
Day12NormalizedOutput <- data.frame(matrix(vector(),116,1, dimnames=list(c(), c("Entry"))))
Entry <- Day12_data$Entry
Day12NormalizedOutput$Entry <- Entry
Day12NormalizedOutput$Day <- c(12)

Day12ColMax <- colMax(Day12_data[,colnum])
Day12ColMin <- colMin(Day12_data[,colnum])
Day12ColSpread <- Day12ColMax-Day12ColMin

#remove row information
Day12_numbers <- Day12_data[5:42]

for (i in 1:34){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_numbers)[x] #sets the current column header as the trait name
  Day12ColMax <- colMax(Day12_numbers[x])
  Day12ColMin <- colMin(Day12_numbers[x])
  Day12ColSpread <- Day12ColMax-Day12ColMin
  newcol <- (Day12_numbers[x] - Day12ColMin) / Day12ColSpread
  Day12NormalizedOutput <- cbind(Day12NormalizedOutput,newcol)
}

NormalizedOutput <- rbind(Day6NormalizedOutput,Day9NormalizedOutput,Day12NormalizedOutput)

write.csv(NormalizedOutput, "C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/MG2_NormalizedBLUPsAllDays.csv")
