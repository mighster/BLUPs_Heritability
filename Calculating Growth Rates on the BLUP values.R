setwd("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/")

#Import new df
df<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/AdjustedBLUPsAllDays.csv")

#set columns as categorical variables
df$Day=as.factor(df$Day)
df$Entry=as.factor(df$Entry)

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")

EntryList <- data.frame(1:300)
colnames(EntryList)[1]="Entry"
EntryList$Timepoint <- c(1)
timepoint1 <- (Day9_data[,3:38] - Day6_data[,3:38])
timepoint1 <- cbind(EntryList, timepoint1)

EntryList1 <- data.frame(1:300)
colnames(EntryList1)[1]="Entry"
EntryList1$Timepoint <- c(2)
timepoint2 <- (Day12_data[,3:38] - Day9_data[,3:38])
timepoint2 <- cbind(EntryList1, timepoint2)

#combine all entries
GCcombined <- rbind(timepoint1, timepoint2)

#subselect only my 3 selected genotypes
threeGenotypes <- subset(GCcombined, GCcombined$Entry == "127"| GCcombined$Entry == "298"| GCcombined$Entry == "199")

#output that beast
write.csv(GCcombined,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/GrowthRates_BLUPs_R.csv")
write.csv(threeGenotypes,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/BLUPs/GrowthRates_BLUPs_3genotypes_R.csv")
