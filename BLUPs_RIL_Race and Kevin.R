
# Description: make Phenotype BLUPs for QTL mapping
# by Race Higgins
# March 2018

######## BLUPs for each phenotype

###############################################################

library(lme4)

df=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/KF_Imputed_DF.csv")
str(df$Rep)
summary(df$Rep)
#set columns as categorical variables
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)
str(df)
df1 <- na.omit(df)
str(df1)
colnames(df) #look at column names
colnum=c(18:53) #take only the columns with numerical data
names=colnames(df) #set column names to a list


#Split data by Day (total of five, missing one)
mylist <- split(df, df$Day)
length(mylist) #3 days
head(mylist)

i=2 #check loop
dflist=list() #empty list for loop
fdflist=list()
anvlist=list() 
re.anvlist=list()
anvlist2=list()
re.anvlist2=list()
outputBLUPData <- data.frame() #empty dataframe (it must have something in it for the cbind to work)

# function to combine unequal rows
mbind<-function(...){
    Reduce( function(x,y){cbind(x,y[match(row.names(x),row.names(y)),])}, list(...) )
}


k=2 #check DAY loop by setting K to a random number between 1 and 3
i=33 #check TRAIT loop by setting i to a random number between 1 and 36
length(mylist) #length of mylist (How many days)
length(colnum) #length of colnum (how many traits)
colnum

for (k in 1:length(mylist)){ #this first loop runs through each DAY, one at a time
  df1=mylist[[k]]
for (i in 4:6){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(df1)[x] #sets the current column header as the trait name
  print(trait)  #prints the trait name using the column header from previous line
  colnames(df1)[x]="y"  #renames the trait variable as "y" for the model analysis below
  sim.mod1 = lm(TotalRootLength~Entry ,df) #simple model 
  #summary(sim.mod1)
  fe.mod1=lm(TotalRootLength~Rep+SB/Rep+Entry+Rep*Entry, df) #fixed model, y~Rep+SB/Rep+Entry+Rep*Entry
  #fe.mod1=lm(y~Entry*Env,df1) #alternate fixed model
  anv=anova(fe.mod1) #run the ANOVA on the fixed model
  anv #ANOVA output
  
  #ANOVA output creation
  anv$Day=names(mylist[k]) #Plugs in DAY # from K into the ANV table I created
  anv$trait=trait          #Plugs in Trait name from i into the ANV table I created
  anv$factor <- rownames(anv) #Plugs in model factors into the ANV table I created
  anvlist[[i]]=as.data.frame(anv) #this makes a new dataframe for each run through the ANOVA
  #re.mod2=lmer(TotalRootLength~(1|Rep)+(1|SB%in%Rep)+(1|Entry)+(1|Entry:Rep),df) #BLUP random effects model
  summary(re.mod2)
  #re.mod1=lmer(y~(1|Entry)+(1|Entry:Env)+(1|Env)+(1|Env:Check_1:Block),df1) #BLUP
  #re.mod2=lmer(y~(1|Entry)+(1|Entry:Env)+(1|Env),df1) #BLUP
  #re.mod3=lmer(y~(1|Entry),df1) #BLUP
  #re.anv=anova(re.mod3,re.mod2,re.mod1)
  #re.anv$Day=names(mylist[k])
  #re.anv$trait=trait
  #re.anvlist[[i]]=as.data.frame(re.anv)
  #summary(ranef(re.mod1))
  
  #scrapes the fixed model output for it's intercept to use in the graph below
  u=coef(fe.mod1)["(Intercept)"]
  
  #BLUE vs BLUP dataframe: compare the variable from a fixed model with the same variable in a random model (BLUP)
  dataframe1 = data.frame(fe1=coef(sim.mod1)+u, re1=coef(re.mod2)$Entry) 
  
  #scrapes the Entry numbers from the random model and sets them as row names in the new dataframe
  rownames(dataframe1)=rownames(coef(re.mod2)$Entry)
  
  dataframe1
  dataframe1[1,1]=dataframe1[1,1]-u
  #dataframe1=dataframe1[-1,]
  
  ###NO PLOTS 
  #plot(dataframe1$fe1,dataframe1$X.Intercept., main = trait, xlab = "LSMean", ylab = "BLUP")
  ###NO PLOTS 
  #legend("topleft", bty="n", legend=  paste("R2 =",round(cor(dataframe1$fe1,dataframe1$X.Intercept.),2)))
  colnames(df1)[x]=trait
  colnames(dataframe1)[2]=trait
  dflist[[i+1]]=dataframe1
  
  #if(i==14){dflist[[1]]=dataframe1} #I don't know what this is doing so I removed it
  print(nrow(dataframe1))
}
  outputBLUPData <- cbind(outputBLUPData,dflist)

  
  final=do.call(mbind,dflist)

  #final$Day=names(mylist)[k]
  fdflist[[k]]=final
  anvlist2[[k]]=do.call(rbind,anvlist)
  re.anvlist2[[k]]=do.call(rbind,re.anvlist)
}



#combine BLUPs from both growth stages
end=do.call(rbind,fdflist)
str(end)
rem=c(seq(2,30,2),31)
#remove duplicated columns (on purpose)
end1=end[,-1:-2] #added a complete trait to the begining so merge would work correctly
#remove the fixed effect model effects
end1=subset(end1,select=rem)

#quickly plot blups against fixed genotype model for each trait
#for (u in 1:(length(rem)-1)){
#o=rem[u]
#plot(end[,o],end[,o-1],main = colnames(end)[o])
#}

write.table(end1,"BLUPs.csv",na="-100.00",sep = ",")

getwd()


#################Kyles code for getting BLUPs from this model  
ranef(re.mod1)

################################

#anova tables for fixed and random effects
#results from previous section

#########################################

star <- function(pval) {
  if (pval <= 0.001) {
    return("***")
  }
  if (pval <= 0.01) {
    return(" **")
  }		
  if (pval <= 0.05) {
    return("  *")
  }
  else {return("   ")
  }
} #function to make star significance levels
library(reshape2) #library to reshape dataframe

fixed=do.call(rbind,anvlist2) #combine on top of each other anova's from both timeopoints
fixed[is.na(fixed)] <- .99  #star function can't handle missing
fixed$sign=sapply(fixed$`Pr(>F)`,star) #make a "star" column
startable=dcast(fixed,Day + factor ~ trait, value.var = "sign" ) #reshape for table (add degrees later)
#df were different for each trait because of missing data
missingdf=subset(fixed,trait=="VREI2",c(1,6,8))
startable1=merge(missingdf,startable)
#assume maximum possible

random=do.call(rbind,re.anvlist) #look at blup model fits

write.csv(startable1,"C:/Users/rhiggin2/Desktop/fixed.csv")
write.csv(random,"C:/Users/rhiggin2/Desktop/random.csv")
anvlist
 