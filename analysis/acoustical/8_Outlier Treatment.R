#Use this script to identify and correct outliers in the IDS data in two ways: Winsorization (using the 5% upper and lower tails of the data as cutoffs) and imputation via meaning (using z-scores > |5|)

library(dplyr)
library(DescTools)
library(naniar)
library(compareDF)
library(reshape)

#Read Files, set up dataframes
IDS<-read.csv(file = '~/git/ids/Data/IDS.csv')
SongInfo<-read.csv(file = '~/git/ids/Results/SongInfo.csv')

#Set problem variables as numeric
IDS$praat_f0_max<- as.numeric(IDS$praat_f0_max)
IDS$praat_f0_range<- as.numeric(IDS$praat_f0_range)

##Winsorization Procedure
#Separate working set from baseline IDS
IDSW<- IDS
#remove non-continuous variables from dataset
IDSW[1:6]<- NULL
#Winsorize
IDSWinsor<- IDSW %>% mutate_all(Winsorize, na.rm=TRUE)
#Return non-continuous variables
IDSWinsor<-cbind(IDS[,1:6], IDSWinsor)

##Mean imputation
#Separate working set from baseline IDS and identify outliers
IDSData<- IDS
#remove non-continuous variables from dataset
IDSData[1:6]<- NULL
#Compute z-scores
ZIDS<-scale(IDSData, center=TRUE,scale=TRUE)
#identify number of cells w/ scores > 5, 403
length(ZIDS[ZIDS > 5])
#identify number of cells w/ scores < -5, 101
length(ZIDS[ZIDS < -5])

#Turn scores >|5| into missing values
ZIDS[ZIDS >5]<-NA
ZIDS[ZIDS <-5]<-NA
na_if(ZIDS,"NA")

#create dataframe of NA locations and place NAs within original dataset 
NA_location <-is.na(ZIDS) 
IDSData[NA_location]<-ZIDS[NA_location]
#Return categorical variables, add sex
IDSData<-cbind(IDS[,1:6], IDSData)
Sex<- SongInfo %>% 
  select(id,Gender)
IDSData<-merge(Sex,IDSData, by="id")

#Impute By Mean and Fix Variables
IDSImputeMean<- IDSData %>% 
  group_by(id_site, Gender, infantdir, song)%>% 
  #Mean by song type, site, and sex
  mutate_all(list(~ifelse(is.na(.), mean(., na.rm = TRUE),.)))

#Sex and extra columns
IDSImputeMean[1:2]<- NULL
IDSImputeMean<-as.data.frame(IDSImputeMean)
IDSImputeMean<-cbind(id=IDS[,1],IDSImputeMean)

#Write CSVs
write.csv(IDSImputeMean,'IDSImputeMean.csv',row.names = F)
write.csv(IDSWinsor,'IDSWinsor.csv',row.names = F)