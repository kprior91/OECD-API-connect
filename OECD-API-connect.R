
#install packages
library(ggplot2)
library(reshape2)
library(tidyverse)
library(readxl)
library(OECD)
library(dplyr)
library(xlsx)

#connecting to the OECD API using the OECD package

dataset_list <- get_datasets()
search_dataset("CRS",data=dataset_list)
dataset <- "CRS1"
dstruc <- get_data_structure(dataset)


sectortotals <- dstruc[['SECTOR']]$id[nchar(dstruc[['SECTOR']]$id)==3]

mysectors <- dstruc[['SECTOR']] %>% 
  filter(id %in% c(sectortotals)) %>% 
  select(id) %>%
  as.matrix() %>% as.character()

# D = constant prices
# A = current prices
myamount <- dstruc[['AMOUNTTYPE']] %>% 
  filter(id == 'D') %>%     
  select(id) %>%
  as.matrix() %>% as.character()

myaidtype <- dstruc[['AIDTYPE']] %>% 
  filter(id %in% c('A01','A02','B01','B03','B04','C01','D01','D02','E01','E02','F01','G01','H01','H02','H03','H04','H05')) %>%     
  select(id) %>%
  as.matrix() %>% as.character()

#112 = disbursements
#115 = commitments
myflowtype <- dstruc[['FLOWTYPE']] %>% 
  filter(id == '112') %>%     
  select(id) %>%
  as.matrix() %>% as.character()

# 100 = Official Development Assistance
myflow <- dstruc[['FLOW']] %>% 
  filter(id == '100') %>%
  select(id) %>%
  as.matrix() %>% as.character()

#100 = All Channels
mychannel <- dstruc[['CHANNEL']] %>% 
  filter(id == '100') %>%
  select(id) %>%
  as.matrix() %>% as.character()


# 10100 = All
recips <- ifelse(grepl("Total|total",dstruc$RECIPIENT$label),0,dstruc$RECIPIENT$id)
recips <- as.numeric(recips)
recips <- recips[recips>0 & recips<10000]
recips

myrecipients <- dstruc[['RECIPIENT']] %>% 
  filter(id %in% c(recips)) %>%
  select(id) %>%
  as.matrix() %>% as.character()


# Donors
mydonor <- dstruc[['DONOR']] %>% 
  filter(id %in% c(12)) %>%
  select(id) %>%
  as.matrix() %>% as.character()


df <- get_dataset(dataset,
                  filter = list(mydonor,myrecipients,mysectors,myflow,mychannel,myamount,myflowtype,myaidtype),
                  start_time = 2018, end_time = 2019)

df$DONOR.name <- dstruc$DONOR$label[match(df$DONOR,dstruc$DONOR$id)]
df$RECIPIENT.name <- dstruc$RECIPIENT$label[match(df$RECIPIENT,dstruc$RECIPIENT$id)]



COUNTRIES_disb <- df %>% select(obsTime,DONOR.name,DONOR,RECIPIENT.name,RECIPIENT,SECTOR,AIDTYPE,obsValue) %>% rename(obsValue.USD = obsValue)
COUNTRIES_comm <- df %>% select(obsTime,DONOR.name,DONOR,RECIPIENT.name,RECIPIENT,SECTOR,AIDTYPE,obsValue) %>% rename(obsValue.USD = obsValue)

write.csv(COUNTRIES_disb,'COUNTRIES_disb.csv')
write.csv(COUNTRIES_comm,'COUNTRIES_comm.csv')

