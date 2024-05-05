install.packages("netstat")
library(stringr)
library(tidyverse)#For data Cleaning
library(dplyr)#Data Wrangling
library(stringr)#String Manipulation
#library(purrr)
library(rvest)#static webpage scraping
library(RSelenium)#dynamic scraping
library(wdman)
library(netstat)#find unused port
library(data.table)#for the rbindlist function
wdman::selenium()
chrome()
binman::list_versions("chromedriver")
chromecommmand<-chrome(retcommand = T,verbose = F,check = F)
chromecommmand
rs_driver_object<-rsDriver(browser = "chrome",chromever = "124.0.6367.91",verbose = F,port = free_port())
remDr<-rs_driver_object$client
remDr$open()
remDr$navigate("https://www.npci.org.in/what-we-do/upi//upi-ecosystem-statistics")
data_table<-remDr$findElement(using='id',"innerTabOneFeb24")
data_table_html<-data_table$getPageSource()
data_table_html
page<-read_html(data_table_html%>%unlist())
df<-html_table(page)
#####Setting Proper Colnames#####
for (i in 1:length(df)) 
{
    if(colnames(df[[i]])[2]!=str_sub(df[[1]][1,2],1,18))
    colnames(df[[i]])<-df[[i]][1,]
}
df[[1]]

#####Selecting specific tables by checking if they have "Remitter Bank" in them"####
k<-as.numeric(str_sub(capture.output(
  for (i in 1:length(df)) 
{
  if(str_sub(colnames(df[[1]])[2],1,18)==str_sub(colnames(df[[i]])[2],1,18))
    print(i)
  
}
),5 ))

#####Selecting Apps data####
colnames(df[[3]])[2]
l<-as.numeric(str_sub(capture.output(
  for (i in 1:length(df)) 
  {
    if(colnames(df[[3]])[2]==colnames(df[[i]])[2])
      print(i)
    
  }
),5 ))
#####Storing the remit bank data####
Remit_banks<-df[k]
Apps_upi<-df[l]
#####
nrow(Remit_banks[[20]])
#####Closing the server#####
remDr$close()
rD$server$stop()
rm(rD, remDr)
gc()

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
Remit_banks[[51]]

