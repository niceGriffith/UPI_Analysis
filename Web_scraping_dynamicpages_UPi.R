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
#wdman::selenium()
#chrome()
binman::list_versions("chromedriver")#Check what chromedriver you have and if it is compatible with chrome
chromecommmand<-chrome(retcommand = T,verbose = F,check = F)
#chromecommmand
rs_driver_object<-rsDriver(browser = "chrome",chromever = "124.0.6367.91",verbose = F,port = free_port())
#Close the tab(optional)
remDr<-rs_driver_object$client
remDr$open()#Keep this tab open
remDr$navigate("https://www.npci.org.in/what-we-do/upi//upi-ecosystem-statistics")
data_table<-remDr$findElement(using='id',"innerTabOneFeb24")
data_table_html<-data_table$getPageSource()
page<-read_html(data_table_html%>%unlist())
df<-html_table(page)#Tables Loaded
#####Setting Proper Colnames#####
df[[3]][2]
for (i in 1:length(df)) 
{
    if(colnames(df[[i]])[2]!=(df[[3]][2]))
    colnames(df[[i]])<-df[[i]][1,]
}
value <- as.character(df[[3]][1,2])  # Replace with the value you are checking for
value
# Function to check if the first element of the second column matches the value
check_first_element <- function(df, value) {
  if (ncol(df) >= 2 && nrow(df) > 0) {
    return(df[1, 2] == value)
  } else {
    return(FALSE)
  }
}
# Apply the function to each data frame in the list
matches_value <- sapply(df, check_first_element, value = value)
# Filter the list based on the first element of the second column
Merchant_UPI <- df[matches_value]
Merchant_UPI<-rev(Merchant_UPI)#Reverse the list
# Function to replace column names with the first row
replace_column_names <- function(df) {
  new_col_names <- as.character(df[1, ])
  df <- df[-1, ]
  colnames(df) <- new_col_names
  return(df)
}
# Apply the column name replacement to each data frame in the reversed list
Merchant_UPI1 <- lapply(Merchant_UPI, replace_column_names)
saveRDS(Merchant_UPI1,"UPI_MER.rds")

# Function to extract high transacting categories
extract_high_transacting <- function(df) {
  high_transacting <- df[df$Type == "High Transacting Categories", "MCC"]
  return(high_transacting)
}

# Apply the function to each data frame in the reversed list
high_transacting_categories <- lapply(Merchant_UPI1, extract_high_transacting)

# Combine all high transacting categories into a single vector
all_high_transacting <- unlist(high_transacting_categories)

# Get unique categories and their frequencies
unique_categories <- unique(all_high_transacting)
category_counts <- table(all_high_transacting)

# Create a data frame with the results
summary_df <- data.frame(
  merchant_category = unique_categories,
  count = as.vector(category_counts)
)

# Order by count descending
summary_df <- summary_df[order(-summary_df$count), ]

#Description_Table
# Function to extract high transacting categories and their descriptions
extract_high_transacting <- function(df) {
  high_transacting <- df %>% 
    filter(Type == "High Transacting Categories") %>% 
    select(MCC, Description)
  return(high_transacting)
}

# Apply the function to each data frame in the reversed list
high_transacting_details <- lapply(Merchant_UPI1, extract_high_transacting)

# Combine all high transacting categories and their descriptions into a single data frame
all_high_transacting_details <- do.call(rbind, high_transacting_details)

# Get unique categories and select one description for each category
unique_high_transacting <- all_high_transacting_details %>%
  group_by(MCC) %>%
  reframe(Description = first(Description))

# Get unique categories, their descriptions, and their frequencies
summary_df_high_desc<- all_high_transacting_details %>%
  group_by(MCC) %>%
  summarise(count = n(), .groups = 'drop') %>%
  inner_join(unique_high_transacting, by = "MCC") %>%
  arrange(desc(count))
View(summary_df_desc)
#Plot
# Load necessary libraries
library(viridis)
ggplot(summary_df_high_desc, aes(x = reorder(MCC, -count), y = count)) +
  geom_bar(stat = "identity", aes(fill = MCC), show.legend = FALSE) +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "High Transacting Merchant Categories Over Time",
       subtitle = "Based on 22 months of transaction data",
       x = "Merchant Category",
       y = "Number of Times High Transacting") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, size = 20,family = "Economica"),
    plot.subtitle = element_text(hjust = 0, size = 15,family = "Economica"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
#####Selecting specific tables by checking if they have "Remitter Bank" in them"####
k<-as.numeric((capture.output(
  for (i in 1:length(df)) 
{
  if((colnames(df[[3]])[2])==(colnames(df[[i]])[2]))
    print(i)
}
) ))

#####Selecting Apps data####
colnames(df[[1]])[2]
l<-as.numeric(str_sub(capture.output(
  for (i in 1:length(df)) 
  {
    if(colnames(df[[1]])[2]==colnames(df[[i]])[2])
      print(i)
    
  }
),5 ))
#####Storing the remit bank data####
Remit_banks<-df[k]
library(writexl)
writexl::write_xlsx(Remit_banks,col_names = TRUE,path = "UPI Remitter Banks.xlsx")
Remit_banks[[1]]<-Remit_banks[[1]][-1,]
Remit_banks[[30]]
for (i in 1:51) 
{
  colnames(Remit_banks[[i]])[2]="UPI Remitter Banks"
  
}

Apps_upi<-df[l]
colnames(Remit_banks[[1]])
#####
for (i in 2:length(Remit_banks)) 
{
  if((Remit_banks[[i]])[1,1]==Remit_banks[[1]][1,1])
    Remit_banks[[i]]<-Remit_banks[[i]][-1,]
}
View(Remit_banks[[1]])
Remit_banks[[1]]
k<-transpose(Remit_banks[[1]])
#####Closing the server#####
remDr$close()
rD$server$stop()
rm(rD, remDr)
gc()

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
Remit_banks[[51]]

