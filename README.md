# UPI_Analysis
This Repo Consists files regarding my final year Dissertation Project.
The list of folders is given as -
1. Codes - Contains the Main codes which are used in the markdown file , these contain earlier versions of the code so not all of them are uodated as per the markdown file.  
2. Data Non Scraped - Data which has been directly downloaded from some source website for ease of access includes RBI daily settlement data,Monthly Upi data , P2P &P2M payment category analysis data over time.
The `Time_series_anal.Rmd` file contains  the main reproducible  Rmarkdown code for the prroject.
the images in img1.png and Rplot02.png have been created using `knitr::kable` and `kableExtra` i am sharing the code t o reproduce these since i find them really useful.
```{r}
text_tbl <- data.frame(Method = c("AR", "MA", "ARMA","ARIMA","SARIMA"),
Full_Name = c("Auto Regressive","Moving Average",
              "Auto Regressive Moving Average","Auto Regressive Integrated Moving Average",
              "Seasonal Auto Regressive Integrated Moving Average")
kbl(text_tbl) %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "30em", background = "lightblue")
```
#### Will keep updating
