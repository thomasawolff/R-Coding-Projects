
library(readr)
library(dplyr)
library(tidyr)


setwd('C:/Users/moose_m7y2ik3/Google Drive/R-Course-HTML-Notes')

retail <- read_csv('online_retail2.csv')

colnames(retail)


retail %>% select(Description,Country,Quantity) %>% group_by(Country) %>% 
  summarise(Total_Sales = sum(Quantity, na.rm=TRUE)) %>% arrange(desc(Total_Sales))

summary(retail)



Invoice_Value <- retail_clean %>% group_by(Country,Invoice) %>% 
  summarise(Total_Revenue =sum(Revenue,na_rm=TRUE))

average_bucket_value <- Invoice_Value %>% group_by(Country) %>% 
  summarise(Average_Bucket_Revenue=mean(Total_Revenue))

## Top 10

average_bucket_value %>% arrange(desc(Average_Bucket_Revenue)) %>% slice(1:10)

retail_clean <- read_csv('retail_Clean.csv')

## On average, how many items are there per an invoice

invoice_count <- retail_clean %>% group_by(Invoice) %>% summarise(count=n())
summary_stat <- invoice_count %>% summarise(mean_items = mean(count,na.rm = TRUE),
                                            median = median(count, na.rum = TRUE),
                                            IQR = IQR(count, na.rm = TRUE),
                                            sd = sd(count, na.rm = TRUE),
                                            min = min(count, na.rm = TRUE),
                                            max = max(count, na.rm = TRUE))

## Case when 

retail_clean %>% mutate(uk_or_not = case_when(Country == "United Kingdom"~TRUE,
                                              Country != "United Kingdom"~FALSE)) %>%
  group_by(uk_or_not) %>% summarise(count=n())


sales_data <- data.frame(skus = c("Shoes_red","Shoes_blue",
                                  "Shoes_yellow","Beach_ball",
                                  "Sandals_green","Sandals_pink"),
                         average_sales = c(50,60,90,120,110,150))


stocks_data <- data.frame(skus = c("Shoes_blue",
                                   "Shoes_yellow","Beach_ball","Paddle",
                                   "Swimming_suit"),
                          current_stock = c(100,150,5,85,70))

names(stocks_data)[1]<-"article"

sales_data %>% left_join(stocks_data, by=c("skus"="article"))

sales_data %>% inner_join(stocks_data,by=c("skus"="article"))

sales_data %>% anti_join(stocks_data, by=c("skus"="article"))

sales_data %>% full_join(stocks_data, by=c("skus"="article"))

retail_clean$Date <- as.Date(retail_clean$InvoiceDate)

sales_per_day_clean <- retail_clean %>% group_by(Description,Date) %>% 
  summarise(total_sales = sum(Quantity, na.rm = TRUE))


sales_per_day_clean <- sales_per_day_clean %>% filter(Description != "?")


## Pivot wider: Shape 1

sales_per_day_clean %>% pivot_wider(id_cols = Date,names_from=Description,
                                    values_from = total_sales,
                                    values_fill = 0)


## Pivot wider: Shape 2

time_series_like <- sales_per_day_clean %>% pivot_wider(id_cols = Description,names_from=Date,
                                    values_from = total_sales,
                                    values_fill = 0)


## Pivot longer

time_series_like %>% pivot_longer(cols = -Description,names_to = 'Date',
                                  values_to = 'total_sales')



retail_clean <- retail_clean %>% separate(col = InvoiceDate,
                                          into = c("date","time"),
                                          sep = " ")


quartiles <- seq(0.1,1,0.1)

retail_clean %>% group_by(Country) %>% summarise(percentile = quantile(Quantity,quartiles)) %>% 
  mutate(name=rep(paste0("percentile_",quartiles),length(unique(Country)))) %>%
  pivot_wider(id_cols = Country,names_from = name, values_from = percentile)

