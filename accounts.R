library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

rent <- 1700
parking <- 200
electric <- 130
car_insurance <- 116

fixed_spending <- rent*12+parking*12+electric*12+car_insurance*12

spending <- readxl::read_excel("/Users/leeschmalz/Documents/personal/money/FI.xlsx",sheet="Spending")

# filter to last square up
spending <- spending %>% 
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date)) %>% 
  filter(end_date <= as.Date("2022-09-25"))

# add estimated spending date based on num transactions and start / end date
spending <- spending %>% 
  group_by(start_date,end_date,name) %>%
  mutate(num_transactions = n(),
         transaction_num = row_number()) %>%
  ungroup() %>%
  mutate(interval = (end_date-start_date)/num_transactions) %>%
  mutate(estimated_day = start_date + interval*transaction_num,
         estimated_week = week(estimated_day),
         estimated_month = month(estimated_day)) %>%
  select(-interval,-transaction_num,-num_transactions) %>%
  arrange(estimated_day) %>%
  mutate(cumulative_spent = cumsum(amount)) %>%
  mutate(transaction_summary = paste0(type," $",amount))

# avg per month assumes 1700 rent, 200 parking, car insurance 116, 100 electric
avg_monthly_spending <- ( round(sum(spending$amount) / as.numeric(max(spending$end_date)-min(spending$start_date)) * (365/12)) + 
  rent + 
  parking +
  car_insurance +
  electric)

avg_yearly_spending <- avg_monthly_spending * 12

p <- ggplot(spending,aes(x=estimated_day,y=cumulative_spent,label=transaction_summary)) +
  geom_smooth(color="grey") +
  geom_point(size=1.1) +
  geom_line()+
  labs(title = paste0("Cumulative Spending - avg yearly spending: $",avg_yearly_spending),
       subtitle = paste0("assumes\nrent: $",rent,"\nparking: $",parking,"\ncar insurance: $",car_insurance,"\nelectric: $",electric))

ggplotly(p)



spending_by_name <- spending %>% 
  filter(split=="N") %>% group_by(name) %>% mutate(cumulative_spent = cumsum(amount))

p <- ggplot(spending_by_name,aes(x=estimated_day,y=cumulative_spent,group=name,color=name,label=transaction_summary)) +
  geom_smooth(color="grey") +
  geom_point(size=1.1) +
  geom_line() +
  ggtitle("Split = \"N\" category spending by person")

ggplotly(p)

library(data.table)

# categories by month percent
categories_month <- spending %>% 
  mutate(category = if_else(type %ilike% "marianos","Marianos",category)) %>%
  mutate(category = if_else(type %ilike% "starbucks","Starbucks",category)) %>%
  group_by(estimated_month,category) %>%
  summarise(category_total = sum(amount)) %>%
  group_by(estimated_month) %>%
  mutate(monthly_total = sum(category_total)) %>%
  ungroup() %>%
  mutate(percent = category_total/monthly_total)

categories_month <- categories_month %>% mutate(month_name = month.abb[estimated_month])
categories_month$month_name <- factor(categories_month$month_name,levels=c("Apr","May","Jun","Jul","Aug","Sep"))


ggplot(categories_month,aes(x=month_name,y=percent,fill=category)) +
  geom_bar(stat="identity") +
  xlab("Month") +
  ggtitle("Monthly Spending by Category")
  


# categories by month total
categories_month_total <- spending %>% 
  group_by(estimated_month,category) %>%
  summarise(category_total = sum(amount)) %>%
  arrange(estimated_month)
categories_month_total <- categories_month_total %>% mutate(month_name = month.abb[estimated_month])

categories_month_total$month_name <- factor(categories_month_total$month_name,levels=c("Apr","May","Jun","Jul","Aug","Sep"))

ggplot(categories_month_total,aes(x=month_name,y=category_total,fill=category)) +
  geom_bar(stat="identity") +
  xlab("Month") +
  ggtitle("Monthly Spending by Category")
  