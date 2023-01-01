rm(list = ls())

library(dplyr)
library(ggplot2)
library(plotly)

data <- data.frame(month=integer(),balance=double(),monthly_contribution=double())

yearly_return_percent <- 8
monthly_contribution <- 300
initial_amount <- 10000

monthly_multiplier <- (1+yearly_return_percent/100)^(1/12)

for (monthly_contribution1 in c(100,200,300,400,500)){
  for (i in 0:(40*12)) {
    if (i==0){
      data <- data %>% add_row(month=i,balance=initial_amount,monthly_contribution=monthly_contribution1)
    }else{
      balance1 <- tail((data %>% filter(monthly_contribution==monthly_contribution1))$balance,n=1)*monthly_multiplier+monthly_contribution1
      data <- data %>% add_row(month=i,balance=balance1,monthly_contribution=monthly_contribution1)
    }
  }
}

data %>% arrange()

p <- ggplot(data,aes(x=month/12,y=balance,frame=monthly_contribution))+
  geom_line()

p <- ggplotly(p)
