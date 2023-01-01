library(data.table)
library(dplyr)
library(ggplot2)

spending <- readxl::read_excel("/Users/leeschmalz/Documents/personal/money/FI.xlsx",sheet="Spending")
monthly_spending <- ( round(sum(spending$amount) / as.numeric(max(spending$end_date)-min(spending$start_date)) * (365/12)))

rent <- 1740
electric <- 140
car_insurance_after_jan <- 116
lee_roth_contribution <- 230

lee_monthly_income <- 1859*2
nat_monthly_income <- 1600*2

nat_bank_acct <- 14000
nat_roth <- 12000
nat_btc <- 2183
nat_403b <- 6000

ring <- 7000
lee_bank_acct <- 4300
lee_crypto <- 22700
lee_brokerage <- 16153
lee_roth <- 36051
lee_401k <- 11815

expendable_accts <- sum(nat_bank_acct,nat_btc,lee_bank_acct,lee_brokerage)-rent+(lee_crypto/2)-ring #subtract last months rent
non_expendable_accts <- sum(lee_crypto,lee_roth,lee_401k,nat_403b,nat_roth)

available_money <- data.frame(date=as.Date(character()),
                              extra_monthly_savings=double(),
                              expendable_money=double())

start_date <- as.Date("2022-10-01")

for (extra in seq(0,1000,by=100)){
  available_money <- available_money %>% add_row(date=start_date,
                                                 extra_monthly_savings=extra,
                                                 expendable_money=expendable_accts)
}

num_months <- 14
for (month in 1:num_months){
  
  if ( year(ymd(start_date) %m+% months(month))==2023 ){
    car_insurance <- car_insurance_after_jan
    lee_roth_contribution <- 0
  }else{
    car_insurance <- 0
  }
  
  for (extra in seq(0,1000,by=100)){
    available_money1 <- available_money %>% filter(extra_monthly_savings==extra) %>% arrange(date)
    prev <- available_money1$expendable_money[length(available_money1$expendable_money)]
    
    available_money <- available_money %>% 
      add_row(date=ymd(start_date) %m+% months(month),
              extra_monthly_savings=extra,
              expendable_money=prev
              + lee_monthly_income
              + nat_monthly_income
              + extra
              - rent
              - electric
              - car_insurance_after_jan
              - monthly_spending
              - lee_roth_contribution
              )
  }
  
}

available_money <- available_money %>% as.data.frame()

p <- ggplot(available_money,aes(x=date,y=expendable_money,color=extra_monthly_savings,group=extra_monthly_savings)) +
  geom_hline(yintercept = 60000,linetype="dashed",color="darkgreen") +
  geom_hline(yintercept = 70000,linetype="dashed",color="darkgreen") +
  geom_hline(yintercept = 80000,linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-04-01"),linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-05-01"),linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-06-01"),linetype="dashed",color="darkgreen") +
  geom_line() +
  geom_point()

# library(plotly)
# 
# p <- ggplotly(p, height = 450, width = 1000 ) # Generate plotly plot with hover text
# htmlwidgets::saveWidget(p, 
#                         selfcontained = FALSE, 
#                         file = "./house.html") # Save plot
  
