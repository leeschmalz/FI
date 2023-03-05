
# current money available
nat_bank_acct <- 14000
lee_bank_acct <- 23700 # including cash from PC
lee_bonus <- 9200
nat_money_from_dad <- 10000
lee_money_from_parents <- 7000

current_money <- sum(nat_bank_acct,
                        lee_bank_acct,
                        lee_bonus,
                        nat_money_from_dad,
                        lee_money_from_parents) - rent # pay one months extra rent


rent <- 1752
parking <- 200
electric <- 140
car_insurance <- 116 # already paid through june

lee_monthly_income <- 2300*2
nat_monthly_income <- 1600*2


data <- data.frame(date=as.Date(character()),
                  monthly_spending=double(),
                  expendable_money=double())

for (monthly_spending in seq(1800,3000,by=200)){
  dates <- seq.Date(as.Date("2023-03-01"),by="month",length.out = 10)
  monthly_increases <- c(0)
  for (date in dates[2:length(dates)]){
    if ( date<as.Date("2023-06-01") ){ 
      monthly_increase <- lee_monthly_income+nat_monthly_income-rent-parking-electric-monthly_spending
    }else{
      monthly_increase <- lee_monthly_income+nat_monthly_income-electric-car_insurance-monthly_spending 
    }
    monthly_increases <- c(monthly_increases,monthly_increase)
  }
  
  data1 <- data.frame(date=dates,
                       money_available=cumsum(monthly_increases)+current_money,
                       monthly_spending=rep(monthly_spending,10))
  
  data <- rbind(data,data1) 
  
}
data <- data %>% mutate(type = "no rent")


data2 <- data.frame(date=as.Date(character()),
                   monthly_spending=double(),
                   expendable_money=double())

for (monthly_spending in seq(1800,3000,by=200)){
  dates <- seq.Date(as.Date("2023-03-01"),by="month",length.out = 10)
  monthly_increases <- c(0)
  for (date in dates[2:length(dates)]){
    if ( date<as.Date("2023-06-01") ){ 
      monthly_increase <- lee_monthly_income+nat_monthly_income-rent-parking-electric-monthly_spending
    }else{
      monthly_increase <- lee_monthly_income+nat_monthly_income-electric-rent-parking-monthly_spending 
    }
    monthly_increases <- c(monthly_increases,monthly_increase)
  }
  
  data1 <- data.frame(date=dates,
                      money_available=cumsum(monthly_increases)+current_money,
                      monthly_spending=rep(monthly_spending,10))
  
  data2 <- rbind(data2,data1) 
  
}

data2 <- data2 %>% mutate(type = "rent")


p <- ggplot(data,aes(x=date,y=money_available,color=monthly_spending,shape=type,group=paste0(monthly_spending,type))) +
  geom_hline(yintercept = 60000,linetype="dashed",color="darkgreen") +
  geom_hline(yintercept = 70000,linetype="dashed",color="darkgreen") +
  geom_hline(yintercept = 80000,linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-06-01"),linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-07-01"),linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-08-01"),linetype="dashed",color="darkgreen") +
  geom_line() +
  geom_point() + 
  scale_colour_continuous(trans="reverse")


data <- rbind(data,data2)


p2 <- ggplot(data,aes(x=date,y=money_available,color=type,group=paste0(monthly_spending,type))) +
  geom_hline(yintercept = 60000,linetype="dashed",color="darkgreen") +
  geom_hline(yintercept = 70000,linetype="dashed",color="darkgreen") +
  geom_hline(yintercept = 80000,linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-06-01"),linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-07-01"),linetype="dashed",color="darkgreen") +
  geom_vline(xintercept = as.Date("2023-08-01"),linetype="dashed",color="darkgreen") +
  geom_line() +
  geom_point() 

