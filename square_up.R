library(data.table)
library(dplyr)
library(ggplot2)

project_path<-"/Users/leeschmalz/Documents/personal/money/"
start_date <- "2023-02-01"
end_date <- "2023-03-01"
outpath<-paste0(project_path,start_date,"_",end_date)
dir.create(outpath)

transactions <- fread("/Users/leeschmalz/Documents/personal/money/current_transaction.csv") %>% mutate(accountRef.id = as.character(accountRef.id))

transactions <- transactions %>% select(id,date,description,amount,accountRef.id,accountRef.name,accountRef.type,category.name)

account_id_map <- c("5879268"="Lee Apple Card",
                    "5890586"="Nat Capital One",
                    "5915178"="Nat Chase",
                    "4806917"="Lee Checking",
                    "5890571"="Nat Checking",
                    "5890570"="Nat Savings",
                    "5890572"="Nat Wells Fargo Credit",
                    "5593652"="Lee Capital One",
                    "5890563"="Nat Apple Card",
                    "5890591"="Nat Venmo",
                    "4905677"="Lee Venmo",
                    "4806935"="Lee Visa",
                    "5879269"="Lee Chase",
                    "5903399"="Lee Chase",
                    "4806916"="Lee Savings",
                    "4806984"="Lee Venmo",
                    "4806915"="Business Acct")

transactions <- transactions %>% filter(date >= as.Date(start_date),
                                        date < as.Date(end_date))

transactions_orginal <- transactions

transactions <- transactions %>% 
  mutate(accountRef.name = account_id_map[accountRef.id]) %>%
  filter(accountRef.type == "CreditAccount" | accountRef.name %like% "Venmo" | (accountRef.name=="Nat Checking" & category.name!="Transfer" & category.name != "Paycheck")) %>%
  mutate(person = if_else(accountRef.name %like% "Lee","Lee","Nat"))

# remove paying off credit cards
transactions <- transactions %>% 
  filter(!(description %like% "ACH Deposit Internet transfer" & accountRef.name %like% "Apple Card")) %>% # apple card is different
  filter(category.name != "Credit Card Payment")

# remove transfer from venmo to bank
transactions <- transactions %>% filter(!(description %like% "Transfer To" & accountRef.name %like% "Venmo"))

# transactions that we are not splitting, not included in any reporting
exempt_transaction_ids <- c("68537869_1393323982_0", # ring band
                         "68537869_1393324013_0", # ring band
                         "68537869_1400260382_0", # lee plane ticket
                         "68537869_1400956354_0", # nat ireland ticket
                         "68537869_1400956367_0", # nat ireland ticket
                         "68537869_1393324019_0",  # diamond
                         "68537869_1419840779_0", # lee used at work, reimbursed
                         "68537869_1419840780_0", # lee jimmy johns at work, reimbursed
                         "68537869_1407463243_0", # 1 password
                         "68537869_1405898375_0", # spotify
                         "68537869_1405898374_0", # spotify
                         "68537869_1405898372_0", # spotify
                         "68537869_1412822427_0", #spotify
                         "68537869_1422165128_0", # wedding photo
                         "68537869_1422165127_0", # wedding airbnb
                         "68537869_1429579659_0", # GPU refund
                         "68537869_1431291705_0", # spotify
                         "68537869_1423427848_0"  # spotify
                         ) 

# these are not exempt, just reported separately
travel <- c("68537869_1426202197_0",
            "68537869_1423997861_0",
            "68537869_1426441620_0",
            "68537869_1426441618_0")

# we still split these, but dont include in monthly totals
exclude_from_total_spent <- c("68537869_1407463289_0", # electric
                              "68537869_1409284776_0", # car insurance
                              "68537869_1431291706_0" # electric
                              )

transactions$amount[which(transactions$description=="SUMMER HOUSE SANTAMONICA")] <- transactions$amount[which(transactions$description=="SUMMER HOUSE SANTAMONICA")] / 3

exempt_transactions <- transactions %>% filter((id %in% exempt_transaction_ids)) 
transactions <- transactions %>% filter(!(id %in% exempt_transaction_ids))
removed_transactions <- transactions_orginal %>% anti_join(transactions,by="id")

fwrite(exempt_transactions,paste0(outpath,"/exempt_transactions.csv"))
fwrite(removed_transactions,paste0(outpath,"/all_removed_records.csv"))
fwrite(transactions,paste0(outpath,"/transactions.csv"))
fwrite(filter(transactions,(id %in% travel)),paste0(outpath,"/travel_transactions.csv"))

# PLOT
transactions_plot <- transactions %>% 
  filter(amount<0) %>% #remove venmo and refunds from plots
  filter(!(id %in% exclude_from_total_spent)) %>%
  filter(!(id %in% travel)) %>%
  group_by(category.name) %>% 
  arrange(date) %>% 
  mutate(spent = cumsum(-amount)) %>%
  ungroup()

cats <- transactions_plot %>% group_by(category.name) %>% summarise(total=sum(amount)) %>% filter(-total>300) 

p<-ggplot(transactions_plot %>% semi_join(cats) %>% filter(!(category.name %like% "Venmo")),aes(x=date,y=spent,color=category.name)) +
  geom_point() +
  geom_smooth(method="lm",se=F,alpha=0.5) +
  ggtitle(paste0(start_date," to ",as.Date(end_date)))+
  ylab("Amount")
ggsave(paste0(outpath,"/spending_over_time_by_category.png"),width = 12,height = 7)

transactions1 <- transactions %>% filter(!(id %in% travel)) %>% filter(!(id %in% exclude_from_total_spent))
p<-ggplot(transactions1 %>% arrange(date),aes(x=date,y=cumsum(-amount))) +
  geom_point() +
  geom_smooth(method="loess",se=F) +
  geom_hline(yintercept=-sum(transactions1$amount),color="black",linetype="dashed") +
  geom_text(aes(x=as.Date(end_date)-10,y=-sum(transactions1$amount),label=round(-sum(transactions1$amount),2) ),nudge_y = 150) +
  ggtitle(paste0(start_date," to ",end_date," Excluding Travel and Monthly Bills")) +
  ylab("Amount")
ggsave(paste0(outpath,"/spending_over_time.png"),width = 12,height = 7)

high_to_low <- arrange(transactions_plot%>%group_by(category.name)%>%summarise(amount=sum(-amount)),-amount)$category.name
transactions_plot$category.name <- factor(transactions_plot$category.name,levels = high_to_low)
transactions_plot$amount[which(transactions_plot$id=="68537869_1400951796_0")] <- transactions_plot$amount[which(transactions_plot$id=="68537869_1400951796_0")]+0.01
transactions_plot$month <- format(as.Date(transactions_plot$date, format="%d/%m/%Y"),"%m")

p<-ggplot(transactions_plot,aes(x=category.name,y=-amount,fill=month)) +
  geom_bar(stat = "sum",na.rm=T) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(size = "none") +
  ggtitle(paste0(start_date," to ",as.Date(end_date)))+
  ylab("Amount")
ggsave(paste0(outpath,"/spending_bar.png"),width = 12,height = 7)

lee_spent <- -sum(filter(transactions,person=="Lee")$amount)
nat_spent <- -sum(filter(transactions,person=="Nat")$amount)

print(transactions %>% group_by(person) %>% summarise(total_spent = -sum(amount)))
if(lee_spent>nat_spent){print(paste0("Nat owes: ",(nat_spent-lee_spent) / 2))}
if(nat_spent>lee_spent){print(paste0("Lee owes: ",(nat_spent-lee_spent) / 2))}


print("discretionary spending:")
print(-sum(filter(transactions,!(id %in% exclude_from_total_spent) & !(id %in% travel))$amount))
                              
print("travel")
print(-sum(filter(transactions,(id %in% travel))$amount))

