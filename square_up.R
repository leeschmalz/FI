library(data.table)
library(dplyr)
library(ggplot2)

project_path<-"/Users/leeschmalz/Documents/personal/money/"
start_date <- "2022-09-27"
outpath<-paste0(project_path,start_date,"_",Sys.Date())
dir.create(outpath)

transactions <- fread("/Users/leeschmalz/Documents/GitHub/FI/current_transaction.csv") %>% mutate(accountRef.id = as.character(accountRef.id))

transactions <- transactions %>% select(id,date,description,amount,accountRef.id,accountRef.name,accountRef.type,category.name)

account_id_map <- c("5879268"="Lee Apple Card",
                    "5890586"="Nat Capital One",
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
                    "4806916"="Lee Savings",
                    "4806984"="Lee Venmo",
                    "4806915"="Business Acct")

transactions <- transactions %>% 
  mutate(accountRef.name = account_id_map[accountRef.id]) %>%
  filter(accountRef.type == "CreditAccount" | accountRef.name %like% "Venmo" | (accountRef.name=="Nat Checking" & category.name!="Transfer" & category.name != "Paycheck")) %>%
  mutate(person = if_else(accountRef.name %like% "Lee","Lee","Nat"))

transactions <- transactions %>% filter(date > as.Date(start_date))

# remove paying off credit cards
transactions <- transactions %>% 
  filter(!(description %like% "ACH Deposit Internet transfer" & accountRef.name %like% "Apple Card")) %>% # apple card is different
  filter(category.name != "Credit Card Payment")

# remove transfer from venmo to bank
transactions <- transactions %>% filter(!(description %like% "Transfer To" & accountRef.name %like% "Venmo"))

# transactions that we are not splitting
exempt_transaction_ids <- c("68537869_1393323982_0", # ring band
                         "68537869_1393324013_0", # ring band
                         "68537869_1400260382_0", # lee plane ticket
                         "68537869_1400956354_0", # nat ireland ticket
                         "68537869_1400956367_0", # nat ireland ticket
                         "68537869_1393324019_0"  # diamond
                         ) 

exempt_transactions <- transactions %>% filter((id %in% exempt_transaction_ids)) 
transactions <- transactions %>% filter(!(id %in% exempt_transaction_ids))

fwrite(exempt_transactions,paste0(outpath,"/exempt_transactions.csv"))
fwrite(transactions,paste0(outpath,"/transactions.csv"))

# PLOT
transactions_plot <- transactions %>% 
  filter(amount<0) %>% #remove venmo and refunds from plots
  group_by(category.name) %>% 
  arrange(date) %>% 
  mutate(spent = cumsum(-amount)) %>%
  ungroup()

cats <- transactions_plot %>% group_by(category.name) %>% summarise(total=sum(amount)) %>% filter(-total>300)

p<-ggplot(transactions_plot %>% semi_join(cats) %>% filter(!(category.name %like% "Venmo")),aes(x=date,y=spent,color=category.name)) +
  geom_point() +
  geom_smooth(method="loess",se=F) +
  ggtitle(paste0(start_date," to ",Sys.Date()))+
  ylab("Amount")
ggsave(paste0(outpath,"/spending_over_time_by_category.png"),width = 12,height = 7)

p<-ggplot(transactions %>% arrange(date),aes(x=date,y=cumsum(-amount))) +
  geom_point() +
  geom_smooth(method="loess",se=F) +
  geom_hline(yintercept=-sum(transactions$amount),color="black",linetype="dashed") +
  geom_text(aes(x=Sys.Date()-10,y=-sum(transactions$amount),label=-sum(transactions$amount)),nudge_y = 150)+
  ggtitle(paste0(start_date," to ",Sys.Date())) +
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
  ggtitle(paste0(start_date," to ",Sys.Date()))+
  ylab("Amount")
ggsave(paste0(outpath,"/spending_bar.png"),width = 12,height = 7)

lee_spent <- -sum(filter(transactions,person=="Lee")$amount)
nat_spent <- -sum(filter(transactions,person=="Nat")$amount)

print(transactions %>% group_by(person) %>% summarise(total_spent = -sum(amount)))
if(lee_spent>nat_spent){print(paste0("Nat owes: ",(nat_spent-lee_spent) / 2))}
if(nat_spent>lee_spent){print(paste0("Lee owes: ",(nat_spent-lee_spent) / 2))}

