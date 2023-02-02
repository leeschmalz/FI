library(data.table)
library(dplyr)
library(ggplot2)

path <- "/Users/leeschmalz/Documents/personal/money/"

transactions <- fread(paste0(path,"current_transaction.csv")) %>% 
  mutate(accountRef.id = as.character(accountRef.id))

transactions <- transactions %>% select(id,
                                        date,
                                        description,
                                        amount,
                                        accountRef.id,
                                        accountRef.name,
                                        accountRef.type,
                                        category.name)

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
  filter(accountRef.type == "CreditAccount" | 
           accountRef.name %like% "Venmo" | 
           (accountRef.name=="Nat Checking" & category.name!="Transfer" & category.name != "Paycheck")) %>%
  mutate(person = if_else(accountRef.name %like% "Lee","Lee","Nat"))

# remove paying off credit cards
transactions <- transactions %>% 
  filter(!(description %like% "ACH Deposit Internet transfer" & accountRef.name %like% "Apple Card")) %>% # apple card is different
  filter(category.name != "Credit Card Payment")

# remove transfer from venmo to bank
transactions <- transactions %>% filter(!(description %like% "Transfer To" & accountRef.name %like% "Venmo"))

# manual exemption
transactions <- transactions %>% filter(id !="68537869_1402764492_0")

View( transactions %>% filter(date > as.Date("2022-12-01")) )

sum((transactions %>% filter(date > as.Date("2022-12-01")))$amount)

fwrite(paste0(path,"current_transaction_processed.csv"))
