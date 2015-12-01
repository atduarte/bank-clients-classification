options(java.parameters = "- Xmx1024m")
install.packages("xlsx", dep = T)      # If you have not installed it before
library(xlsx)



transactions  <- read.csv("Data/transaction.csv",TRUE,";")
accounts      <- read.xlsx("Data/banking.xlsx", sheetIndex = 1)
clients       <- read.xlsx("Data/banking.xlsx", sheetIndex = 2)
dispositions  <- read.xlsx("Data/banking.xlsx", sheetIndex = 3)
payment_orders<- read.xlsx("Data/banking.xlsx", sheetIndex = 4)
loans         <- read.xlsx("Data/banking.xlsx", sheetIndex = 5)
creditCards   <- read.xlsx("Data/banking.xlsx", sheetIndex = 6)
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7)


accounts_transactions <- merge(accounts,transactions,"transaction_id")


#clients <- merge(clients,dispositions,"client_id")
#remove(dispositions)

#clients <- merge(clients,creditCards,"disp_id")
#remove(creditCards)

#clients <- merge(clients,districts, by.x = "district_id" , by.y = "A1")
#remove(districts)

#clients <- merge(clients,accounts,"account_id")
#remove(accounts)

#clients <- merge(clients,loans,"account_id")
#remove(loans)

#clients <- merge(clients,payment_orders,"account_id")
#remove(payment_orders)

#clients <- merge(clients,transactions,"account_id")
#remove(transactions)