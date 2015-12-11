#sudo apt-get install r-cran-rjava
install.packages("xlsx", dep = T);      # If you have not installed it before
install.packages("plyr");
install.packages("gsubfn");
install.packages("foreign");
install.packages("XML");
install.packages("kulife");


options(java.parameters = "- Xmx2048m")
library(xlsx);
library(plyr);
library(gsubfn)
library(kulife)
library(foreign)

#################
#     Loans     #
#################

# Load
loans            <- read.xlsx("Data/banking.xlsx", sheetIndex = 5)
# Prefix ("Loan_")
colnames(loans)  <- paste("Loan", colnames(loans), sep = "_");
# Treat "Loan_Date" & "Loan_Status" (Good or Bad?)
loans$Loan_date  <- as.Date(as.character(loans$Loan_date) , "%y%m%d")
loans$Loan_status<- as.character(loans$Loan_status);
loans$Loan_status<- ifelse(test = ( loans$Loan_status == "C" ),"Good",loans$Loan_status)
loans$Loan_status<- ifelse(test = ( loans$Loan_status == "A" ),"Good", "Bad")

##################
#Loans - Accounts#
##################
accounts               <- read.xlsx("Data/banking.xlsx", sheetIndex = 1);
accounts$date          <- as.Date(as.character(accounts$date),"%y%m%d");
colnames(accounts)     <- paste("Account", colnames(accounts), sep = "_");
loans   <- merge(accounts,loans, by.x = "Account_account_id",by.y = "Loan_account_id");
loans$Account_clientRelationAge <- as.numeric(difftime(loans$Loan_date,loans$Account_date,units = "weeks")/52.25)
remove(accounts);
####################
# Loans - District #
####################
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
names(districts)[names(districts) == 'A2']  <- 'name';
names(districts)[names(districts) == 'A3']  <- 'region';
names(districts)[names(districts) == 'A4']  <- 'no_inhabitants';
names(districts)[names(districts) == 'A5']  <- 'no_mun<499';
names(districts)[names(districts) == 'A6']  <- 'no_mun500-1999';
names(districts)[names(districts) == 'A7']  <- 'no_mun2000-9999';
names(districts)[names(districts) == 'A8']  <- 'no_mun2000-9999';
names(districts)[names(districts) == 'A9']  <- 'no_mun10000';
names(districts)[names(districts) == 'A10'] <- 'urban_inhabitants_ratio';
names(districts)[names(districts) == 'A11'] <- 'avg_salary';
names(districts)[names(districts) == 'A12'] <- 'unemploy_95';
names(districts)[names(districts) == 'A13'] <- 'unemploy_96';
names(districts)[names(districts) == 'A14'] <- 'unterpreneurs_1000';
names(districts)[names(districts) == 'A15'] <- 'crimes_95';
names(districts)[names(districts) == 'A16'] <- 'crimes_96';
colnames(districts) <- paste("Account_District", colnames(districts), sep = "_");
loans   <- merge(loans,districts,by.x = "Account_district_id" , by.y = "Account_District_A1");
remove(districts);
loans$district_id <- NULL;
#########################################################################################################

#######################
#   Loading Clients   #
#######################
clients <- read.xlsx("Data/banking.xlsx", sheetIndex = 2);
clients$birth_number <- as.character(clients$birth_number);
clients$gender <- ifelse(is.na(as.numeric(strapplyc(clients$birth_number,"\\d{2}([5-9]\\d)\\d{2}"))), "M", "F");
clients$birth_number <- ifelse(clients$gender == "F",paste(strapplyc(clients$birth_number,"^\\d{2}"),sprintf("%02d",(as.numeric(strapplyc(clients$birth_number,"\\d{2}([5-9]\\d)\\d{2}"))-50)),strapplyc(clients$birth_number,"\\d{2}$"),sep = ""),clients$birth_number);
clients$birth_number <- paste("19",clients$birth_number,sep = "")
clients$birth_date   <- as.Date(clients$birth_number , "%Y%m%d")
clients$birth_number <- NULL;
colnames(clients) <- paste("Client", colnames(clients), sep = "_");
##########################
# Clients - Dispositions #
##########################
dispositions  <- read.xlsx("Data/banking.xlsx", sheetIndex = 3);
colnames(dispositions) <- paste("Disposition", colnames(dispositions), sep = "_");
clients       <- merge(clients, dispositions,by.x = "Client_client_id",by.y = "Disposition_client_id");
remove(dispositions);
#########################
#  Client-Cred.Card     #
#########################
creditCards   <- read.xlsx("Data/banking.xlsx", sheetIndex = 6);
creditCards$issued <- as.Date(substr(as.character(creditCards$issued),1,7), "%y%m%d");
colnames(creditCards) <- paste("CreditCards", colnames(creditCards), sep = "_");
clients <- merge(clients, creditCards, by.x = "Disposition_disp_id", by.y = "CreditCards_disp_id", all = T);
remove(creditCards);

########################
#  Client - Districts  #
########################
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
names(districts)[names(districts) == 'A2']  <- 'name';
names(districts)[names(districts) == 'A3']  <- 'region';
names(districts)[names(districts) == 'A4']  <- 'no_inhabitants';
names(districts)[names(districts) == 'A5']  <- 'no_mun<499';
names(districts)[names(districts) == 'A6']  <- 'no_mun500-1999';
names(districts)[names(districts) == 'A7']  <- 'no_mun2000-9999';
names(districts)[names(districts) == 'A8']  <- 'no_mun2000-9999';
names(districts)[names(districts) == 'A9']  <- 'no_mun10000';
names(districts)[names(districts) == 'A10'] <- 'urban_inhabitants_ratio';
names(districts)[names(districts) == 'A11'] <- 'avg_salary';
names(districts)[names(districts) == 'A12'] <- 'unemploy_95';
names(districts)[names(districts) == 'A13'] <- 'unemploy_96';
names(districts)[names(districts) == 'A14'] <- 'unterpreneurs_1000';
names(districts)[names(districts) == 'A15'] <- 'crimes_95';
names(districts)[names(districts) == 'A16'] <- 'crimes_96';
colnames(districts) <- paste("Client_District", colnames(districts), sep = "_");
clients <- merge(clients, districts, by.x="Client_district_id", by.y = "Client_District_A1");
remove(districts)

##################################
#     Client - Loan -Cred Card   #
##################################
clients <- merge(clients,loans,by.x = "Disposition_account_id",by.y = "Account_account_id");
clients <- ddply(clients,"Disposition_account_id",.fun = function(clients){
  cards  <- subset(clients, clients$CreditCards_issued < clients$Loan_date);
  dispSex<- subset(clients,clients$Disposition_type == "DISPONENT")$Client_gender;
  if( is.null(nrow(cards)) || nrow(cards) == 0){
    as.data.frame(c(subset(clients,clients$Disposition_type == "OWNER"), OwnerCreditCard_type = "" , Disponent_Gender = dispSex));
  } else {
    as.data.frame(c(subset(clients,clients$Disposition_type == "OWNER"), OwnerCreditCard_type = as.character(cards$CreditCards_type), Disponent_Gender = dispSex))
  }
},.progress='text');
remove(loans);
clients$Loan_Client_Age <- as.numeric(difftime(clients$Loan_date,clients$Client_birth_date, units = "weeks")/52.25) 

########################
#     Transactions     #
########################
transactions  <- read.csv("Data/transaction.csv",TRUE,";");
transactions$Date_Temp <- format(as.Date(as.character(transactions$date) , "%y%m%d"),"%y%m")
transactions$date      <- as.Date(as.Date(as.character(transactions$date) , "%y%m%d"))
colnames(transactions) <- paste("Transaction", colnames(transactions), sep = "_");
clients <- merge(clients,transactions,by.x="Disposition_account_id",by.y="Transaction_account_id");
remove(transactions);

clients <- ddply(clients,"Loan_loan_id",.fun = function(loanSet){
  loans <- subset(loanSet,loanSet$Transaction_date < loanSet$Loan_date)
  index <- which.min(loanSet$Transaction_date);
  loans <- loans[-index,]


  sanctionsCount <- nrow(subset(loans,loans$Transaction_k_symbol == "sanction for negative balance"))
  loans <- ddply(loans,"Transaction_Date_Temp",.fun = function(trans) {
      result <- c(
          inflow          = sum (subset(trans,as.character(trans$Transaction_type ) == "credit")$Transaction_amount),
          outflow         = sum (subset(trans,as.character(trans$Transaction_type ) != "credit")$Transaction_amount),
          inflowCount     = nrow(subset(trans,as.character(trans$Transaction_type ) == "credit")),
          outflowCount    = nrow(subset(trans,as.character(trans$Transaction_type ) == "credit"))
      )
      result;
  })
  result <- c(
    Transaction_inflowDeviation    = stats::sd(loans$inflow),
    Transaction_outflowDeviation   = stats::sd(loans$outflow),
    Transaction_outflowAvg         = mean(loans$outflow),
    Transaction_inflowAvg          = mean(loans$inflow),
    Transaction_inflowCountAvg     = mean(loans$inflowCount),
    Transaction_outflowCountAvg    = mean(loans$outflowCount),
    Transaction_sanctionCount      = sanctionsCount
  )
  partial <- loanSet[1,];
  partial <- partial[, -grep("Transaction_", colnames(partial))]
  as.data.frame(c(result,partial))
},.progress='text')

################
# Payment Order#
################
payment_orders<- read.xlsx("Data/banking.xlsx", sheetIndex = 4)
colnames(payment_orders) <- paste("Orders", colnames(payment_orders), sep = "_");
clients <- merge(clients,payment_orders,by.x = "Disposition_account_id",by.y = "Orders_account_id")
remove(payment_orders);
clients <- ddply(clients,'Disposition_account_id',.fun = function(order){
  result <- c(
    Orders_hasHouseHold = nrow(subset(order,order$Orders_k_symbol == "household")),
    Orders_hasleasing   = nrow(subset(order,order$Orders_k_symbol == "hasLeasing")),
    Orders_hasLoan      = nrow(subset(order,order$Orders_k_symbol == "hasLoan")),
    Orders_hasInsurance = nrow(subset(order,order$Orders_k_symbol == "hasInsurance"))
  )
  partial <- order[1,];
  as.data.frame(c(result,partial))
},.progress='text');

clients$Disposition_account_id <- NULL;
clients$Disposition_disp_id <- NULL;
clients$Client_client_id <- NULL;
clients$CreditCards_card_id <- NULL;
clients$Loan_loan_id <-NULL;
clients$Orders_order_id <- NULL;
clients$Orders_bank_to <- NULL;
clients$Orders_account_to <- NULL;
clients$Orders_amount <- NULL;
clients$Loan_date <- NULL;
clients$Client_birth_date <- NULL;
write.csv(clients,"result.csv",row.names = FALSE);
nrow(subset(clients,clients$Loan_status == "good"))

