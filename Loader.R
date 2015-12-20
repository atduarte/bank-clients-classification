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


loans_csv = TRUE;
loans_path = "Data/banking-test-loans-public.csv"
transaction_csv = TRUE;
transaction_path = "Data/banking-transaction-test.csv";

payment_order_csv = TRUE;
payment_order_path= "Data/banking-test-payments.csv";

#################
#     Loans     #
#################
# Load
if(loans_csv){
  loans <- read.csv(loans_path,header = TRUE) 
} else {
  loans <- read.xlsx(loans_path, sheetIndex = 5)
}
# Prefix ("Loan_")
colnames(loans)  <- paste("Loan", colnames(loans), sep = "_");
loans$Loan_date  <- as.character(loans$Loan_date); #Remove
loans$Loan_date  <- ifelse( test = (nchar(loans$Loan_date) == 3) , paste("20000",loans$Loan_date,sep = "") , paste("2000",loans$Loan_date,sep = "")); #Remove
loans$Loan_date  <- as.Date(as.character(loans$Loan_date) , "%Y%m%d") # Change Y to y


loans$Loan_status     <- as.character(loans$Loan_status);
loans$Loan_Goodness   <- loans$Loan_status
loans$Loan_Goodness   <- ifelse(test = ( loans$Loan_Goodness == "C" )   ,"Good",loans$Loan_Goodness)
loans$Loan_Goodness   <- ifelse(test = ( loans$Loan_Goodness == "A" )   ,"Good",loans$Loan_Goodness)
loans$Loan_Goodness   <- ifelse(test = ( loans$Loan_Goodness == "Good" ),"Good","Bad")

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
clients_ <- clients;
clients <- clients;
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
clients <- ddply(clients,"Disposition_account_id",.fun = function(clients){
  cards       <- subset(clients, clients$CreditCards_issued < clients$Loan_date);
  currentCard <- "NA";
  if(nrow(cards) > 0){
    currentCard <- as.character(cards$CreditCards_type);
  }
  dispSex<- subset(clients,clients$Disposition_type == "DISPONENT")$Client_gender;
  if( is.null(nrow(cards)) || nrow(cards) == 0){
    as.data.frame(c(subset(clients,clients$Disposition_type == "OWNER"), OwnerCreditCard_type = "" , Disponent_Gender = dispSex, OwnerCurrentCreditCard_type = currentCard));
  } else {
    as.data.frame(c(subset(clients,clients$Disposition_type == "OWNER"), OwnerCreditCard_type = as.character(cards$CreditCards_type), Disponent_Gender = dispSex, OwnerCurrentCreditCard_type = currentCard))
  }
},.progress='text');
clients <- merge(clients,loans,by.x = "Disposition_account_id",by.y = "Account_account_id");

remove(loans);
clients$Loan_Client_Age <- as.numeric(difftime(clients$Loan_date,clients$Client_birth_date, units = "weeks")/52.25) 

########################
#     Transactions     #
########################
if(transaction_csv){
   transactions <- read.csv(transaction_path,header = TRUE)
} else {
  transactions  <- read.csv(transaction_path,TRUE,";");  
}
clients_ <- clients;
clients <- clients_;
transactions$Date_Temp <- format(as.Date(as.character(transactions$date) , "%y%m%d"),"%y%m")
transactions$date      <- as.Date(as.Date(as.character(transactions$date) , "%y%m%d"))
colnames(transactions) <- paste("Transaction", colnames(transactions), sep = "_");
clients <- merge(clients,transactions,by.x="Disposition_account_id",by.y="Transaction_account_id");
remove(transactions);


clients <- ddply(clients,"Loan_loan_id",.fun = function(loanSet){
  loans    <- loanSet[-which.min(loanSet$Transaction_date),] 
  loanspre <- subset(loans,loans$Transaction_date < loans$Loan_date)
  loanspos <- subset(loans,loans$Transaction_date < loans$Loan_date)
  
  loans <- loanSet;
  sanctionsCount     <- nrow(subset(loans,loans$Transaction_k_symbol == "sanction for negative balance"))
  sanctionsCountPre  <- nrow(subset(loanspre,loanspre$Transaction_k_symbol == "sanction for negative balance"))
  sanctionsCountPos  <- nrow(subset(loanspos,loanspos$Transaction_k_symbol == "sanction for negative balance"))
  flowCalc <- function(trans) {
    result <- c(
      inflow          = sum (subset(trans,as.character(trans$Transaction_type ) == "credit")$Transaction_amount),
      outflow         = sum (subset(trans,as.character(trans$Transaction_type ) != "credit")$Transaction_amount),
      inflowCount     = nrow(subset(trans,as.character(trans$Transaction_type ) == "credit")),
      outflowCount    = nrow(subset(trans,as.character(trans$Transaction_type ) == "credit"))
    )
    result;
  }
  
  #loansPre <- ddply(loansPre,"Transaction_Date_Temp",.fun = flowCalc)
  #loansPos <- ddply(loansPos,"Transaction_Date_Temp",.fun = flowCalc)
  loansStats    <- ddply(loans,"Transaction_Date_Temp",.fun = flowCalc)
  loansStatspre <- ddply(loanspre,"Transaction_Date_Temp",.fun = flowCalc)
  loansStatspos <- ddply(loanspos,"Transaction_Date_Temp",.fun = flowCalc)
  result <- c(
    Transaction_Total_inflowDeviation    = stats::sd(loansStats$inflow),
    Transaction_Total_outflowDeviation   = stats::sd(loansStats$outflow),
    Transaction_Total_outflowAvg         = mean(loansStats$outflow),
    Transaction_Total_inflowAvg          = mean(loansStats$inflow),
    Transaction_Total_inflowCountAvg     = mean(loansStats$inflowCount),
    Transaction_Total_outflowCountAvg    = mean(loansStats$outflowCount),
    Transaction_Total_sanctionCount      = sanctionsCount,
    
    Transaction_Pre_inflowDeviation    = stats::sd(loansStatspre$inflow),
    Transaction_Pre_outflowDeviation   = stats::sd(loansStatspre$outflow),
    Transaction_Pre_outflowAvg         = mean(loansStatspre$outflow),
    Transaction_Pre_inflowAvg          = mean(loansStatspre$inflow),
    Transaction_Pre_inflowCountAvg     = mean(loansStatspre$inflowCount),
    Transaction_Pre_outflowCountAvg    = mean(loansStatspre$outflowCount),
    Transaction_Pre_sanctionCount      = sanctionsCountPre,
    
    Transaction_Pos_inflowDeviation    = stats::sd(loansStatspos$inflow),
    Transaction_Pos_outflowDeviation   = stats::sd(loansStatspos$outflow),
    Transaction_Pos_outflowAvg         = mean(loansStatspos$outflow),
    Transaction_Pos_inflowAvg          = mean(loansStatspos$inflow),
    Transaction_Pos_inflowCountAvg     = mean(loansStatspos$inflowCount),
    Transaction_Pos_outflowCountAvg    = mean(loansStatspos$outflowCount),
    Transaction_Pos_sanctionCount      = sanctionsCountPos
  )
  partial <- loans[1,];
  partial <- partial[, -grep("Transaction_", colnames(partial))]
  as.data.frame(c(result,partial))
},.progress='text')

################
# Payment Order#
################
if(payment_order_csv){
  payment_orders <- read.csv(payment_order_path,header = TRUE)
} else {
  payment_orders<- read.xlsx(payment_order_path, sheetIndex = 4)
}
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
clients$Orders_order_id <- NULL;
clients$Orders_bank_to <- NULL;
clients$Orders_account_to <- NULL;
clients$Orders_amount <- NULL;
clients$Loan_date <- NULL;
clients$Client_birth_date <- NULL;
clients$CreditCards_type <- NULL;
clients$CreditCards_issued <- NULL;

clients$Disponent_Gender  <- ifelse(is.na(clients$Disponent_Gender),"ND",as.character(clients$Disponent_Gender))
clients$Orders_k_symbol   <- ifelse(as.character(clients$Orders_k_symbol) == " ","ND",as.character(clients$Orders_k_symbol));
clients$OwnerCreditCard_type <- ifelse(as.character(clients$OwnerCreditCard_type) == " ", "ND", as.character(clients$CreditCards_type));
                                       
                                       
write.csv(clients,"result.csv",row.names = FALSE);
nrow(subset(clients,clients$Loan_Goodness == "Good"))
nrow(subset(clients,clients$Loan_Goodness == "Bad"))


#######################
#     Decriptive DATA #
#######################

clients       <- read.xlsx("Data/banking.xlsx", sheetIndex = 2);
dispositions  <- read.xlsx("Data/banking.xlsx", sheetIndex = 3);
creditCards   <- read.xlsx("Data/banking.xlsx", sheetIndex = 6);
loans         <- read.xlsx("Data/banking.xlsx", sheetIndex = 5);
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex = 7);
clients$gender <- ifelse(is.na(as.numeric(strapplyc(clients$birth_number,"\\d{2}([5-9]\\d)\\d{2}"))), "M", "F");

colnames(clients)      <- paste("Client"     , colnames(clients), sep = "_");
colnames(dispositions) <- paste("Disposition", colnames(dispositions), sep = "_");
colnames(creditCards)  <- paste("CreditCard" , colnames(creditCards), sep = "_");
colnames(loans)        <- paste("Loans"     , colnames(loans), sep = "_");names(districts)[names(districts) == 'A2']  <- 'name';
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
colnames(districts)    <- paste("District", colnames(districts), sep = "_");


clients_ <- merge(clients,dispositions,by.x="Client_client_id"   ,by.y = "Disposition_client_id");
clients_ <- merge(clients_,creditCards,by.x="Disposition_disp_id",by.y="CreditCard_disp_id", all = T);
clients_ <- merge(clients_,loans,by.x="Disposition_account_id",by.y="Loans_account_id", all = T);
clients_ <- merge(clients_,districts,"Client_district_id",by.y="District_A1");

clients_$Loans_amount   <- ifelse(is.na(clients_$Loans_amount),0,clients_$Loans_amount);
clients_$Loans_duration <- ifelse(is.na(clients_$Loans_duration),0,clients_$Loans_duration);
clients_$Loans_payments <- ifelse(is.na(clients_$Loans_payments),0,clients_$Loans_payments);
clients_$hasCreditCard  <- is.na(clients_$CreditCard_card_id);
clients_$hasLoan        <- is.na(clients_$Loans_loan_id);
clients_$Disposition_disp_id <- NULL;
clients_$Disposition_account_id <- NULL;
clients_$Client_client_id <- NULL;
clients_$CreditCard_card_id <- NULL;
clients_$Loans_loan_id <- NULL;
clients_$Loans_date <- NULL;
clients_$Loans_status <- NULL;
clients_$CreditCard_issued <- NULL;
write.csv(clients_,"result_desc.csv",row.names = FALSE);
