#sudo apt-get install r-cran-rjava
install.packages("xlsx", dep = T);      # If you have not installed it before
install.packages("plyr");
install.packages("gsubfn");

options(java.parameters = "- Xmx2048m")

library(xlsx);
library(plyr);
library(gsubfn)

#################
#Loading   Loans#
#################
loans            <- read.xlsx("Data/banking.xlsx", sheetIndex = 5)
colnames(loans)  <- paste("Loan", colnames(loans), sep = "_");
loans$Loan_date  <- as.Date(as.character(loans$Loan_date) , "%y%m%d")
loans$Loan_status<- ifelse(test = ( loans$Loan_status == "C" ),"Good",loans$Loan_status)
loans$Loan_status<- ifelse(test = ( loans$Loan_status == "A" ),"Good", "Bad")
##################
#Loans - Accounts#
##################
accounts               <- read.xlsx("Data/banking.xlsx", sheetIndex = 1);
accounts$date          <- as.Date(as.character(accounts$date),"%y%m%d");
colnames(accounts)     <- paste("Account", colnames(accounts), sep = "_");
accounts$Account_date  <- as.Date(as.character(accounts$Account_date) , "%y%m%d")
loans   <- merge(accounts,loans, by.x = "Account_account_id",by.y = "Loan_account_id");
loans$Account_clientRelationAge <- as.numeric(difftime(loans$Loan_date,loans$Account_date,units = "weeks")/52.25)
remove(accounts);
####################
# Loans - District #
####################
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
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
colnames(creditCards) <- paste("CreditCards", colnames(creditCards), sep = "_");
clients <- merge(clients, creditCards, by.x = "Disposition_disp_id", by.y = "CreditCards_disp_id", all = T);
remove(creditCards);

########################
#  Client - Districts  #
########################
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
colnames(districts) <- paste("Client", colnames(districts), sep = "_");
clients <- merge(clients, districts, by.x="Client_district_id", by.y = "Client_A1");
remove(districts)

########################
#      - Districts     #
########################
transactions  <- read.csv("Data/transaction.csv",TRUE,";")
transactions$Date_Temp <- format(as.Date(as.character(transactions$date) , "%y%m%d"),"%y%m")
colnames(transactions) <- paste("Transaction", colnames(transactions), sep = "_");
loans <- merge(loans,transactions,by.x="Account_account_id",by.y="Transaction_account_id");
remove(transactions);
loans_old <- loans;

loans <- loans_old;
abc <- ddply(loans,"Loan_loan_id",.fun = function(loanSet){
  #loans <- subset(loanSet,loanSet$Loan_Account_Date < loanSet$Loan_Date)
  loans <-loanSet;
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
    inflowDeviation    = stats::sd(loans$inflow),
    outflowDeviation   = stats::sd(loans$outflow),
    inflowCountAvg     = mean(loans$inflowCount),
    outflowCountAvg    = mean(loans$outflowCount)
  )
  c(result,loanSet[1,])
},.progress='text')


dispositions   <- ddply(dispositions_,.variables = "account_id",.fun = function(piece){
  newPiece <- subset(piece,piece$type == "OWNER");
  newPiece$disp_count <- length(piece$type);
  newPiece
});


dispositions$type <- NULL;
remove(dispositions_);
accumulator <- merge(accumulator,dispositions,"account_id");
remove(dispositions);

clients        <- read.xlsx("Data/banking.xlsx", sheetIndex = 2);
accumulator <- merge(accumulator,clients,"client_id");
remove(clients);


abc <- merge(accumulator,creditCards,"disp_id");


colnames(creditCards)[colnames(creditCards)=="type"] <- "credit_card_type";
accumulator   <- merge(accumulator,creditCards,"disp_id");





write.csv(accumulator,"result.csv",row.names = FALSE);




#payment_orders<- read.xlsx("Data/banking.xlsx", sheetIndex = 4)
#creditCards   <- read.xlsx("Data/banking.xlsx", sheetIndex = 6)
#districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7)


#accounts_transactions <- merge(accounts,transactions,"account_id")
#accounts_transactions_ <- aggregate(accounts_transactions,by=accounts_transactions,FUN=mean,na.rm=TRUE)

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
