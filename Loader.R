#sudo apt-get install r-cran-rjava
install.packages("xlsx", dep = T);      # If you have not installed it before
install.packages("plyr");
install.packages("gsubfn");

options(java.parameters = "- Xmx1024m")

library(xlsx);
library(plyr);
library(gsubfn)

#################
#Loading   Loans#
#################
loans         <- read.xlsx("Data/banking.xlsx", sheetIndex = 5)
loans$date    <- as.Date(as.character(loans$date) , "%y%m%d")
loans$status  <- ifelse(test = ( loans$status == "C" ),"Good",loans$status)
loans$status  <- ifelse(test = ( loans$status == "A" ),"Good", "Bad")
##################
#Loans - Accounts#
##################
accounts      <- read.xlsx("Data/banking.xlsx", sheetIndex = 1)
accounts$date <- as.Date(as.character(accounts$date) , "%y%m%d")
colnames(accounts)[colnames(accounts)=="date"] <- "account_create_date";
loans   <- merge(accounts,loans,"account_id");
loans$clientRelationAge <- as.numeric(difftime(loans$date,loans$account_create_date,units = "weeks")/52.25)
remove(accounts);
####################
# Loans - District #
####################
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
colnames(districts) <- paste("Acc", colnames(districts), sep = "_");
loans   <- merge(loans,districts,by.x = "district_id" , by.y = "Acc_A1");
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
##########################
# Clients - Dispositions #
##########################
dispositions  <- read.xlsx("Data/banking.xlsx", sheetIndex = 3);
clients       <- merge(clients, dispositions,"client_id");
colnames(clients)[colnames(clients)=="type"] <- "disp_type";
remove(dispositions);
#########################
#  Client-Cred.Card     #
#########################
creditCards   <- read.xlsx("Data/banking.xlsx", sheetIndex = 6);
colnames(creditCards)[colnames(creditCards)=="type"] <- "credit_card_type";
clients <- merge(clients,creditCards,by = "disp_id",all = T);
remove(creditCards);
########################
#  Client - Districts  #
########################
districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
colnames(districts) <- paste("Cli", colnames(districts), sep = "_");
clients <- merge(clients, districts, "district_id", by.x = "district_id", by.y = "Cli_A1");
remove(districts)

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


transactions  <- read.csv("Data/transaction.csv",TRUE,";")

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
