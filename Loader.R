#sudo apt-get install r-cran-rjava
install.packages("xlsx", dep = T);      # If you have not installed it before
install.packages("plyr");
install.packages("gsubfn");

options(java.parameters = "- Xmx1024m")

library(xlsx);
library(plyr);
library(gsubfn)

loans         <- read.xlsx("Data/banking.xlsx", sheetIndex = 5)
loans$date    <- as.Date(as.character(loans$date) , "%y%m%d")

accounts      <- read.xlsx("Data/banking.xlsx", sheetIndex = 1)
accounts$date <- as.Date(as.character(accounts$date) , "%y%m%d")
colnames(accounts)[colnames(accounts)=="date"] <- "account_create_date";

accumulator   <- merge(accounts,loans,"account_id");
remove(accounts);
remove(loans);
accumulator$clientRelationAge <- as.numeric(difftime(accumulator$date,accumulator$account_create_date,units = "weeks")/52.25)

districts     <- read.xlsx("Data/banking.xlsx", sheetIndex  = 7);
colnames(districts) <- paste("Acc", colnames(districts), sep = "_");
accumulator   <- merge(accumulator,districts,by.x = "district_id" , by.y = "Acc_A1");
remove(districts);
accumulator$district_id <- NULL;

dispositions_  <- read.xlsx("Data/banking.xlsx", sheetIndex = 3)
dispositions   <- ddply(dispositions_,.variables = "account_id",.fun = function(piece){
  newPiece <- subset(piece,piece$type == "OWNER");
  newPiece$disp_count <- length(piece$type);
  newPiece
});
remove(dispositions_);
accumulator <- merge(accumulator,dispositions,"account_id");
remove(dispositions);

clients        <- read.xlsx("Data/banking.xlsx", sheetIndex = 2);
clients$gender <- ifelse(is.na(as.numeric(strapplyc(clients$birth_number,"\\d{2}([5-9]\\d)\\d{2}"))), "M", "F");
clients$birth_number <- ifelse(clients$gender == "F",paste(strapplyc(clients$birth_number,"^\\d{2}"),as.character(as.numeric(strapplyc(clients$birth_number,"\\d{2}([5-9]\\d)\\d{2}"))-50),strapplyc(clients$birth_number,"\\d{2}$"),sep = ""),clients$birth_number);
clients$birth_date   <- as.Date(clients$birth_number , "%y%m%d")
clients$birth_number <- NULL;
accumulator <- merge(accumulator,clients,"client_id");
remove(clients);


accumulator$client_id <- NULL;
accumulator$account_id <- NULL;
write.csv(accumulator,"result.csv",row.names = FALSE);


#transactions  <- read.csv("Data/transaction.csv",TRUE,";")

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
