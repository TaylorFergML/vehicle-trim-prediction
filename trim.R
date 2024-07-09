library(readr)
trim_train <- read_csv("C:/Users/tjf4x/Downloads/Boeing Data Science Challenge Problem/Training_DataSet.csv")
View(Training_DataSet)

library(tidyverse)

glimpse(trim_train)

# Investigating variables that don't appear to vary

unique(trim_train$SellerIsPriv)
sum(trim_train$SellerIsPriv, na.rm = TRUE)/length(trim_train)
unique(trim_train$VehBodystyle)
unique(trim_train$VehFuel)
unique(trim_train$VehType)
unique(trim_train$VehTransmission)

# Check for dupes
sum(duplicated(trim_train))

# Remove independent variables that don't change
trim_train <- subset(trim_train, select = -c(SellerIsPriv, VehTransmission, ListingID, VehBodystyle, VehType) )

# Next I removed city and zipcode to just leave state for geographic location
trim_train <- subset(trim_train, select = -c(SellerCity, SellerZip) )

trim_train$SellerState <- as.factor(trim_train$SellerState)

# removed model since both makes only have a single model in the dataset
unique(trim_train$VehMake)
unique(trim_train$VehModel)

trim_train <- subset(trim_train, select = -c(VehModel) )

# Exploring usefulness of dealer variables
count(trim_train, SellerListSrc)
plot(factor(trim_train$SellerListSrc), trim_train$Dealer_Listing_Price)

count(trim_train, SellerName)

trim_train$SellerListSrc <- as.factor(trim_train$SellerListSrc)

# Removing seller name. Leaving listing service for now.

trim_train <- subset(trim_train, select = -c(SellerName) )

plot(trim_train$SellerRating, trim_train$Dealer_Listing_Price)
abline(lm(trim_train$Dealer_Listing_Price ~ trim_train$SellerRating))

plot(trim_train$SellerRevCnt, trim_train$Dealer_Listing_Price)
abline(lm(trim_train$Dealer_Listing_Price ~ trim_train$SellerRevCnt))

# There does appear to be a slight relationship between Seller Rating and Seller review counts with listing price

# Exploring vehicle features

#Vehicle Exterior Colors
library(dplyr)
ext_color <- rename(count(trim_train, VehColorExt), Freq = n)
print(ext_color, n=269)

unique(trim_train$VehColorExt)

split <- split(trim_train$Vehicle_Trim, trim_train$VehColorExt)
print(split)

# sorting colors into official color names when possible. Grouping the rest by basic color or other for uncommon colors
trim_train$VehColorExt <-  tolower(trim_train$VehColorExt)

trim_train$VehColorExt <- gsub(".*Red.*line.*","Redline",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*True.*Blue.*","True Blue Pearl",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*velvet.*","Velvet Red Pearl",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*walnut.*","Walnut Brown",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*steel.*","Steel Metallic Clear Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*sangria.*","Sangria Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*Ivory.*","Ivory Tri-Coat Pearl",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*granite.*","Granite Crystal Metallic Clear Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*diamond.*","Diamond Black Crystal Pearl Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*diamond.*","Diamond Black Crystal Pearl Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*cherry.*","Deep Cherry Red Crystal Pearl Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*billet.*","Billet Silver Metallic Clear Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*forest.*","Black Forest Green Pearl",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*brilliant.*black.*","Brilliant Black Crystal Pearl Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*bright.*white.*","Bright White Clear Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*Cashmere.*","Cashmere Pearl Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*stellar.*","Stellar Black Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*coast.*","Silver Coast Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*passion.*","Red Passion Tintcoat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*radiant.*silver.*","Radiant Silver Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*midnight.*","Midnight Sky Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*harbor.*","Harbor Blue Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*amethyst.*","Deep Amethyst Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*adriatic.*","Dark Adriatic Blue Metallic",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*crystal.*white.*","Crystal White Tricoat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*billiet.*","Billet Silver Metallic Clear Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*recon.*","Recon Green",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*rhino.*","Rhino",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*mineral.*","Mineral Gray",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*moonlight.*","Silver Moonlight",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*luxury.*brown.*","Luxury Brown",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*brownstone.*","Brownstone",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*auburn.*","Deep Auburn Pearl Coat",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*horizon.*","Red Horizon",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*frost.*","White Frost",trim_train$VehColorExt, ignore.case=TRUE)
trim_train$VehColorExt <- gsub(".*red.*", "Red",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*black.*", "Black",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*gray.*", "Gray",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*white.*", "White",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*blue.*", "Blue",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*bronze.*", "Bronze Dune Metallic",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*bown.*", "Brown",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub(".*silver.*", "Silver",trim_train$VehColorExt, ignore.case=FALSE)
trim_train$VehColorExt <- gsub("^[a-z].*", "Other",trim_train$VehColorExt, ignore.case=FALSE)

trim_train$VehColorExt <- as.factor(trim_train$VehColorExt)

#Vehicle Interior Colors

int_color <-  split(trim_train$VehColorInt, trim_train$Vehicle_Trim)

unique(trim_train$VehColorInt)

# sorting interior colors into a few unique colors to models, then categorizing them by style or basic color

trim_train$VehColorInt <-  tolower(trim_train$VehColorInt)

trim_train$VehColorInt <- gsub(".*dark.*ruby.*", "Dark Ruby Red",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*ruby.*", "Ruby Red",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*maple.*", "Maple Sugar",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*leather.*", "Leather Interior",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*cloth.*", "Cloth Interior",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*trim.*", "Special Trim",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*accent.*", "Special Accents",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*frost.*", "Frost",trim_train$VehColorInt, ignore.case=TRUE)
trim_train$VehColorInt <- gsub(".*red.*", "Red",trim_train$VehColorInt, ignore.case=FALSE)
trim_train$VehColorInt <- gsub(".*black.*", "Black",trim_train$VehColorInt, ignore.case=FALSE)
trim_train$VehColorInt <- gsub(".*gray.*", "Gray",trim_train$VehColorInt, ignore.case=FALSE)
trim_train$VehColorInt <- gsub(".*brown.*", "Brown",trim_train$VehColorInt, ignore.case=FALSE)
trim_train$VehColorInt <- gsub(".*tan.*", "Brown",trim_train$VehColorInt, ignore.case=FALSE)
trim_train$VehColorInt <- gsub("^[a-z].*", "Other",trim_train$VehColorInt, ignore.case=FALSE)

trim_train$VehColorInt <- as.factor(trim_train$VehColorInt)

# Vehicle drive type

drive <- rename(count(trim_train, VehDriveTrain), Freq = n)

unique(trim_train$VehDriveTrain)

# Sorted drive type to simply Fwd or 4x4

trim_train$VehDriveTrain <- gsub(".*2.*|.*front.*|.*fwd.*", "Fwd",trim_train$VehDriveTrain, ignore.case=TRUE)
trim_train$VehDriveTrain <- gsub(".*4.*|.*a.*|.*four.*", "4x4",trim_train$VehDriveTrain, ignore.case=TRUE)

trim_train$VehDriveTrain <- as.factor(trim_train$VehDriveTrain)

# Vehicle engine

Engine <- rename(count(trim_train, VehEngine), Freq = n)

unique(trim_train$VehEngine)

# Sorted engine by size when possible, otherwise type

trim_train$VehEngine <- gsub(".*3\\.6.*", "3.6L",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*3\\.0.*", "3.0L",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*6\\.4.*", "6.4L",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*5\\.7.*", "5.7L",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*6\\.2.*", "6.2L",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*8.*", "V8",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*6.*c.*", "V6",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*v.*6.*", "V6",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*hemi.*", "V8",trim_train$VehEngine, ignore.case=TRUE)
trim_train$VehEngine <- gsub(".*^6$.*", "V6",trim_train$VehEngine, ignore.case=TRUE)

trim_train$VehEngine <- as.factor(trim_train$VehEngine)

# Vehicle Fuel

Fuel <- rename(count(trim_train, VehFuel), Freq = n)

unique(trim_train$VehFuel)

trim_train$VehFuel <- as.factor(trim_train$VehFuel)

# Make

unique(trim_train$VehMake)

trim_train$VehMake <- as.factor(trim_train$VehMake)

# Price Label

unique(trim_train$VehPriceLabel)

trim_train$VehPriceLabel <- as.factor(trim_train$VehPriceLabel)

# Year

unique(trim_train$VehYear)

# Trim

unique(trim_train$Vehicle_Trim)
trim <- rename(count(trim_train, Vehicle_Trim), Freq = n)

# Cleaning up vehicle trim names

trim_train$Vehicle_Trim <- gsub(".*75th.*", "75th Anniversary",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*Limited.*", "Limited",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*premium.*luxury.*", "Premium Luxury",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub("^luxury.*", "Luxury",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*platinum.*", "Platinum",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*laredo.*", "Laredo",trim_train$Vehicle_Trim, ignore.case=TRUE)

# Investigating FWD trim. It is likely a Base Cadillac but decided to look further after text mining 
basetrim <- trim_train %>% 
  filter(Vehicle_Trim == "FWD")
## Came back and changed to Base
trim_train$Vehicle_Trim <- gsub(".*FWD.*", "Base",trim_train$Vehicle_Trim, ignore.case=TRUE)

# Removed records with missing trim since they wouldn't provide any predictive power

missing <- !complete.cases(trim_train$Vehicle_Trim)
trim_train <- trim_train[!is.na(trim_train$Vehicle_Trim), ]

trim_train$Vehicle_Trim <- as.factor(trim_train$Vehicle_Trim)

barplot(table(trim_train$Vehicle_Trim))

summary(trim_train$Vehicle_Trim)

#History

unique(trim_train$VehHistory)

# Created logical variables for different categories listed in history

trim_train$Accidents <- grepl("Accident", trim_train$VehHistory)
trim_train$TitleIssue <- grepl("Title", trim_train$VehHistory)
trim_train$NonPersonalUse <- grepl("Non-Personal", trim_train$VehHistory)
trim_train$BuybackProtec <- grepl("Buyback", trim_train$VehHistory)

# Removed everything after "Owner" to create a variable containing number of owners.
trim_train$VehHistory <- sub("\\ Owner.*", "", trim_train$VehHistory)

trim_train$VehHistory <- as.numeric(trim_train$VehHistory)

# Numeric Variables

glimpse(trim_train)

hist(trim_train$SellerRating)

hist(trim_train$SellerRevCnt)

hist(trim_train$VehListdays)

hist(trim_train$VehMileage)

hist(trim_train$Dealer_Listing_Price)

# Features and Seller Notes

install.packages("quanteda")
library(quanteda)

install.packages("quanteda.textstats")
library(quanteda.textstats)
unique(trim_train$VehFeats)

# combining features and seller notes

trim_train$VehSellerNotes[is.na(trim_train$VehSellerNotes)] <- " "
trim_train$VehFeats[is.na(trim_train$VehFeats)] <- " "

trim_train$VehFeats <- tolower(trim_train$VehFeats)
trim_train$VehSellerNotes <- tolower(trim_train$VehSellerNotes)

trim_train$text <- str_c(trim_train$VehFeats, " ", trim_train$VehSellerNotes)

# Create a vector of trims

trims <- c(tolower(levels(trim_train$Vehicle_Trim)))

# Created a binary variable for each trim listed in remaining string variables.

library(stringr)
for(i in trims){
  trim_train[i] <- +str_detect(apply(trim_train[,1:18], 1, paste0, collapse = " "), trims[which(trims == i)])
}

featCorpus <- corpus(trim_train$text)

docvars(featCorpus, "Textno") <-
  sprintf("%02d", 1:ndoc(featCorpus))

feattoken <-
  tokens(
    featCorpus,
    split_hyphens = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    include_docvars = TRUE
  )

feattoken <- tokens_remove(feattoken, stopwords())

featdfm <- dfm(feattoken,
               tolower = TRUE)

featdfm <- dfm_remove(featdfm, stopwords("english"))

trim_train %>% 
  group_by(Vehicle_Trim) %>%
  summarise(no_rows = length(Vehicle_Trim))/length(trim_train)

featdfm <-
  dfm_trim(
    featdfm,
    min_docfreq = 0.10,
    max_docfreq = 0.45,
    docfreq_type = "prop"
  ) 

groupfeatdfm <- dfm_group(featdfm, groups = trim_train$Vehicle_Trim)
head(groupfeatdfm)

groupfeatkey <- textstat_keyness(groupfeatdfm)

print(groupfeatkey)

feat <- dfm_tfidf(groupfeatdfm, scheme_tf = "propmax")

topfeatures(feat, n = 20, groups = docnames(feat))

# No other features stand out. I'll do a test run to see how easily
# the data classifies currently.

trim_train %>% replace_na(list(VehColorInt = "unknown", VehDriveTrain = "unknown",VehPriceLabel = "unknown"))

# Seperating test and train set
set.seed(123)

trim_train$Vehicle_Trim <- gsub(".*x75th Anniversary.*", "x75th_Anniversary",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*High Altitude.*", "High_Altitude",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*Premium Luxury.*", "Premium_Luxury",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*SRT Night.*", "SRT_Night",trim_train$Vehicle_Trim, ignore.case=TRUE)
trim_train$Vehicle_Trim <- gsub(".*Sterling Edition.*", "Sterling",trim_train$Vehicle_Trim, ignore.case=TRUE)


trim_train <- subset(trim_train, select = -c(VehFeats, VehSellerNotes, text) )

trim_train$id <- 1:nrow(trim_train)

#use 70% of dataset as training set and 30% as test set 
train <- trim_train %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(trim_train, train, by = 'id')

# Running a random forest to get a bench mark

install.packages("ranger")
library(ranger)
library(caret)

CTRL <- trainControl(method = "repeatedcv", 
                     number = 2, 
                     repeats = 1,
                     classProbs = TRUE)

ranger_model <- caret::train(Vehicle_Trim ~ .,
                             train,
                             method = "ranger",
                             trControl = CTRL,
                             na.action = na.omit,
                             metric="ROC",
                             tuneGrid = expand.grid(mtry=c(1,5), splitrule="gini", min.node.size = 10))

ranger_model

