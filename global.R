#### Load libraries ####
library(lubridate)

#### Cleaning and Getting Data ####

# Read in train data
training <- read.csv("./www/data/train.csv", stringsAsFactors = FALSE)

# Convert DateTime to POSUX date time format
training$DateTime <- as.POSIXct(as.character(training$DateTime), format="%Y-%m-%d %H:%M:%S")

# Extract hour, weekday, month and year from DateTime
training$Hour    <- hour(training$DateTime)
training$Weekday <- wday(training$DateTime, label = TRUE)
training$Month   <- month(training$DateTime, label = TRUE)
training$Year    <- year(training$DateTime)

# OutcomeType, OutcomeSubtype, AnimalType and SexuponOutcome as factor
training$OutcomeType <- as.factor(training$OutcomeType)
training$OutcomeSubtype <- as.factor(training$OutcomeSubtype)
training$AnimalType <- as.factor(training$AnimalType) 
training$SexuponOutcome <- as.factor(training$SexuponOutcome)


# Reduce Color options
training$ColorReduced <- sapply(training$Color, 
                      function(x) strsplit(x, split = '/| ')[[1]][1])


# Reduce Breed options
training$BreedReduced <- sapply(training$Breed, 
                       function(x) strsplit(x, split = '/')[[1]][1])


mixIndex <- grep("mix", training$BreedReduced, ignore.case = TRUE)
pureSatus <- training$BreedReduced[mixIndex]
mixedStatus <- training$BreedReduced[-mixIndex]

# Read in test data
testing <- read.csv("./www/data/test.csv", stringsAsFactors = FALSE)

# Adjust ID name and type to match training set
names(testing)[1] <- "AnimalID"
testing$AnimalID <- as.character(testing$AnimalID)








#### Working ####
str(training)
summary(training$AnimalID)
summary(training$Name)
sum(is.na(training$DateTime))
table(training$OutcomeType)
table(training$OutcomeSubtype)   
table(training$SexuponOutcome)
table(training$AgeuponOutcome)
hour(training$DateTime[1])
training$DateTime[1]

as.POSIXct(as.character(training$DateTime[1]), format="%Y-%m-%d %H:%M:%S")
training$DateTime[1]

names(training)
names(testing)
table(training$AgeuponOutcome)

tempWeeks <- grep("week", training$AgeuponOutcome)
tempTrainWeeks <- training[tempWeeks, ]
table(tempTrainWeeks$AgeuponOutcome)

tempMonths <- grep("month", training$AgeuponOutcome)
tempTrainMonths <- training[tempMonths, ]
table(tempTrainMonths$AgeuponOutcome)

tempYear <- grep("year", training$AgeuponOutcome)
tempTrainYear <- training[tempYear, ]
table(tempTrainYear$AgeuponOutcome)

tempDays <- grep("day", training$AgeuponOutcome)
tempTrainDays <- training[tempDays, ]
table(tempTrainDays$AgeuponOutcome)


1850 + 9620 + 14843 + 398

levels(as.factor(training$AgeuponOutcome))
training$AgeuponOutcome[training$AgeuponOutcome==""]
testing$AgeuponOutcome[testing$AgeuponOutcome==""]


levels(as.factor(training$Breed))
levels(as.factor(training$Color))

splitColors <- strsplit(levels(as.factor(training$Color)), "/")[[1]]
splitColors <- sapply(levels(as.factor(training$Color)), function(x) strsplit(x, "/")[[1]][[1]])   
unique(splitColors)

SimpleColor <- sapply(training$Color, 
                           function(x) strsplit(x, split = '/| ')[[1]][1])

unique(SimpleColor)


BreedReduced <- sapply(training$Breed, 
                                function(x) strsplit(x, split = '/')[[1]][1])
sort(unique(BreedReduced))





