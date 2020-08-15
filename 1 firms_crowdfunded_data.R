
library("dplyr")

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Thesis/Data/Script")


###### DATA PREPARATION FILE #######


# control groups that will be created in this data file

# control group 1, non random, size 1:1 to CF sample, non.random_control_group.1
# control group 2, non random, size 1:10 to CF sample, non.random_control_group.2
# control group 3, random, size 1:1 to CF sample, random_control_group.3
# control group 4, random, size 1:10 to CF sample, random_control_group.4
# control group 5, all gathered firms from Orbis, control_group



# substrRight function to select N characters of the a string, R has no built-in function for this remarkably 
# in this script, used to select the last 4 values of the string founding date, allows me to select founding year

set.seed( 393493479 )
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



# load control group files 

control_group2 <- read.csv( "controlgroup2.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group3 <- read.csv( "controlgroup3.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group4 <- read.csv( "controlgroup4.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group5 <- read.csv( "controlgroup5.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group6 <- read.csv( "controlgroup6.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group7 <- read.csv( "controlgroup7.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group8 <- read.csv( "controlgroup8.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group9 <- read.csv( "controlgroup9.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group10 <- read.csv( "controlgroup10.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group11 <- read.csv( "controlgroup11.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group12 <- read.csv( "controlgroup12.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group13 <- read.csv( "controlgroup13.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group14 <- read.csv( "controlgroup14.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group15 <- read.csv( "controlgroup15.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group16 <- read.csv( "controlgroup16.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group17 <- read.csv( "controlgroup17.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group18 <- read.csv( "controlgroup18.csv", sep=";", stringsAsFactors = FALSE, dec="," )
control_group19 <- read.csv( "controlgroup19.csv", sep=";", stringsAsFactors = FALSE, dec="," )

str( control_group2 )
str( control_group3 )
str( control_group4 )
str( control_group5 )
str( control_group6 )
str( control_group7 )
str( control_group8 )
str( control_group9 )
str( control_group10 )
str( control_group11 )
str( control_group12 )
str( control_group13 )
str( control_group14 )
str( control_group15 )
str( control_group16 )
str( control_group17 )
str( control_group18 )
str( control_group19 )



# merge the seperate control group files to one dataframe

control_group <- rbind(control_group2, control_group3, control_group4, control_group5, 
                       control_group6, control_group7, control_group8, control_group9, control_group10, 
                       control_group11, control_group12, control_group13, control_group14, control_group15, 
                       control_group16, control_group17, control_group18, control_group19)



# use substrRight function to select founding year, here I shorten it to the last 2 digits

control_group$Founding.Date= as.numeric( substrRight( as.character( control_group$Date.of.incorporation ), 2 ) )
control_group$isCrowdfunded = 0

str(control_group )



# remove NAs total assets and non-current liab from control group

control_group = control_group[ which( !( is.na( control_group$Total.assets.th.EUR.2017 ) ) & !( is.na( control_group$Total.assets.th.EUR.2016  ) ) 
                                      & !( is.na( control_group$Non.current.liabilities.th.EUR.2017  ) ) & !( is.na( control_group$Non.current.liabilities.th.EUR.2016  ) )  ) , ]





################################# EQUITY CF FIRMS #################################

# load equity CF firms file from data set provided by Crowdfundmarkt

data_crowdfunded <- read.csv( "Equity CF Firms.csv", sep=";", dec="," )
str( data_crowdfunded )



# load financials gathered from Orbis for equity CF firms

orbis <- read.csv( "Equity CF Financials.csv", sep=";", stringsAsFactors = FALSE, dec="," )
head( orbis )
str( orbis )



# merge both firms and financial based on company name

colnames(orbis)[1] = "Company.Name"
head( data_crowdfunded$Company.Name)
head( orbis$Company.Name)
data_crowdfunded$Company.Name <- as.character( data_crowdfunded$Company.Name)
data_crowdfunded$Company.Name <- toupper( as.character( data_crowdfunded$Company.Name))
test <-intersect( as.character( data_crowdfunded$Company.Name ), orbis$Company.Name )
setdiff( orbis$Company.Name, test ) 
setdiff( data_crowdfunded$Company.Name, test ) 

totalData <- merge( data_crowdfunded, orbis, by="Company.Name", all.x = F, all.y = F)


                    
# orbis shows founding date in different formats, here we choose the year (last 2 numbers)

totalData$Founding.Date <- as.numeric( substrRight( as.character( totalData$Date.of.incorporation ), 2 ) )





################################# DEBT CF FIRMS #################################

# load debt CF Firms file from data set provided by Crowdfundmarkt

debt_funding_firms <- read.csv( "Debt CF Firms.csv", sep=";", dec="," )
str( debt_funding_firms )
str( data_crowdfunded )



# load financials gathered from Orbis for Debt CF firms

orbis <- read.csv( "Debt CF Financials.csv", sep=";", stringsAsFactors = FALSE, dec="," )
head( orbis )
str( orbis )



# merge both firms and financial based on company name

colnames(orbis)[1] = "Company.Name"
head( debt_funding_firms$Company.Name)
#data_crowdfunded$Company.Name <- as.character( data_crowdfunded$Company.Name)
debt_funding_firms$Company.Name <- toupper( as.character( debt_funding_firms$Company.Name))
test <-intersect( debt_funding_firms$Company.Name, orbis$Company.Name ) 
setdiff( orbis$Company.Name, test ) 
setdiff( debt_funding_firms$Company.Name, test ) 

totalData2 <- merge( debt_funding_firms, orbis, by="Company.Name", all.x = F, all.y = F)
totalData2$Founding.Date= as.numeric( substrRight( as.character( totalData2$Date.of.incorporation ), 2 ) )



# final data set, all CF firms 

totalData3 = bind_rows( totalData, totalData2 )
totalData3$isCrowdfunded = 1
totalData = totalData3



# remove firms where sector is not known and remove some extreme values

totalData <- totalData[ which( totalData$BvD.major.sector != "" ), ]
totalData <- totalData[!totalData$Company.Name == "TOP OF HOLLAND B.V.", ]
totalData <- totalData[!totalData$Company.Name == "WARANT B.V.", ]



# add average assets to both CF and non CF to be able to compute quantiles for the asset sizes later on for non-random control group

totalData$averageAssets = rowMeans( totalData[, c( "Total.assets.th.EUR.2017", "Total.assets.th.EUR.2016", "Total.assets.th.EUR.2015", "Total.assets.th.EUR.2014", "Total.assets.th.EUR.2013")], na.rm=T)
control_group$averageAssets = rowMeans( control_group[, c( "Total.assets.th.EUR.2017", "Total.assets.th.EUR.2016", "Total.assets.th.EUR.2015", "Total.assets.th.EUR.2014", "Total.assets.th.EUR.2013")], na.rm=T)



# remove NAs for total assets and long term debt CF firms

# firms funded in 2016, I use total assets and long term debt 2016 and 2017
# firms funded in 2017, I use total assets and long term debt 2015 and 2016
# firms with funding year NA, I use total assets and long term debt 2015, 2016, 2017

cf.firms.2016 <- totalData[ which( totalData$Funding.year == 2016), ] # CF in 2016
cf.firms.2015 <- totalData[ which( totalData$Funding.year == 2015), ] # CF in 2015
f.yr.NA <- totalData[ which( ( is.na( totalData$Funding.year ) ) ), ] # NA, here we remove all 15, 16 and 17 to be sure

cf.firms.2016 = cf.firms.2016[ which( !( is.na( cf.firms.2016$Total.assets.th.EUR.2017 ) ) & !( is.na( cf.firms.2016$Total.assets.th.EUR.2016 ) ) 
                          & !( is.na( cf.firms.2016$Non.current.liabilities.th.EUR.2017 ) ) & !( is.na( cf.firms.2016$Non.current.liabilities.th.EUR.2016 ) ) ) , ]

cf.firms.2015 = cf.firms.2015[ which( !( is.na( cf.firms.2015$Total.assets.th.EUR.2016 ) ) & !( is.na( cf.firms.2015$Total.assets.th.EUR.2015 ) ) 
                          & !( is.na( cf.firms.2015$Non.current.liabilities.th.EUR.2016 ) ) & !( is.na( cf.firms.2015$Non.current.liabilities.th.EUR.2015 ) ) ) , ]

f.yr.NA = f.yr.NA[ which( !( is.na( f.yr.NA$Total.assets.th.EUR.2017 ) ) & !( is.na( f.yr.NA$Total.assets.th.EUR.2016 ) ) 
                          & !( is.na( f.yr.NA$Total.assets.th.EUR.2015 ) )
                          & !( is.na( f.yr.NA$Non.current.liabilities.th.EUR.2017 ) ) 
                          & !( is.na( f.yr.NA$Non.current.liabilities.th.EUR.2016 ) ) 
                          & !( is.na( f.yr.NA$Non.current.liabilities.th.EUR.2015 ) ) ) , ]

totalData <- bind_rows(cf.firms.2016, cf.firms.2015, f.yr.NA)



# remove all CF firms from control group file (non CF)
# warning, takes a while 


used <- totalData3$Company.Name

for(i in 1:length(used)) {
  control_group <- control_group[!control_group$Company.name == used[i], ]
}

# control group is now reduced from 508194 to 507871 firms



# add the firm age in years to the data set, I transform the funding year to age in years 

# CF sample 

no.changes.age <- totalData[ which( totalData$Founding.Date >= 0 & totalData$Founding.Date <= 19) , ] 
changes.age <- totalData[ which( totalData$Founding.Date >= 47 & totalData$Founding.Date <= 99) , ] 

no.changes.age$firm.age <- (19 - no.changes.age$Founding.Date)
changes.age$firm.age <- ((100 - changes.age$Founding.Date) + 19)

totalData <- bind_rows(no.changes.age, changes.age)
  


# non-CF sample

no.changes.age2 <- control_group[ which( control_group$Founding.Date >= 0 & control_group$Founding.Date <= 19) , ] 
changes.age2 <- control_group[ which( control_group$Founding.Date >= 47 & control_group$Founding.Date <= 99) , ] 


no.changes.age2$firm.age <- (19 - no.changes.age2$Founding.Date)
changes.age2$firm.age <- ((100 - changes.age2$Founding.Date) + 19)


control_group <- bind_rows(no.changes.age2, changes.age2)



# create back-up of crowdfunded sample

CF.data.cleaned <- totalData

