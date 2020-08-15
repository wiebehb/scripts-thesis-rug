
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Thesis/Data/Script")

#install.packages( "psych")
library("reshape2")
library("psych")
library(corrplot)


###### ANALYSIS FILE #######



# CHOOSE HERE THE DESIRED CONTROL GROUP FOR ANALYSIS 

whichcontrolgroup = "control group 1" 



# OVERVIEW CREATED CONTROL GROUPS

# control group 1, non random, size 1:1 to CF sample, non.random_control_group.1
# control group 2, non random, size 1:10 to CF sample, non.random_control_group.2
# control group 3, random, size 1:1 to CF sample, random_control_group.3
# control group 4, random, size 1:10 to CF sample, random_control_group.4
# control group 5, all gathered firms from Orbis, control_group



# next the CF firms are automatically merged with chosen control group firms

if( whichcontrolgroup == "control group 1" )
{
  totalData <- bind_rows(CF.data.cleaned, non.random_control_group1)
} else if( whichcontrolgroup == "control group 2" ) 
  {
  totalData <- bind_rows(CF.data.cleaned, non.random_control_group2)
} else if( whichcontrolgroup == "control group 3" ) 
  {
  totalData <- bind_rows(CF.data.cleaned, random_control_group.3)
} else if( whichcontrolgroup == "control group 4" ) 
  {
  totalData <- bind_rows(CF.data.cleaned, random_control_group.4)
} else if( whichcontrolgroup == "control group 5" ) 
  {
  totalData <- bind_rows(CF.data.cleaned, control_group)
}



# equity or debt crowdfunding

totalData$isEquityCrowdfunded = ifelse( totalData$Type.of.crowdfunding == "Equity", 1, 0 )
totalData$isEquityCrowdfunded[ which( is.na( totalData$isEquityCrowdfunded))] = 0

totalData$isDebtCrowdfunded = ifelse( totalData$Type.of.crowdfunding == "Equity", 0, 1 )
totalData$isDebtCrowdfunded[ which( is.na( totalData$isDebtCrowdfunded))] = 0



# select leverage 

whichLeverage = "long-term debt" # or short term debt or total debt 

if( whichLeverage == "long-term debt" )
{
  totalData$leverage2017 = totalData$Non.current.liabilities.th.EUR.2017/totalData$Total.assets.th.EUR.2017
  totalData$leverage2016 = totalData$Non.current.liabilities.th.EUR.2016/totalData$Total.assets.th.EUR.2016
  totalData$leverage2015 = totalData$Non.current.liabilities.th.EUR.2015/totalData$Total.assets.th.EUR.2015
} else if( whichLeverage == "short-term debt" ) {
  totalData$leverage2017 = totalData$Current.liabilities.th.EUR.2017/totalData$Total.assets.th.EUR.2017
  totalData$leverage2016 = totalData$Current.liabilities.th.EUR.2016/totalData$Total.assets.th.EUR.2016
  totalData$leverage2015 = totalData$Current.liabilities.th.EUR.2015/totalData$Total.assets.th.EUR.2015
} else if( whichLeverage == "total debt" )
{
  totalData$leverage2017 = ( totalData$Current.liabilities.th.EUR.2017 + totalData$Non.current.liabilities.th.EUR.2017 )/totalData$Total.assets.th.EUR.2017
  totalData$leverage2016 = ( totalData$Current.liabilities.th.EUR.2016 + totalData$Non.current.liabilities.th.EUR.2016 )/totalData$Total.assets.th.EUR.2016
  totalData$leverage2015 = ( totalData$Current.liabilities.th.EUR.2015 + totalData$Non.current.liabilities.th.EUR.2015 )/totalData$Total.assets.th.EUR.2015
}



totalData$leverageAfterCrowdfunding = NA

totalData$leverageAfterCrowdfunding[ which( is.na( totalData$Funding.year ) ) ] =  rowMeans( totalData[ which( is.na( totalData$Funding.year ) ), c( "leverage2016", "leverage2017") ], na.rm=T)
totalData$leverageAfterCrowdfunding[ which(totalData$Funding.year == 2015) ] = totalData$leverage2016[ which(totalData$Funding.year == 2015) ]
totalData$leverageAfterCrowdfunding[ which(totalData$Funding.year == 2016) ] = totalData$leverage2017[ which(totalData$Funding.year == 2016) ]
totalData$leverageBeforeCrowdfunding[ which( is.na( totalData$Funding.year ) ) ] =  rowMeans( totalData[ which( is.na( totalData$Funding.year ) ), c( "leverage2015", "leverage2016") ], na.rm=T)
totalData$leverageBeforeCrowdfunding[ which(totalData$Funding.year == 2015) ] = totalData$leverage2015[ which(totalData$Funding.year == 2015) ]
totalData$leverageBeforeCrowdfunding[ which(totalData$Funding.year == 2016) ] = totalData$leverage2016[ which(totalData$Funding.year == 2016) ]



# test if there are no NAs present in the data set

table( is.na( totalData$leverageAfterCrowdfunding ) )



# back-up data set for hypothesis 2

data_hyp_2 <- totalData





################################# HYPOTHESIS 1 NON-TRIMMED #################################

use.variables = c( "BvD.major.sector", "leverageAfterCrowdfunding", "leverageBeforeCrowdfunding", "isCrowdfunded", "averageAssets" )
lm.Data = totalData[ use.variables]
lm.Data = lm.Data[ complete.cases(lm.Data[ use.variables]), use.variables ]



# for the corrplot, and joint hypothesis test of the category variable, it is handy to make dummy variables for the sector 
# instead of including it in the model as a factor

lm.Data$BvD.major.sector = factor( lm.Data$BvD.major.sector, sort(unique(lm.Data$BvD.major.sector)) )



# dummies_sector = dummy_cols( lm.Data$BvD.major.sector, remove_first_dummy = T, sort_columns = T )

dummies_sector = dummy.code( lm.Data$BvD.major.sector )



# as a baseline sector in the regression, I use 'Banks'

lm.Data2 = lm.Data[ , setdiff(colnames(lm.Data), 'BvD.major.sector')] 
lm.Data2 = cbind( lm.Data2, dummies_sector  )



# change inf values to NAs

is.na(lm.Data2) <- sapply(lm.Data2, is.infinite)



# corrplot to gain an insight into the strongest correlations

corr.data = cor( lm.Data2 )
rownames(corr.data) = colnames( corr.data ) = substring( colnames(corr.data), 1, 15 ) # shorter names so the figure becomes more clear

corrplot( corr.data )

summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 1 ])
summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 0 ])



lm.Data2 = lm.Data2[, setdiff( colnames( lm.Data2), c( "Banks") )]
model1 <- lm( data = lm.Data2, leverageAfterCrowdfunding ~ . )
coefmodel1 <- summary( model1 )
coefmodel1


plot( lm.Data2$leverageAfterCrowdfunding )
lines( model1$fitted.values)





################################# HYPOTHESIS 1 TRIM 90th percentile #################################

use.variables = c( "BvD.major.sector", "leverageAfterCrowdfunding", "leverageBeforeCrowdfunding", "isCrowdfunded", "averageAssets" )
lm.Data = totalData[ use.variables]
lm.Data = lm.Data[ complete.cases(lm.Data[ use.variables]), use.variables ]



# data trimming 

# I trim the right side of the data, top 10 percent

quantile(lm.Data$leverageAfterCrowdfunding, 0.90, na.rm=T) 
quantile(lm.Data$leverageBeforeCrowdfunding, 0.90, na.rm=T)
quantile(lm.Data$averageAssets, 0.90, na.rm=T) 

lm.Data <- lm.Data[ which( lm.Data$leverageAfterCrowdfunding <= 0.7782715 ) , ]
lm.Data <- lm.Data[ which( lm.Data$leverageBeforeCrowdfunding <= 0.8538003 ) , ]
lm.Data <- lm.Data[ which( lm.Data$averageAssets <= 1930.405 ), ]



# for the corrplot, and joint hypothesis test of the category variable, it is handy to make dummy variables for the sector (instead of including it in the model as a factor)

lm.Data$BvD.major.sector = factor( lm.Data$BvD.major.sector, sort(unique(lm.Data$BvD.major.sector)) )



# dummies_sector = dummy_cols( lm.Data$BvD.major.sector, remove_first_dummy = T, sort_columns = T )

dummies_sector = dummy.code( lm.Data$BvD.major.sector )



# as a baseline sector in the regression, we use 'Banks'

lm.Data2 = lm.Data[ , setdiff(colnames(lm.Data), 'BvD.major.sector')] 
lm.Data2 = cbind( lm.Data2, dummies_sector  )



# change inf values to NAs

is.na(lm.Data2) <- sapply(lm.Data2, is.infinite)



# corrplot to gain an insight into the strongest correlations

corr.data = cor( lm.Data2 )
rownames(corr.data) = colnames( corr.data ) = substring( colnames(corr.data), 1, 15 ) # shorter names so the figure becomes more clear

corrplot( corr.data )

summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 1 ])
summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 0 ])



lm.Data2 = lm.Data2[, setdiff( colnames( lm.Data2), c( "Banks") )]
model1 <- lm( data = lm.Data2, leverageAfterCrowdfunding ~ . )
coefmodel1 <- summary( model1 )
coefmodel1


plot( lm.Data2$leverageAfterCrowdfunding )
lines( model1$fitted.values)





################################# HYPOTHESIS 1 ZERO LEVERAGE BEFORE FUNDING NON-TRIMMED ########################

# select zero leverage firms 

zero.leverage.data <- totalData[ which(totalData$leverageBeforeCrowdfunding == 0.0000000) , ]

use.variables = c( "BvD.major.sector", "leverageAfterCrowdfunding", "isCrowdfunded", "averageAssets" ) # to check if all zero leverage firms are removed, add leveragebeforefunding here
lm.Data = zero.leverage.data[ use.variables]
lm.Data = lm.Data[ complete.cases(lm.Data[ use.variables]), use.variables ]



# number of firms zero leverage before crowdfunding

nrow(zero.leverage.data[ which(lm.Data$isCrowdfunded == 1) , ]) # 103 CF firms
nrow(zero.leverage.data[ which(lm.Data$isCrowdfunded == 0) , ]) # 175 non CF firms



# for the corrplot, and joint hypothesis test of the category variable, it is handy to make dummy variables for the sector (instead of including it in the model as a factor)

lm.Data$BvD.major.sector = factor( lm.Data$BvD.major.sector, sort(unique(lm.Data$BvD.major.sector)) )



# dummies_sector = dummy_cols( lm.Data$BvD.major.sector, remove_first_dummy = T, sort_columns = T )

dummies_sector = dummy.code( lm.Data$BvD.major.sector )



# as a baseline sector in the regression, we use 'Banks'

lm.Data2 = lm.Data[ , setdiff(colnames(lm.Data), 'BvD.major.sector')] 
lm.Data2 = cbind( lm.Data2, dummies_sector  )



# change inf values to NAs

is.na(lm.Data2) <- sapply(lm.Data2, is.infinite)



# corrplot to gain an insight into the strongest correlations

corr.data = cor( lm.Data2 )
rownames(corr.data) = colnames( corr.data ) = substring( colnames(corr.data), 1, 15 ) # shorter names so the figure becomes more clear

corrplot( corr.data )

summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 1 ])
summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 0 ])


lm.Data2 = lm.Data2[, setdiff( colnames( lm.Data2), c( "Banks") )]
model1 <- lm( data = lm.Data2, leverageAfterCrowdfunding ~ . )
coefmodel1 <- summary( model1 )
coefmodel1


plot( lm.Data2$leverageAfterCrowdfunding )
lines( model1$fitted.values)





################################# HYPOTHESIS 1 ZERO LEVERAGE BEFORE FUNDING TRIMMED ############################

# for zero leverage before crowdfunding, select this lm data in next lines

zero.leverage.data <- totalData[ which(totalData$leverageBeforeCrowdfunding == 0.0000000) , ]
lm.Data = zero.leverage.data[ use.variables]
lm.Data = lm.Data[ complete.cases(lm.Data[ use.variables]), use.variables ]



# data trimming 

# I ONLY trim the right side of the data, 90 percent  

quantile(lm.Data$leverageAfterCrowdfunding, 0.90, na.rm=T) 
quantile(lm.Data$leverageBeforeCrowdfunding, 0.90, na.rm=T) # outcome should be zero
quantile(lm.Data$averageAssets, 0.90, na.rm=T) 



lm.Data <- lm.Data[ which( lm.Data$leverageAfterCrowdfunding <= 0 ) , ]
#lm.Data <- lm.Data[ which( lm.Data$leverageBeforeCrowdfunding <= 0.8538003 ) , ]
lm.Data <- lm.Data[ which( lm.Data$averageAssets <= 1738.588 ), ]



# number of firms zero leverage before crowdfunding

nrow(lm.Data[ which(lm.Data$isCrowdfunded == 1) , ]) # 75 CF firms
nrow(lm.Data[ which(lm.Data$isCrowdfunded == 0) , ]) # 150 non CF firms



use.variables = c( "BvD.major.sector", "leverageAfterCrowdfunding", "isCrowdfunded", "averageAssets" ) # to check if all zero leverage firms are removed, add leveragebeforefunding here
lm.Data = lm.Data[ use.variables]
lm.Data = lm.Data[ complete.cases(lm.Data[ use.variables]), use.variables ]



# for the corrplot, and joint hypothesis test of the category variable, it is handy to make dummy variables for the sector (instead of including it in the model as a factor)

lm.Data$BvD.major.sector = factor( lm.Data$BvD.major.sector, sort(unique(lm.Data$BvD.major.sector)) )



# dummies_sector = dummy_cols( lm.Data$BvD.major.sector, remove_first_dummy = T, sort_columns = T )

dummies_sector = dummy.code( lm.Data$BvD.major.sector )



# as a baseline sector in the regression, we use 'Banks'

lm.Data2 = lm.Data[ , setdiff(colnames(lm.Data), 'BvD.major.sector')] 
lm.Data2 = cbind( lm.Data2, dummies_sector  )



# change inf values to NAs

is.na(lm.Data2) <- sapply(lm.Data2, is.infinite)



# corrplot to gain an insight into the strongest correlations

corr.data = cor( lm.Data2 )
rownames(corr.data) = colnames( corr.data ) = substring( colnames(corr.data), 1, 15 ) # shorter names so the figure becomes more clear

corrplot( corr.data )

summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 1 ])
summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 0 ])


lm.Data2 = lm.Data2[, setdiff( colnames( lm.Data2), c( "Banks") )]
model1 <- lm( data = lm.Data2, leverageAfterCrowdfunding ~ . )
coefmodel1 <- summary( model1 )
coefmodel1


plot( lm.Data2$leverageAfterCrowdfunding )
lines( model1$fitted.values)





################################# HYPOTHESIS 1 WINSORIZATION #################################

# select winsorization data 

winsor.data <- totalData



# change inf values to NAs

is.na(winsor.data) <- sapply(winsor.data, is.infinite)



# convert top 10 percent values to 90th percentile

quantile(winsor.data$leverageAfterCrowdfunding, 0.90, na.rm=T)
quantile(winsor.data$leverageBeforeCrowdfunding, 0.90, na.rm=T)
quantile(winsor.data$averageAssets, 0.90, na.rm=T)



# winsorize leverage after CF

j <- 1
for (i in winsor.data$leverageAfterCrowdfunding){
  if(i > 0.8518847){ winsor.data$leverageAfterCrowdfunding[j] <- 0.8518847  } else { }
  j = j + 1
}



# winsorize leverage before CF

j <- 1
for (i in winsor.data$leverageBeforeCrowdfunding){
  if(i > 0.9003865){ winsor.data$leverageBeforeCrowdfunding[j] <- 0.9003865  } else { }
  j = j + 1
}



# winsorize average assets 

j <- 1
for (i in winsor.data$averageAssets){
  if(i > 2373.414){ winsor.data$averageAssets[j] <- 2373.414  } else { }
  j = j + 1
}



# regression

use.variables = c( "BvD.major.sector", "leverageAfterCrowdfunding", "leverageBeforeCrowdfunding", "isCrowdfunded", "averageAssets" )
lm.Data = winsor.data[ use.variables]
lm.Data = lm.Data[ complete.cases(lm.Data[ use.variables]), use.variables ]




# for the corrplot, and joint hypothesis test of the category variable, it is handy to make dummy variables for the sector (instead of including it in the model as a factor)

lm.Data$BvD.major.sector = factor( lm.Data$BvD.major.sector, sort(unique(lm.Data$BvD.major.sector)) )



# dummies_sector = dummy_cols( lm.Data$BvD.major.sector, remove_first_dummy = T, sort_columns = T )

dummies_sector = dummy.code( lm.Data$BvD.major.sector )



# as a baseline sector in the regression, we use 'Banks'

lm.Data2 = lm.Data[ , setdiff(colnames(lm.Data), 'BvD.major.sector')] 
lm.Data2 = cbind( lm.Data2, dummies_sector  )



# corrplot to gain an insight into the strongest correlations

corr.data = cor( lm.Data2 )
rownames(corr.data) = colnames( corr.data ) = substring( colnames(corr.data), 1, 15 ) # shorter names so the figure becomes more clear

corrplot( corr.data )

summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 1 ])
summary( lm.Data2$averageAssets[ lm.Data2$isCrowdfunded == 0 ])



lm.Data2 = lm.Data2[, setdiff( colnames( lm.Data2), c( "Banks") )]
model1 <- lm( data = lm.Data2, leverageAfterCrowdfunding ~ . )
coefmodel1 <- summary( model1 )
coefmodel1


plot( lm.Data2$leverageAfterCrowdfunding )
lines( model1$fitted.values)





################################# HYPOTHESIS 2 #################################

# load firm sample as chosen

totalData <- data_hyp_2



# select CF firms 

equity.vs.loan = totalData[ which( totalData$isCrowdfunded == 1  ), ]
equity.vs.loan$amountCrowdfunded = ifelse( equity.vs.loan$isEquityCrowdfunded == 1, equity.vs.loan$Invested, equity.vs.loan$Target.value)

use.variables = c( "BvD.major.sector", "leverageBeforeCrowdfunding", "leverageAfterCrowdfunding", "isEquityCrowdfunded", "averageAssets", "Share", "Funding.year" )
equity.vs.loan = equity.vs.loan[ , use.variables]



# for our regression, we want to compare 'equity' share with debt-crowdfunded firms; so for debt-crowdfunded firms we set equity to zero

equity.vs.loan$Share[ is.na( equity.vs.loan$Share)] = 0
equity.vs.loan = equity.vs.loan[ complete.cases(equity.vs.loan), ]



# trim variables 

quantile( equity.vs.loan$leverageAfterCrowdfunding, 0.90 )
quantile( equity.vs.loan$leverageBeforeCrowdfunding, 0.90, na.rm=T )
quantile( equity.vs.loan$averageAssets, 0.90 )
#quantile( equity.vs.loan$amountCrowdfunded, 0.90 )

equity.vs.loan = equity.vs.loan[ which( equity.vs.loan$leverageAfterCrowdfunding <= 0.8499792 ), ]
equity.vs.loan = equity.vs.loan[ which( equity.vs.loan$leverageBeforeCrowdfunding <= 0.9411115 ), ]
equity.vs.loan = equity.vs.loan[ which( equity.vs.loan$averageAssets <= 1951.407 ), ]
#equity.vs.loan = equity.vs.loan[ which( equity.vs.loan$amountCrowdfunded <= 465000 ), ]



# create sector dummies

equity.vs.loan$BvD.major.sector = factor( equity.vs.loan$BvD.major.sector, sort(unique(equity.vs.loan$BvD.major.sector)) )

dummies_sector = dummy.code( equity.vs.loan$BvD.major.sector )



# as a baseline sector in the regression, I use 'Banks'

equity.vs.loan2 = equity.vs.loan[ , setdiff(colnames(equity.vs.loan), 'BvD.major.sector')] 
equity.vs.loan2 = cbind( equity.vs.loan2, dummies_sector  )



# H2: Is there a difference between equity and debt crowd funding?


# to gain an insight into the strongest correlations, I make a corrplot

corr.data = cor( equity.vs.loan2 )
rownames(corr.data) = colnames( corr.data ) = substring( colnames(corr.data), 1, 15 ) #shorter names so the figure becomes more clear
corrplot( corr.data)



# method 1: use binary variable to distinguish between equity and debt funding 

# remove any variables to check different models or to avoid the dummy trap

equity.vs.loan3 = equity.vs.loan2[, setdiff( colnames( equity.vs.loan2), c( "Banks", "Share") )]

model2 <- lm( data = equity.vs.loan3, leverageAfterCrowdfunding ~ . )
coefmodel2 <- summary( model2 )
coefmodel2


plot( equity.vs.loan3$leverageAfterCrowdfunding )
lines( model2$fitted.values)



# method 2: using a linear function of equity share instead of an 'either/or' equity funding

# remove any variables to check different models or to avoid the dummy trap

equity.vs.loan4 = equity.vs.loan2[, setdiff( colnames( equity.vs.loan2), c( "Banks", "isEquityCrowdfunded") )]

model2b <- lm( data = equity.vs.loan4, leverageAfterCrowdfunding ~ . )
coefmodel2b <- summary( model2b )
coefmodel2b





################################# HYPOTHESIS 3 T-TESTS #################################

# Hypothesis 3: younger firms show higher leverage than older firms



# Create date of incorporation brackets: 2010-2017, 2000-2009, < 2000 to categorize firms 

equity.firms <- totalData[ which( totalData$isEquityCrowdfunded == 1  ), ]
debt.firms <- totalData[ which( totalData$isDebtCrowdfunded == 1  ), ]

data_hyp3 = rbind(equity.firms, debt.firms)
table(data_hyp3$Founding.Date)



# between 2010-2017

data_hyp3$youngFirms = ifelse( data_hyp3$Founding.Date >= 10 & data_hyp3$Founding.Date <= 18, 1, 0 ) 

# between 2000 and 2009

data_hyp3$matureFirms = ifelse( data_hyp3$Founding.Date >= 0 & data_hyp3$Founding.Date <= 9, 1, 0  )

# earlier than 2000

data_hyp3$oldFirms = ifelse( data_hyp3$Founding.Date > 18, 1, 0 ) 



# only select firms where we know the leverage in the year after crowdfunding

data_hyp3 = data_hyp3[ which( !is.na( data_hyp3$leverageAfterCrowdfunding )), ]



# trim data 

quantile(data_hyp3$leverageAfterCrowdfunding, 0.90, na.rm=T)
#quantile(data_hyp3$leverageBeforeCrowdfunding, 0.98, na.rm=T)
#quantile(data_hyp3$averageAssets, 0.98, na.rm=T)

data_hyp3 = data_hyp3[ which( data_hyp3$leverageAfterCrowdfunding <= 0.8487567 ), ]
#data_hyp3 = data_hyp3[ which( data_hyp3$leverageBeforeCrowdfunding <= 0.9344049 ), ]
#data_hyp3 = data_hyp3[ which( data_hyp3$averageAssets <= 1941.422 ), ]



# null hypotehsis in a t-test is always that there is NO difference between groups (so exactly opposite of hypothesis 3)
# so in order to confirm hypotehsis 3, we need to REJECT the null hypotehsis of the t-test (so p < 0.05)


# the hypothesis that young firms do NOT show higher leverage than mature firms is REJECTED (this means that Hypothesis 3 is confirmed in this case)
# I can see this from the p-value of 4.152e-07 (significant at 1% level). The difference in means is 0.2392299 - 0.1606417 = 0.0785882

t.test( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)], data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$matureFirms == 1)], var.equal=F, alternative="greater" )
wilcox.test( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)], data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$matureFirms == 1)], var.equal=F, alternative="greater" )



# same test between young firms and old firms. Here we see that we cannot reject the hypothesis. In fact, old firms have even higher leverage than young and mature firms

t.test( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)], data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$oldFirms == 1)], var.equal=F, alternative="greater" )
wilcox.test( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)], data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$oldFirms == 1)], var.equal=F, alternative="greater" )



# conclusion: it seems that the oldest companies have the highest leverage, mature companies have the lowest leverage, and young firms have in-between leverage
# a final test to combine mature and old firms and compare them to young firms



# young firms have higher leverage than mature and old companies combined. That is because there are many more mature firms than old firms (mature firms have a bigger impact on the combined average)

t.test( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)], data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms != 1)], var.equal=F, alternative="greater" )
wilcox.test( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)], data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms != 1)], var.equal=F, alternative="greater" )



# see: there are not so many old firms, which is why they don't have such a big impact on the combined t-test

length( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$youngFirms == 1)] )
length( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$matureFirms == 1)] )
length( data_hyp3$leverageAfterCrowdfunding[ which( data_hyp3$oldFirms == 1)] )



### Conclusion: young firms indeed show higher leverage than old firms (as measured by long-term debt in the years 2016 and 2017,
# where for crowdfunded firms we consider leverage in the year after funding and for non-crowdfunded firms we consider the average leverage in 2016 and 2017
# this definition of leverage is chosen to remain consistent with hypothesis 1 and 2





################################# HYPOTHESIS 4 #################################

## Hypothesis 4: There is a positive relationship between firm size and leverage 

data_hyp4 = data_hyp3

# check trimming

# trim data


# I already trimmed the data in h3



#data_hyp4 = data_hyp4[ which( data_hyp4$leverageAfterCrowdfunding <= 1.433607 ), ]

#data_hyp4 = data_hyp4[ which( data_hyp4$averageAssets <= 1933.932 ), ]



# make groups based on assets

data_hyp4$assets.Low25 = ifelse( data_hyp4$averageAssets < quantile(data_hyp4$averageAssets, 0.25 ), 1, 0 )
data_hyp4$assets.Middle50 = ifelse( data_hyp4$averageAssets > quantile(data_hyp4$averageAssets, 0.25 ) & data_hyp4$averageAssets < quantile(data_hyp4$averageAssets, 0.75 ), 1, 0 )
data_hyp4$assets.High25 = ifelse( data_hyp4$averageAssets > quantile(data_hyp4$averageAssets, 0.75 ), 1, 0 )



# only select firms where we know the leverage in the year after crowdfunding

data_hyp4 = data_hyp4[ which( !is.na( data_hyp4$leverageAfterCrowdfunding )), ]



# firms with assets in the middle 50% show higher leverage than firms with assets in the bottom 25% (0.212 leverage versus 0.164 leverage), p value < 0.01

t.test( data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Middle50 == 1 )],data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Low25 == 1 )], var.equal=FALSE, alternative = "greater"  )
wilcox.test( data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Middle50 == 1 )],data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Low25 == 1 )], var.equal=FALSE, alternative = "greater"  )



# firms with assets in the high 25% show higher leverage than firms with assets in the bottom 25% (0.209 leverage versus 0.164 leverage), p value < 0.05

t.test( data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.High25 == 1 )],data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Low25 == 1 )], var.equal=FALSE, alternative = "greater"  )
wilcox.test( data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.High25 == 1 )],data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Low25 == 1 )], var.equal=FALSE, alternative = "greater"  )



# firms with assets in the high 25% show no difference in leverage with the middle 50%

t.test( data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.High25 == 1 )],data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Middle50 == 1 )], var.equal=FALSE, alternative = "greater"  )
wilcox.test( data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.High25 == 1 )],data_hyp4$leverageAfterCrowdfunding[ which( data_hyp4$assets.Middle50 == 1 )], var.equal=FALSE, alternative = "greater"  )



# conclusion: Firm size is positively correlated to leverage (as we already confirmed in the process of hypothesis 1)
# so this confirms hypothesis 4. The effect is only statistically different between 'small' firms and 'medium/large' firms
# but there is no more significant difference between medium and large firms. So as firms get reasonably large, they start to be more leveraged
# but the leverage does not increase indefinitely.





################################# ADDITIONAL ANALYSIS #################################

# extra analysis: aggregate by platform and make a table with one-by-one t-tests and overall f test
# hypothesis: There is a positive relationship between platform reputation and leverage



table( totalData$Platform )
platformData = totalData[ which( !is.na( totalData$Platform ) ), ]



# I can only perform t-tests for platforms with 'enough' observations to obtain reliable results
# so we select only platforms with 8 platforms minimum, which is chosen quite arbitrarily


platformTable = sort( table(platformData$Platform) )



platformData = platformData[ platformData$Platform %in% names( which( platformTable >= 8) ), ]
platformData = platformData[ which( !is.na( platformData$leverageAfterCrowdfunding)), ]
sort( table(platformData$Platform) )



# net returns platforms 

# fundingcircle 6 percent
# geldvoorelkaar 5 percent 
# kapitaalopmaat 4 percent
# collincrowdfund 4 percent 



# perform t-tests

# test fundingcircle against geldvoorelkaar, kapitaalopmaat and collincrowdfund
# test geldvoorelkaar against kapitaalopmaat and collincrowdfund
# test kapitaalopmaat against collincrowdfund 



# analysis

Fundingcircle = platformData[ platformData$Platform %in% c( "Fundingcircle"), "leverageAfterCrowdfunding" ]
Geldvoorelkaar = platformData[ platformData$Platform %in% c( "Geldvoorelkaar"), "leverageAfterCrowdfunding" ]
Kapitaalopmaat = platformData[ platformData$Platform %in% c( "Kapitaalopmaat"), "leverageAfterCrowdfunding" ]
Collincrowdfund = platformData[ platformData$Platform %in% c( "Collincrowdfund"), "leverageAfterCrowdfunding" ]
#Investormatch = platformData[ platformData$Platform %in% c( "Investormatch"), "leverageAfterCrowdfunding" ]
#Duurzaaminvesteren = platformData[ platformData$Platform %in% c( "Duurzaaminvesteren"), "leverageAfterCrowdfunding" ]



# compare mean leverage in the year after CF per platform pair
# if reputation platform 1 > platform 2, then one sided test 'greater'
# otherwise, reputation platform 1 = platform 2, two-sided t-test to see if one of them is sign. larger than the other



# fundingcircle highest yield, so test mean against all other 3 platforms

t.test( Fundingcircle, Kapitaalopmaat, var.equal= FALSE, alternative="greater", conf.level = 0.95) #No
t.test( Fundingcircle, Collincrowdfund, var.equal= FALSE, alternative="greater", conf.level = 0.95) #No
t.test( Fundingcircle, Geldvoorelkaar, var.equal= FALSE, alternative="greater", conf.level = 0.95) #No



# similar testing for geldvoorelkaar, test against kapitaalopmaat and collincrowdfund

t.test( Geldvoorelkaar, Kapitaalopmaat, var.equal= FALSE, alternative="greater", conf.level = 0.95) #No
t.test( Geldvoorelkaar, Collincrowdfund, var.equal= FALSE, alternative="greater", conf.level = 0.95) #No



# finally for kapitaalopmaat and collincrowdfund, which have equal yields, a two-sided t-test

t.test( Kapitaalopmaat, Collincrowdfund, var.equal= FALSE, alternative="two.sided", conf.level = 0.95) #No



# are there ANY differences between these 5 platforms?

# for this ANOVA (one-way analysis of variance) is used, which is the generalization of a two-sample t-test
# it compares the means over the 5 groups simultaneously. It will tell us whether there is any difference in means between these platforms
# see: http://www.sthda.com/english/wiki/one-way-anova-test-in-r


# null hypothesis: the means of the different groups are the same
# alternative hypothesis: At least one sample mean is not equal to the others.


platformSelect = c( "Fundingcircle", "Geldvoorelkaar", "Kapitaalopmaat", "Collincrowdfund" )

platformData = platformData[ which( platformData$Platform %in% platformSelect ), ]



# calculate the analysis of variance
res.aov <- aov( leverageAfterCrowdfunding ~ Platform, data = platformData) # no significant differences between groups (Null hypothesis not rejected)
summary( res.aov)



# conclusion additional analysis: It seems that there is no influence of the platform reputation on leverage after the crowdfunding year

