
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Thesis/Data/Script")


################################# NON-RANDOM CONTROL GROUPS #################################

# in this file, I generate 2 non-random control samples
# who are equal in terms of FIRM SIZE (measured in total assets) and SECTOR to CF firm sample


# first compute the current sector distribution of CF firms

sector.distribution.CF <- table( totalData$BvD.major.sector)/sum(table( totalData$BvD.major.sector)) # distribution factors

sector.distr.values <- c( 0.010899183, 0.010899183, 0.021798365, 0.040871935, 0.008174387, 0.021798365,
                          0.065395095, 0.027247956, 0.010899183, 0.525885559, 0.002724796, 0.016348774,
                          0.002724796, 0.010899183, 0.008174387, 0.212534060, 0.002724796 )

overview.sectors = data.frame( "Sector" = names(sector.distribution.CF),
                               "distribution" = sector.distr.values )

overview.sectors = overview.sectors %>% mutate(N.firms.CF = round(overview.sectors$distribution*nrow(totalData)))
overview.sectors # overview of sector distribution 



# secondly, compute the asset quantiles of CF firms 

d.cf = data.frame()

for( Sector in overview.sectors$Sector ) {
  assets2 = totalData[ which( totalData$BvD.major.sector == Sector ), ]
  assets = assets2$averageAssets
  
  
  q.assets <- quantile(assets, prob = seq(0, 1, length = 5), type = 5, na.rm = TRUE)
  
  
  for (i in 1:5) {
    d.cf[Sector,i] <- q.assets[[i]]
  }
  
}


d.distr.sector.quant <- cbind(d.cf[[1]], d.cf[[2]], d.cf[[3]], d.cf[[4]], d.cf[[5]]) %>% as.data.frame()
colnames(d.distr.sector.quant) <- c("Begin.value", "Q1", "Q2", "Q3", "Q4")
d.distr.sector.quant #here, the quantile distances are summarized 



# continue with computing how many firms we want for each quartile
# we generate two control groups with different sample sizes


N.control.group.1 <- 367 # control group 1, 1:1 to size of CF sample 
N.control.group.2 <- 3670 # control group 2, 1:10 to size of CF sample 





################################# NON-RANDOM CONTROL GROUP 1 #################################

# 1 to 1 to CF firms



# determine the sector distribution

overview.sectors = overview.sectors %>% mutate(N.desired.control.sample = round(overview.sectors$distribution * N.control.group.1))
overview.sectors # a new column is added with the size of the desired control group for each sector



# now determine the size of CF firms for each quantile

n.cf.class <- overview.sectors$N.desired.control.sample / 4
r.classes <- round(n.cf.class)



# I have the problem that the sum of each quartiel is not in all cases equal to a rounded number, therefore I assume here to 
# round the value in the first category, because mainly all firms are small, therefore removing a small firms has less influence
# to final analysis then removing/add a large firm

first.value <- round(overview.sectors$N.desired.control.sample - (r.classes *3))

r.classes - first.value # if this number is unequal to zero, the first value is different from the other classes due to rounding

sum((r.classes*3) + first.value) # if this sum is unequal to desired control group size, check rounding



desired.d.contr.group <- data.frame("Sector" = names(sector.distribution.CF),
                                    "Q1" = first.value,
                                    "Q2" = r.classes, 
                                    "Q3" = r.classes,
                                    "Q4" = r.classes)

# this overview shows the number of firms for each quartile 



# now, the magic is happening: in next for loop the control group will be generated

non.random_control_group1 = data.frame()
j <- 1

for( Sector in overview.sectors$Sector ){
  sector.firms = control_group[ which( control_group$BvD.major.sector == Sector ), ]
  
  #print(Sector)
  
  q1.firms <- sector.firms[ which( sector.firms$averageAssets >= (d.distr.sector.quant$Begin.value[j] %>% as.numeric()) & sector.firms$averageAssets <= (d.distr.sector.quant$Q1[j] %>% as.numeric())) , ] 
  q2.firms <- sector.firms[ which( sector.firms$averageAssets >= (d.distr.sector.quant$Q1[j] %>% as.numeric()) & sector.firms$averageAssets <= (d.distr.sector.quant$Q2[j] %>% as.numeric())) , ] 
  q3.firms <- sector.firms[ which( sector.firms$averageAssets >= (d.distr.sector.quant$Q2[j] %>% as.numeric()) & sector.firms$averageAssets <= (d.distr.sector.quant$Q3[j] %>% as.numeric())) , ] 
  q4.firms <- sector.firms[ which( sector.firms$averageAssets >= (d.distr.sector.quant$Q3[j] %>% as.numeric()) & sector.firms$averageAssets <= (d.distr.sector.quant$Q4[j] %>% as.numeric())) , ] 
  
  if(desired.d.contr.group$Q1[j] > 0){ q1 <- q1.firms[1:desired.d.contr.group$Q1[j],] } else { q1 = NULL } 
  if(desired.d.contr.group$Q2[j] > 0){ q2 <- q2.firms[1:desired.d.contr.group$Q2[j],] } else { q2 = NULL } 
  if(desired.d.contr.group$Q3[j] > 0){ q3 <- q3.firms[1:desired.d.contr.group$Q3[j],] } else { q3 = NULL }
  if(desired.d.contr.group$Q4[j] > 0){ q4 <- q4.firms[1:desired.d.contr.group$Q4[j],] } else { q4 = NULL } 

  
  #print( paste0( "Sector: ", j, " - ", nrow(rbind(q1,q2,q3,q4)) ))
  non.random_control_group1 <- rbind(non.random_control_group1, q1, q2, q3, q4)
  
  #print(j)
  j = j + 1
  
}


# last, add firms where only 1 is available for each sector, we extend the asset class a bit to select the nearest firm 

non.random_control_group1 = non.random_control_group1[ which( !( is.na( non.random_control_group1$Total.assets.th.EUR.2017 ) )  ) , ]



# Post & telecommunications, 11

post.firms.1 <- control_group[ which( control_group$BvD.major.sector == "Post & telecommunications" ), ]

d.distr.sector.quant$Begin.value[11] # change quartile boundaries below until we have 10 firms in this class
d.distr.sector.quant$Q1[11]

nrow(post.firms.1[ which( post.firms.1$averageAssets >= (4600.421) & post.firms.1$averageAssets <= (4700.421)) , ])

post.firms.left1 <- post.firms.1[ which( post.firms.1$averageAssets > (4600.421) & post.firms.1$averageAssets < (4700.421)) , ]

non.random_control_group1 <- rbind(non.random_control_group1, post.firms.left1)



# Publishing, printing, 13

publ.print.1 <- control_group[ which( control_group$BvD.major.sector == "Publishing, printing" ), ]

d.distr.sector.quant$Begin.value[13] # change quartile boundaries below until we have 10 firms in this class
d.distr.sector.quant$Q1[13]

nrow(publ.print.1[ which( publ.print.1$averageAssets >= (1280.200) & publ.print.1$averageAssets <= (1284.415)) , ])

publ.print.left1 <- publ.print.1[ which( publ.print.1$averageAssets > (1280.200) & publ.print.1$averageAssets < (1284.415)) , ]

non.random_control_group1 <- rbind(non.random_control_group1, publ.print.left1)



# Wood, cork, paper, 17

wood.cork.1 <- control_group[ which( control_group$BvD.major.sector == "Wood, cork, paper" ), ]

d.distr.sector.quant$Begin.value[17] # change quartile boundaries below until we have 10 firms in this class
d.distr.sector.quant$Q1[17]

nrow(wood.cork.1[ which( wood.cork.1$averageAssets >= (12.346) & wood.cork.1$averageAssets <= (12.946)) , ])

wood.cork.left1 <- wood.cork.1[ which( wood.cork.1$averageAssets > (12.346) & wood.cork.1$averageAssets < (12.946)) , ]

non.random_control_group1 <- rbind(non.random_control_group1, wood.cork.left1)





################################# NON-RANDOM CONTROL GROUP 2 #################################

# 1 to 10 to CF firms



# determine the sector distribution

overview.sectors = overview.sectors %>% mutate(N.desired.control.sample = overview.sectors$distribution * N.control.group.2)
overview.sectors # a new column is added with the size of the desired control group for each sector



# now determine the size of CF firms for each quantile

n.cf.class <- overview.sectors$N.desired.control.sample / 4
r.classes <- round(n.cf.class)



# I have the problem that the sum of each quartiel is not in all cases equal to a rounded number, therefore I assume here to 
# round the value in the first category, because mainly all firms are small, therefore removing a small firms has less influence
# to final analysis then removing/add a large firm

first.value <- round(overview.sectors$N.desired.control.sample - (r.classes *3))

r.classes - first.value # if this number is unequal to zero, the first value is different from the other classes due to rounding

sum((r.classes*3) + first.value) # if this sum is unequal to desired control group size, check rounding



desired.d.contr.group <- data.frame("Sector" = names(sector.distribution.CF),
                                    "Q1" = first.value,
                                    "Q2" = r.classes, 
                                    "Q3" = r.classes,
                                    "Q4" = r.classes)

# this overview shows the number of firms for each quartile 



# now, the magic is happening, in next for loop the control group will be generated

non.random_control_group2 = data.frame()
j <- 1


for( Sector in overview.sectors$Sector ){
  sector.firms = control_group[ which( control_group$BvD.major.sector == Sector ), ]
  
  #print(Sector)
  
  q1.firms <- sector.firms[ which( sector.firms$averageAssets > (d.distr.sector.quant$Begin.value[j] %>% as.numeric()) & sector.firms$averageAssets < (d.distr.sector.quant$Q1[j] %>% as.numeric())) , ] 
  q2.firms <- sector.firms[ which( sector.firms$averageAssets > (d.distr.sector.quant$Q1[j] %>% as.numeric()) & sector.firms$averageAssets < (d.distr.sector.quant$Q2[j] %>% as.numeric())) , ] 
  q3.firms <- sector.firms[ which( sector.firms$averageAssets > (d.distr.sector.quant$Q2[j] %>% as.numeric()) & sector.firms$averageAssets < (d.distr.sector.quant$Q3[j] %>% as.numeric())) , ] 
  q4.firms <- sector.firms[ which( sector.firms$averageAssets > (d.distr.sector.quant$Q3[j] %>% as.numeric()) & sector.firms$averageAssets < (d.distr.sector.quant$Q4[j] %>% as.numeric())) , ] 
  
  q1 <- q1.firms[1:desired.d.contr.group$Q1[j],]
  q2 <- q2.firms[1:desired.d.contr.group$Q2[j],]
  q3 <- q3.firms[1:desired.d.contr.group$Q3[j],]
  q4 <- q4.firms[1:desired.d.contr.group$Q4[j],]
  
  non.random_control_group2 <- rbind(non.random_control_group2, q1, q2, q3, q4)
  
  #print(j)
  j = j + 1
  
}



# select 10 firms for the 3 sectors who have only 1 value, just as previous control group (1)

non.random_control_group2 <- non.random_control_group2[ which( !( is.na( non.random_control_group2$Total.assets.th.EUR.2017 ) )  ) , ]



# Post & telecommunications, 11

post.firms.2 <- control_group[ which( control_group$BvD.major.sector == "Post & telecommunications" ), ]

d.distr.sector.quant$Begin.value[11] # change quartile boundaries below until we have 10 firms in this class
d.distr.sector.quant$Q1[11]

nrow(post.firms.2[ which( post.firms.2$averageAssets >= (4411.421) & post.firms.2$averageAssets <= (5411.421)) , ])

post.firms.left <- post.firms.2[ which( post.firms.2$averageAssets > (4411.421) & post.firms.2$averageAssets < (5411.421)) , ]

non.random_control_group2 <- rbind(non.random_control_group2, post.firms.left)



# Publishing, printing, 13

publ.print.2 <- control_group[ which( control_group$BvD.major.sector == "Publishing, printing" ), ]

d.distr.sector.quant$Begin.value[13] # change quartile boundaries below until we have 10 firms in this class
d.distr.sector.quant$Q1[13]

nrow(publ.print.2[ which( publ.print.2$averageAssets >= (1267.415) & publ.print.2$averageAssets <= (1291.415)) , ])

publ.print.left <- publ.print.2[ which( publ.print.2$averageAssets > (1267.415) & publ.print.2$averageAssets < (1291.415)) , ]

non.random_control_group2 <- rbind(non.random_control_group2, publ.print.left)



# Wood, cork, paper, 17

wood.cork.2 <- control_group[ which( control_group$BvD.major.sector == "Wood, cork, paper" ), ]

d.distr.sector.quant$Begin.value[17] # change quartile boundaries below until we have 10 firms in this class
d.distr.sector.quant$Q1[17]

nrow(wood.cork.2[ which( wood.cork.2$averageAssets >= (9.546) & wood.cork.2$averageAssets <= (14.946)) , ])

wood.cork.left <- wood.cork.2[ which( wood.cork.2$averageAssets > (9.546) & wood.cork.2$averageAssets < (14.946)) , ]

non.random_control_group2 <- rbind(non.random_control_group2, wood.cork.left)



