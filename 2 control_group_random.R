
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Thesis/Data/Script")


################################# RANDOM CONTROL GROUP #################################

# in this file, I create two randomly selected non-crowdfunded control groups





################################# RANDOM CONTROL GROUP 1 #################################

# control group 3, 1:1 to size of CF sample 

random_control_group.3 = control_group[ , ]

set.seed(3434)
select.random = sample(1:nrow(control_group), 367, replace=F )
random_control_group.3 = control_group[ select.random, ]





################################# RANDOM CONTROL GROUP 2 #################################

# control group 4, 1:10 to size of CF sample 

random_control_group.4 = control_group[ , ]

set.seed(3434)
select.random = sample(1:nrow(control_group), 3670, replace=F )
random_control_group.4 = control_group[ select.random, ]


