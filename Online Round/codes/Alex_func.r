###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting directory depending on a user
if (Sys.info()[8] == "lizzzi111")           {setwd("~/Documents/DSG_2017_Finals/")} 
if (Sys.info()[8] == "kozodoi")             {setwd("~/Documents/Competitions/DSG_2017_Finals/")}
if (Sys.info()[8] == "nataliasverchkova")   {setwd("~/Documents/DSG/DSG_2017_Finals/")}
if (Sys.info()[8] == "oleksiyostapenko")    {setwd("/Users/oleksiyostapenko/Documents/HU_Berlin/ML/DSG/DSG_2017_Finals")}

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if (require(pacman) == FALSE) install.packages("pacman")
library(pacman)
p_load(dplyr, data.table, caret, Metrics, xgboost, vtreat, plyr)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "code_0_parameters.R"))


###################################
#                                 #
#         CREATING FEATURES       #
#                                 #
###################################

# loading data
load(file.path(data.folder, "data_known_noNAs_first"))
load(file.path(data.folder, "data_unknown.rda"))


# Comment Alex: need store the labels of the test subset in a seperate vector first and set the label column to NAs#
#==================================================================================================================#


##### CODES FOR NEW FEATURES: LIZA


# ##### CODES FOR NEW FEATURES: NATALIA
# data_known[(grep('COMP_PRICE', colnames(data_known)))] <- apply(data_known[(grep('COMP_PRICE', colnames(data_known)))], 1, function(x) ifelse(is.na(x), 0, x))
# data_known = data.table(data_known)
# data_known[is.na(PRICE), PRICE :=0 ]
# 
# data_known[is.na(Name_Of_Competitor), Name_Of_Competitor :=0 ]
# data_known[is.na(Comp_reference_number), Comp_reference_number :=0 ]


###
data_known_physical = data_known %>% filter( PL=="c2Z5"  ) %>% select(ID, Material, PL, Volume, Gross_Weight, Length, Width, Height) 

table( data_known$PL  )

set.seed(20)
wss5 <- sapply(c(4,5,6,7,8,9,10,11,15,20,25,30),function(k){
  print(k)
  kmeans(data_known_physical[,4:8], k, nstart=10 )$tot.withinss})

plot( c(4:11,15, 20, 25, 30), wss5,
      type="b", pch = 19, frame = FALSE,
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares")


clusters = kmeans(data_known_physical[,4:8], 8, nstart = 100)
data_known_physical$Cluster=clusters$cluster

data_known[PL=="c2Z5", Cluster_PL := data_known_physical$Cluster]
data_known[PL=="c2Z5", Cluster := paste("c2Z5", Cluster_PL, sep = "_") ]






#
setkey(data_known, ID)
setkey(data_known_physical, ID)
data_known_merged = data.table::merge(data_known, data_known_physical, all.x = TRUE)




# table(data_known_group_9CJ3$Cluster, data_known_group_9CJ3$DP_FAMILY_CODE)



#PRISE
table(is.na(data_known$PRICE))
data_known = data.table(data_known)

library(lubridate)
data_known[, `:=`  (month = month(First_MAD),  year = year(First_MAD)) ]

data_known[, mean_sprise_tmp:=mean(PRICE, na.rm = TRUE), by=.(Material, year, month)]
data_known[is.na(PRICE), PRICE:=mean_sprise_tmp ]


data_known[, mean_sprise_tmp:=mean(PRICE, na.rm = TRUE), by=.(Material, month)]
data_known[is.na(PRICE), PRICE:=mean_sprise_tmp ]


data_known[, mean_sprise_tmp:=mean(PRICE, na.rm = TRUE), by=.(Material, year)]
data_known[is.na(PRICE), PRICE:=mean_sprise_tmp ]


data_known[, mean_sprise_tmp:=mean(PRICE, na.rm = TRUE), by=.(Material)]
data_known[is.na(PRICE), PRICE:=mean_sprise_tmp ]


data_known[, mean_sprise_tmp:=mean(PRICE, na.rm = TRUE), by=.(PL, DP_FAMILY_CODE)]
data_known[is.na(PRICE), PRICE:=mean_sprise_tmp ]


data_known[, mean_sprise_tmp:=mean(PRICE, na.rm = TRUE), by=.(PL, DP_FAMILY_CODE)]
data_known[is.na(PRICE), PRICE:=mean_sprise_tmp ]

data_known$mean_shortage_tmp=NULL

#
data_known %>% filter(is.na(PRICE), !is.na(COMP_PRICE_AVG)) %>% select(Material)







##### CODES FOR NEW FEATURES: NIKITA


##### CODES FOR NEW FEATURES: OLEKS

#NAs handling AGE_ZN_ZI_years
data_known = data.table(data_known)
data_known = data_known[is.na(Age_ZN_ZI_years), Age_ZN_ZI_years:=  as.numeric(difftime(as.POSIXct("2017-09-25" ), min((as.POSIXct( First_MAD ))), units = "days")/365)  , by=.(Material)]

# CBO_CBO_Qty_Shortage
data_known[, mean_shortage_tmp:=median(CBO_CBO_Qty_Shortage, na.rm = TRUE), by=.(Material, Plant) ]
data_known[is.na(CBO_CBO_Qty_Shortage), CBO_CBO_Qty_Shortage:=mean_shortage_tmp ]
data_known$mean_shortage_tmp=NULL


data_known[, mean_shortage_tmp:=median(CBO_CBO_Qty_Shortage, na.rm = TRUE), by=.(Material) ]
data_known[is.na(CBO_CBO_Qty_Shortage), CBO_CBO_Qty_Shortage:=mean_shortage_tmp ]
data_known$mean_shortage_tmp=NULL


data_known[, mean_shortage_tmp:=median(CBO_CBO_Qty_Shortage, na.rm = TRUE), by=.(Plant) ]
data_known[is.na(CBO_CBO_Qty_Shortage), CBO_CBO_Qty_Shortage:=mean_shortage_tmp ]
data_known$mean_shortage_tmp=NULL


# PROD_LINE DP_FAMILI ...
data_known[DP_FAMILY_CODE=="", DP_FAMILY_CODE:="None",]
data_known[DP_FAMILY_CODE=="None", SUBRANGE:="None",]

#
data_known[PRODUCT_STATUS=="", PRODUCT_STATUS:="None",]
data_known[PRODUCT_STATUS=="None", ORIGINAL_SUPPLIER:="None",]


data_known[DP_FAMILY_CODE=="None", DP_FAMILY_CODE:=paste( PL,"None")]
data_known[DP_FAMILY_CODE=="None", SUBRANGE:=paste( PL, DP_FAMILY_CODE, "None")]


# 
# ff1 = bbb %>% group_by(PL, DP_FAMILY_CODE, SUBRANGE ) %>% summarise (sd = sd(Volume), min_vol = min(Volume), first_q = quantile(Volume, 0.25), mean_vol = mean(Volume),median_vol = median(Volume), third_q = quantile(Volume, 0.75),    max_vol = max(Volume))    
# ff2 = bbb %>% group_by(PL) %>% summarise (sd = sd(Volume), min_vol = min(Volume), first_q = quantile(Volume, 0.25), mean_vol = mean(Volume),median_vol = median(Volume), third_q = quantile(Volume, 0.75),    max_vol = max(Volume) ) 
# ff3 = bbb %>% group_by(PL) %>% filter(DP_FAMILY_CODE=="None")  %>% summarise (sd = sd(Volume), min_vol = min(Volume), first_q = quantile(Volume, 0.25), mean_vol = mean(Volume),median_vol = median(Volume), third_q = quantile(Volume, 0.75),    max_vol = max(Volume) )
# ff4 = bbb %>% group_by(SUBRANGE) %>% summarise (sd = sd(Volume), min_vol = min(Volume), first_q = quantile(Volume, 0.25), mean_vol = mean(Volume),median_vol = median(Volume), third_q = quantile(Volume, 0.75),    max_vol = max(Volume) ) 


####### DP_FAMILY_CODE, SUBRANGE

#Correct LT NAs
#data_known = data.table(data_known)
#data_known[ , LT_mean := .N, by=.(Material) ]
data_known[694525,]$LT=0


#MOQ
data_known[ Volume < 0.001 , MOQ_mean := mean(MOQ, na.rm = TRUE) ]
data_known[ is.na(MOQ) , MOQ := MOQ_mean]
data_known$MOQ_mean=NULL

#ROP
data_known[ Volume < 0.001 , ROP_mean := mean(ROP, na.rm = TRUE) ]
data_known[ is.na(ROP) , ROP := ROP_mean]
data_known$ROP_mean=NULL

#SafetyStk 694525
data_known[ Volume < 0.001 , SafetyStk_mean := mean(SafetyStk, na.rm = TRUE) ]
data_known[ is.na(SafetyStk) , SafetyStk := SafetyStk_mean]
data_known$SafetyStk_mean=NULL
########
################
########################
################################



which(is.na(data_known$CBO_CBO_Qty_Shortage))

#columns with NAs
names(which(colSums(is.na(data_known))>0))


save(data_known,   file = file.path(data.folder, "data_known_noNAs_first.rda")) 





#prod_line 9CJ3 8 clusters 

# 
# data_known_group_9CJ3 = data_known %>% filter(PL == "9CJ3") %>%  select(Material, DP_FAMILY_CODE, Volume, Gross_Weight, Length, Width, Height) 
# 
# table(data_known_group_9CJ3$cluster, data_known_group_9CJ3$DP_FAMILY_CODE)
# 
# 
# set.seed(20)
# wss5 <- sapply(c(4,5,6,7,8,9,10,11,15,20),function(k){
#   print(k)
#   kmeans(data_known_group_9CJ3[,3:7], k, nstart=10 )$tot.withinss})
# 
# plot( c(4:11,15, 20), wss5,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares") 
# 
# 
# clusters = kmeans(data_known_group_9CJ3[,3:7], 10, nstart = 20)
# data_known_group_9CJ3$Cluster=clusters$cluster

# table(data_known_group_9CJ3$Cluster, data_known_group_9CJ3$DP_FAMILY_CODE)








###################################
#                                 #
#        AUTOMATIC FEATURES       #
#                                 #
###################################

# data partitioning
data_train <- data_known[data_known$part == "train", ]
data_valid <- data_known[data_known$part == "valid", ]

# adding factor features (Nikita)
data_unknown <- add_factor_features(data_train, data_unknown, target = dv, smooth = 10)
data_known   <- add_factor_features(data_train, data_valid,   target = dv, smooth = 10)
data_train   <- data_known$train
data_valid   <- data_known$valid
data_unknown <- data_unknown$valid

# scaling data
data_unknown <- scale_data(data_train, data_unknown, type = "minmax", except = c(dv, ign_vars))
data_known   <- scale_data(data_train, data_valid,   type = "minmax", except = c(dv, ign_vars))
data_unknown <- data_unknown$valid
data_train   <- data_known$train
data_valid   <- data_known$valid

#adding moments per groups for correlated variabel (Alex) 
#don't use target as "corelated_real_var" since function does no smoothing
data_known <-  moments_per_group_on_real_corelated_var(data_train, data_valid, corelated_real_var = "Age", list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")))
data_train <- data_known$train
data_valid <- data_known$valid

#adding smoothed mean per groups (Alex)
# here we calculate the mean only for the target variable per groups 
data_known <-  smoothed_mean_per_group(data_train = data_train, data_valid = data_valid, target_name = dv, var_groups = list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")), alpha = 10)
data_train <- data_known$train
data_valid <- data_known$valid

# saving data as .RDA
save(data_train, file <- file.path(data.folder, "data_train_prepared.rda"))
save(data_valid, file <- file.path(data.folder, "data_valid_prepared.rda"))
