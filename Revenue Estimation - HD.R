### CLEAN VERSION ###

## Load packages, data, and set working directory

library(ggplot2)
library(dplyr)
library(lubridate)
library(plyr)
library(glmnet)
library(reshape)
library(openxlsx)

setwd("U:/Revenue Traceback/Key Data/System Revenue/Base Data Files")


######  LOAD ALL DATA SETS  #######

PJM.Plant.Data <- read.csv("PJM Supply Curve - Full.csv")
load.2015 <- read.csv("2015 Load - PJM.csv")
load.2014 <- read.csv("2014 Load - PJM.csv")
VRE.Factors <- read.csv("VRE Factors.csv")
LMP <- read.csv("PJM 2015 - LMP.csv")
LMP.2014 <- read.csv("PJM 2014 - LMP.csv")


### INPUTS
Capacity.Price <- 135 # In $/MW-day
Disc.Rate <- .08 # As a %
Years <- 30 
CRF <- Disc.Rate/(((1-(1+Disc.Rate)^-Years)))

Coal.Cap <- 4000 # All in $/KW
NG.Cap <- 1000
Hyd.Cap <- 3000
Nuc.Cap <- 6000
Bio.Cap <- 4000
Wind.Cap <- 650
Solar.Cap <- 400




## Clean and eliminiate missing data ##
PJM.Plant.Data <- PJM.Plant.Data[which(!is.na(PJM.Plant.Data$Plant.Key)),]





## Drop missing price data ##
PJM.Plant.Data$Marginal.Price <- as.numeric(as.character(PJM.Plant.Data$Marginal.Price))
PJM.Plant.Data <- PJM.Plant.Data[which(PJM.Plant.Data$Marginal.Price != "#VALUE!"),]
PJM.Plant.Data <- PJM.Plant.Data[which(PJM.Plant.Data$Marginal.Price != "DIV/0!"),]
PJM.Plant.Data$Marginal.Price <- as.numeric(as.character(PJM.Plant.Data$Marginal.Price))
PJM.Plant.Data$Fixed.O.M.Cost.... <- as.numeric(as.character(PJM.Plant.Data$Fixed.O.M.Cost....))
PJM.Plant.Data <- PJM.Plant.Data[which(PJM.Plant.Data$Marginal.Price >= 0),]
PJM.Plant.Data <- PJM.Plant.Data[which(!is.na(PJM.Plant.Data$Marginal.Price)),] 


## Remove dupes ##
PJM.Plant.Data <- PJM.Plant.Data %>% distinct(Power.Plant, .keep_all = TRUE)




## Order Data ##

PJM.Plant.Data <- PJM.Plant.Data[order(PJM.Plant.Data$Marginal.Price, decreasing = F),]
PJM.Plant.Data$Operating.Capacity <- as.numeric(as.character(PJM.Plant.Data$Operating.Capacity))






## Fix capacity factors ##
PJM.Plant.Data$Capacity.Factor.... <- as.numeric(as.character(PJM.Plant.Data$Capacity.Factor....))
PJM.Plant.Data$Capacity.Factor.... <- PJM.Plant.Data$Capacity.Factor..../100








### Clean Load Data ###

load.2015$DAY <- as.Date(load.2015$DAY, format= "%m/%d/%Y")
load.2015 <- load.2015[order(load.2015$DAY, load.2015$HOUR),]
load.2015$HOY <- c(1:8760)
load.2015$NTotal <- load.2015$Total-2000
load.2015$NNTotal <- load.2015$Total-4000

load.2014$DAY <- as.Date(load.2014$DAY, format= "%m/%d/%Y")
load.2014 <- load.2014[order(load.2014$DAY, load.2014$HOUR),]
load.2014$HOY <- c(1:8760)
load.2014$NTotal <- load.2014$Total-2000
load.2014$NNTotal <- load.2014$Total-4000


## Build empty supply curve list

Supply.Curve <- vector("list", 8760)





## Generate Cumulative Capacity

PJM.Plant.Data$Capacity <- 0
# PJM.Plant.Data$CRF.Hour <- 0
# PJM.Plant.Data$CRF.Hour.5  <- 0
# cumsum(PJM.Plant.Data$Operating.Capacity)

## Derate capacity

# PJM.Plant.Data$Capacity <- PJM.Plant.Data$Capacity*.9


## Loop to build supply curves with wind/solar data incorporated

for(i in 1:8760){
  Supply.Curve[[i]] <- PJM.Plant.Data
  Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")] <- VRE.Factors$Wind[which(VRE.Factors$Hour == i)]
  Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")] <- VRE.Factors$Solar[which(VRE.Factors$Hour == i)]
  Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")] <- Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")]*Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")]
  Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")] <- Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")]*Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")]
  Supply.Curve[[i]]$Capacity <- cumsum(Supply.Curve[[i]]$Operating.Capacity)
  Supply.Curve[[i]]$Capacity <- Supply.Curve[[i]]$Capacity*.9
}

## Empty table for energy payment revenue ##
comp.revenue <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))
as.revenue <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))






#### SET UP PRICE-LOAD MODELS FOR REVENUE INPUTS #####
## Load and Clean Data


LMP <- melt(LMP, id=c("PUBLISHDATE", "VERSION", "ZONE", "PNODEID", "PNODENAME", "PNODETYPE", "PRICINGTYPE"))
colnames(LMP)[8] <- "Hour"
colnames(LMP)[9] <- "Price"

LMP <- LMP[which(LMP$PRICINGTYPE == "TotalLMP"),]
LMP$PUBLISHDATE <- as.Date(LMP$PUBLISHDATE, format = "%m/%d/%Y")
LMP <- LMP[order(LMP$PUBLISHDATE, LMP$Hour),]
LMP$Hour <- sub('*.', '', LMP$Hour)
LMP <- LMP[which(LMP$Hour != "2DST"),]



LMP.2014 <- melt(LMP.2014, id=c("PUBLISHDATE", "VERSION", "ZONE", "PNODEID", "PNODENAME", "PNODETYPE", "PRICINGTYPE"))
colnames(LMP.2014)[8] <- "Hour"
colnames(LMP.2014)[9] <- "Price"

LMP.2014 <- LMP.2014[which(LMP.2014$PRICINGTYPE == "TotalLMP"),]
LMP.2014$PUBLISHDATE <- as.Date(LMP.2014$PUBLISHDATE, format = "%m/%d/%Y")
LMP.2014 <- LMP.2014[order(LMP.2014$PUBLISHDATE, LMP.2014$Hour),]
LMP.2014$Hour <- sub('*.', '', LMP.2014$Hour)
LMP.2014 <- LMP.2014[which(LMP.2014$Hour != "2DST"),]


##### Build Quarterly Price-Load data sets #####

## Full Year ##

price.load <- data.frame(LMP$Price, load.2015$Total)

colnames(price.load) <- c("LMP.2015.Price","Total")

cube.fit <- lm(LMP.2015.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load)


### Break into quarters ###
## NORMAL
price.load.q1 <- data.frame(LMP$Price[1:2160],
                            load.2015$Total[1:2160])

price.load.q2 <- data.frame(LMP$Price[2160:4344],
                            load.2015$Total[2160:4344])

price.load.q3 <- data.frame(LMP$Price[4344:6552],
                            load.2015$Total[4344:6552])

price.load.q4 <- data.frame(LMP$Price[6552:8760],
                            load.2015$Total[6552:8760])

colnames(price.load.q1) <- c("LMP.2015.Price","Total")
colnames(price.load.q2) <- c("LMP.2015.Price","Total")
colnames(price.load.q3) <- c("LMP.2015.Price","Total")
colnames(price.load.q4) <- c("LMP.2015.Price","Total")


##2014##

price.load.q1.14 <- data.frame(LMP.2014$Price[1:2160],
                            load.2014$Total[1:2160])

price.load.q2.14 <- data.frame(LMP.2014$Price[2160:4344],
                            load.2014$Total[2160:4344])

price.load.q3.14 <- data.frame(LMP.2014$Price[4344:6552],
                            load.2014$Total[4344:6552])

price.load.q4.14 <- data.frame(LMP.2014$Price[6552:8760],
                            load.2014$Total[6552:8760])

colnames(price.load.q1.14) <- c("LMP.2014.Price","Total")
colnames(price.load.q2.14) <- c("LMP.2014.Price","Total")
colnames(price.load.q3.14) <- c("LMP.2014.Price","Total")
colnames(price.load.q4.14) <- c("LMP.2014.Price","Total")



## 2k retirements
price.load.q1.R <- data.frame(LMP$Price[1:2160],
                              load.2015$NTotal[1:2160])

price.load.q2.R <- data.frame(LMP$Price[2160:4344],
                              load.2015$NTotal[2160:4344])

price.load.q3.R <- data.frame(LMP$Price[4344:6552],
                              load.2015$NTotal[4344:6552])

price.load.q4.R <- data.frame(LMP$Price[6552:8760],
                              load.2015$NTotal[6552:8760])



colnames(price.load.q1.R) <- c("LMP.2015.Price","Total")
colnames(price.load.q2.R) <- c("LMP.2015.Price","Total")
colnames(price.load.q3.R) <- c("LMP.2015.Price","Total")
colnames(price.load.q4.R) <- c("LMP.2015.Price","Total")






## 4k retirements
price.load.q1.2R <- data.frame(LMP$Price[1:2160],
                              load.2015$NNTotal[1:2160])

price.load.q2.2R <- data.frame(LMP$Price[2160:4344],
                              load.2015$NNTotal[2160:4344])

price.load.q3.2R <- data.frame(LMP$Price[4344:6552],
                              load.2015$NNTotal[4344:6552])

price.load.q4.2R <- data.frame(LMP$Price[6552:8760],
                              load.2015$NNTotal[6552:8760])



colnames(price.load.q1.2R) <- c("LMP.2015.Price","Total")
colnames(price.load.q2.2R) <- c("LMP.2015.Price","Total")
colnames(price.load.q3.2R) <- c("LMP.2015.Price","Total")
colnames(price.load.q4.2R) <- c("LMP.2015.Price","Total")



####### Build regressions ########

## Quarterly cubic fits ##
# 2015 #

cube.fit.q1 <- lm(LMP.2015.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q1)


cube.fit.q2 <- lm(LMP.2015.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q2)


cube.fit.q3 <- lm(LMP.2015.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q3)


cube.fit.q4 <- lm(LMP.2015.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q4)

# 2014 #

cube.fit.q1.14 <- lm(LMP.2014.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q1.14)


cube.fit.q2.14 <- lm(LMP.2014.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q2.14)


cube.fit.q3.14 <- lm(LMP.2014.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q3.14)


cube.fit.q4.14 <- lm(LMP.2014.Price~Total+I(Total^2)
                  +I(Total^3), data=price.load.q4.14)


## 2k Retirements ##
cube.fit.q1.R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q1.R)


cube.fit.q2.R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q2.R)


cube.fit.q3.R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q3.R)


cube.fit.q4.R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q4.R)


cube.fit.q1.2R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q1.2R)


cube.fit.q2.2R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q2.2R)


cube.fit.q3.2R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q3.2R)


cube.fit.q4.2R <- lm(LMP.2015.Price~Total+I(Total^2)
                    +I(Total^3), data=price.load.q4.2R)



#### BUILD BI-WEEKLY REGRESSIONS ####


# Divide into weeks
Weeks.List <- vector("list", 26)

##2015
i<-1
while(i < 8736){
  for(j in 1:26){
    Weeks.List[[j]] <- data.frame(LMP$Price[i:(i+336)],
                                  load.2015$Total[i:(i+336)])
    colnames(Weeks.List[[j]]) <- c("Price", "Total")
    i <- i+336
  }
  i<-i
}

##2014

Weeks.List.14 <- vector("list", 26)

i<-1
while(i < 8736){
  for(j in 1:26){
    Weeks.List.14[[j]] <- data.frame(LMP.2014$Price[i:(i+336)],
                                  load.2014$Total[i:(i+336)])
    colnames(Weeks.List.14[[j]]) <- c("Price", "Total")
    i <- i+336
  }
  i<-i
}


## Build Regressions for bi-weekly ##

##2015
fits.list <- vector("list", 26)

for(j in 1:26){
  fits.list[[j]] <- lm(Price~Total+I(Total^2)
                       +I(Total^3), data=data.frame(Weeks.List[[j]]))
}

##2014

fits.list.14 <- vector("list", 26)

for(j in 1:26){
  fits.list.14[[j]] <- lm(Price~Total+I(Total^2)
                       +I(Total^3), data=data.frame(Weeks.List.14[[j]]))
}







############ Loop to set hourly revenue and marginal plants ##############


### ADDING STORAGE NOTES ###
# If price <= mean predict(price) of hour i:i-24 then charge, if price > mean predict(price) of hour i:i-24 (WRONG!!)
# Discharge 5 hours, charge for 6 hours
# 1 GW
# Everyday starts half full and ends half full
# Discharge = reduce demand by 1 GW, charge, increase demand by 1 GW
# This will involve shifting supply curves for hours of discharge
# for(i in Date){
# supplycurve[[i]] <- pjmplant[]
#}


### Yearly version ###

# for(i in 1:8760){
#   load <- load.2015[i,"Total"]
#   Cur.Curve <- data.frame(Supply.Curve[[i]])
#   marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
#   # marg.price <- max(marg.plants$Marginal.Price)
#   marg.price <- as.numeric(predict(cube.fit, newdata=data.frame(Total=c(load))))
#   comp.revenue[,i] <- ifelse(Cur.Curve$Capacity <= load, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)
#   
#   # ## Spinning/Nonspinning reserve payments
#   #   if(i==1){
#   #     as.revenue[,i] <- 0
#   #   } else  {
#   #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] == 0 & comp.revenue[,i] > 0, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)
#   #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] > 0 & comp.revenue[,i] == 0, marg.price*Supply.Curve[[i]]$Operating.Capacity, as.revenue[,i])
#   #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] == 0 & comp.revenue[,i] == 0, 0, as.revenue[,i])
#   #     as.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar"),] <- 0
#   #     as.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind"),] <- 0
#   #   }
#   # }
# }







### Quarterly Version ###


# for(i in 1:8760){
#   load <- load.2015[i,"Total"]
#   Cur.Curve <- data.frame(Supply.Curve[[i]])
#   marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
#   # marg.price <- max(marg.plants$Marginal.Price)
#   if(i<=2160){
#     marg.price <- as.numeric(predict(cube.fit.q4.14, newdata=data.frame(Total=c(load))))
#   }  else if(i<=4344 & i>2160){
#     marg.price <- as.numeric(predict(cube.fit.q1, newdata=data.frame(Total=c(load))))
#   }  else if(i<=6552 & i>4344){
#     marg.price <- as.numeric(predict(cube.fit.q2, newdata=data.frame(Total=c(load))))
#   }  else
#     marg.price <- as.numeric(predict(cube.fit.q3, newdata=data.frame(Total=c(load))))
# 
#   comp.revenue[,i] <- ifelse(Cur.Curve$Capacity <= load, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)
# 
# # ## Spinning/Nonspinning reserve payments
# #   if(i==1){
# #     as.revenue[,i] <- 0
# #   } else  {
# #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] == 0 & comp.revenue[,i] > 0, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)
# #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] > 0 & comp.revenue[,i] == 0, marg.price*Supply.Curve[[i]]$Operating.Capacity, as.revenue[,i])
# #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] == 0 & comp.revenue[,i] == 0, 0, as.revenue[,i])
# #     as.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar"),] <- 0
# #     as.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind"),] <- 0
# #   }
# # }
# }






### Bi-weekly version ###

for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  # marg.price <- max(marg.plants$Marginal.Price)
  if(i<=336){
    marg.price <- as.numeric(predict(fits.list.14[[26]], newdata=data.frame(Total=c(load))))
  }  else if(i<=672 & i>336){
    marg.price <- as.numeric(predict(fits.list[[1]], newdata=data.frame(Total=c(load))))
  }  else if(i<=1008 & i>672){
    marg.price <- as.numeric(predict(fits.list[[2]], newdata=data.frame(Total=c(load))))
  }  else if(i<=1344 & i>1008){
    marg.price <- as.numeric(predict(fits.list[[3]], newdata=data.frame(Total=c(load))))
  }  else if(i<=1680 & i>1344){
    marg.price <- as.numeric(predict(fits.list[[4]], newdata=data.frame(Total=c(load))))
  }  else if(i<=2016 & i>1680){
    marg.price <- as.numeric(predict(fits.list[[5]], newdata=data.frame(Total=c(load))))
  }  else if(i<=2352 & i>2016){
    marg.price <- as.numeric(predict(fits.list[[6]], newdata=data.frame(Total=c(load))))
  }  else if(i<=2688 & i>2352){
    marg.price <- as.numeric(predict(fits.list[[7]], newdata=data.frame(Total=c(load))))
  }  else if(i<=3024 & i>2688){
    marg.price <- as.numeric(predict(fits.list[[8]], newdata=data.frame(Total=c(load))))
  }  else if(i<=3360 & i>3024){
    marg.price <- as.numeric(predict(fits.list[[9]], newdata=data.frame(Total=c(load))))
  }  else if(i<=3696 & i>3360){
    marg.price <- as.numeric(predict(fits.list[[10]], newdata=data.frame(Total=c(load))))
  }  else if(i<=4032 & i>3696){
    marg.price <- as.numeric(predict(fits.list[[11]], newdata=data.frame(Total=c(load))))
  }  else if(i<=4368 & i>4032){
    marg.price <- as.numeric(predict(fits.list[[12]], newdata=data.frame(Total=c(load))))
  }  else if(i<=4704 & i>4368){
    marg.price <- as.numeric(predict(fits.list[[13]], newdata=data.frame(Total=c(load))))
  }  else if(i<=5040 & i>4704){
    marg.price <- as.numeric(predict(fits.list[[14]], newdata=data.frame(Total=c(load))))
  }  else if(i<=5376 & i>5040){
    marg.price <- as.numeric(predict(fits.list[[15]], newdata=data.frame(Total=c(load))))
  }  else if(i<=5712 & i>5376){
    marg.price <- as.numeric(predict(fits.list[[16]], newdata=data.frame(Total=c(load))))
  }  else if(i<=6048 & i>5712){
    marg.price <- as.numeric(predict(fits.list[[17]], newdata=data.frame(Total=c(load))))
  }  else if(i<=6384 & i>6048){
    marg.price <- as.numeric(predict(fits.list[[18]], newdata=data.frame(Total=c(load))))
  }  else if(i<=6720 & i>6384){
    marg.price <- as.numeric(predict(fits.list[[19]], newdata=data.frame(Total=c(load))))
  }  else if(i<=7056 & i>6720){
    marg.price <- as.numeric(predict(fits.list[[20]], newdata=data.frame(Total=c(load))))
  }  else if(i<=7392 & i>7056){
    marg.price <- as.numeric(predict(fits.list[[21]], newdata=data.frame(Total=c(load))))
  }  else if(i<=7728 & i>7392){
    marg.price <- as.numeric(predict(fits.list[[22]], newdata=data.frame(Total=c(load))))
  }  else if(i<=8064 & i>7728){
    marg.price <- as.numeric(predict(fits.list[[23]], newdata=data.frame(Total=c(load))))
  }  else if(i<=8400 & i>8064){
    marg.price <- as.numeric(predict(fits.list[[24]], newdata=data.frame(Total=c(load))))
  }  else if(i<=8736 & i>8400){
    marg.price <- as.numeric(predict(fits.list[[25]], newdata=data.frame(Total=c(load))))
  }  else
    marg.price <- as.numeric(predict(fits.list[[26]], newdata=data.frame(Total=c(load))))

  comp.revenue[,i] <- ifelse(Cur.Curve$Capacity <= load, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)

  # ## Spinning/Nonspinning reserve payments
  #   if(i==1){
  #     as.revenue[,i] <- 0
  #   } else  {
  #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] == 0 & comp.revenue[,i] > 0, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)
  #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] > 0 & comp.revenue[,i] == 0, marg.price*Supply.Curve[[i]]$Operating.Capacity, as.revenue[,i])
  #     as.revenue[,i] <- ifelse(comp.revenue[,i-1] == 0 & comp.revenue[,i] == 0, 0, as.revenue[,i])
  #     as.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar"),] <- 0
  #     as.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind"),] <- 0
  #   }
  # }
}

# Establish max/min values function

maxN <- function(x, N=5){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
  }
  sort(x, partial=len-N+1)[len-N+1:N]
}

minN <- function(x, N=6){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
  }
  sort(x, partial=len-N+1)[1:N]
}

# ### FIX THIS ####
# Supply.Curve[[i]]$Day <- 1
# 
# 
# for(i in 1:8784){
#   i<-1
#   for(j in 1:365){
#       Supply.Curve[[i:i+24]]["Day"] <- j
#       i <- i+24
#   }
#   i<-i
# }
# 
# 
# PJM.Plant.Data$Power.Plant <- as.character(PJM.Plant.Data$Power.Plant)
# PJM.Plant.Data$Technology.Type <- as.character(PJM.Plant.Data$Technology.Type)
# PJM.Plant.Data <- rbind(PJM.Plant.Data, c("Storage", 0, "Storage", 1000, NA, NA, NA, NA, NA, NA, NA, NA, "Storage", NA))
# PJM.Plant.Data$Technology.Type <- as.factor(PJM.Plant.Data$Technology.Type)
# 
# ###AFTER DAY ESTABLISHED, ADD STORAGE TO EACH CURVE###
# for(i in 1:8760){
#   Supply.Curve[[i]] <- PJM.Plant.Data
#   Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")] <- VRE.Factors$Wind[which(VRE.Factors$Hour == i)]
#   Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")] <- VRE.Factors$Solar[which(VRE.Factors$Hour == i)]
#   Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")] <- Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")]*Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")]
#   Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")] <- Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")]*Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")]
#   Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Technology.Type == "Storage")] <- Supply.Curve[[i]]$Operating.Capacity
#   Supply.Curve[[i]]$Capacity <- cumsum(Supply.Curve[[i]]$Operating.Capacity)
#   Supply.Curve[[i]]$Capacity <- Supply.Curve[[i]]$Capacity*.9
# }
# 
# ### FIX THIS ###







### BUILD SYSTEM REVENUE DATA FRAME ###
SR <- data.frame(matrix(0, ncol=2, nrow=2))
colnames(SR) <- c("Revenue.Source", "Revenue")

SR[1,1] <- "Energy Payments (VOM)"
SR[2,1] <- "Capacity Payments"



## Plant Revenue (EP ONLY)
PR <- data.frame(PJM.Plant.Data$Power.Plant,
                 apply(comp.revenue, 1, sum))
# PR[which(PJM.Plant.Data$Power.Plant == "Quad Cities"),] <- PR[which(PJM.Plant.Data$Power.Plant == "Quad Cities")]+(9.7*M.Operating[which(PJM.Plant.Data$Power.Plant == "Quad Cities"),])


## Sum to system revenue ##
SR[1,2] <- sum(PR$apply.comp.revenue..1..sum.)




### ADD CAPACITY PAYMENTS ###
PR$Capacity.Payment <- Capacity.Price*365*PJM.Plant.Data$Operating.Capacity
PR$Capacity.Payment <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Wind", Capacity.Price*365*PJM.Plant.Data$Operating.Capacity*.12, PR$Capacity.Payment)
PR$Capacity.Payment <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Solar", Capacity.Price*365*PJM.Plant.Data$Operating.Capacity*.38, PR$Capacity.Payment)
SR[2,2] <- sum(PR$Capacity.Payment)



## Technology Revenue (EP ONLY) ##
TR <- data.frame(unique(PJM.Plant.Data$Primary.Fuel.Type))

for(i in unique(PJM.Plant.Data$Primary.Fuel.Type)){
  TR[which(TR$unique.PJM.Plant.Data.Primary.Fuel.Type.==i), "V2"] <- sum(comp.revenue[which(PJM.Plant.Data$Primary.Fuel.Type == i),])
  TR[which(TR$unique.PJM.Plant.Data.Primary.Fuel.Type.==i), "V3"] <- sum(PR$Capacity.Payment[which(PJM.Plant.Data$Primary.Fuel.Type == i)])
}



##### MWH IN SYSTEM (Competitive Market) #####

M.Operating <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))

for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  M.Operating[,i] <- ifelse(Cur.Curve$Capacity<=load,Cur.Curve$Operating.Capacity, 0)
}



















#########  REVENUE ESTIMATION (COST PLUS) ##########


CP.Operating <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))
CP.MP.Revenue <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))




PJM.Plant.Data <- PJM.Plant.Data[!is.na(PJM.Plant.Data$Power.Plant),]

### Calculate Operating Hours and MP only Revenue ###

for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  CP.Operating[,i] <- ifelse(Cur.Curve$Capacity<=load, 1, 0)
  CP.MP.Revenue[,i] <- ifelse(Cur.Curve$Capacity<=load, Cur.Curve$Marginal.Price*Cur.Curve$Operating.Capacity, 0)
}






### Calculate MWH of each plant ###
for(j in 1:nrow(CP.Operating)){
  PJM.Plant.Data$CalcHrs[j] <- sum(CP.Operating[j,])
  PJM.Plant.Data$CalcMWH[j] <- PJM.Plant.Data$Operating.Capacity[j]*PJM.Plant.Data$CalcHrs[j]
}




#### BUILD DATA FRAME FOR REVENUE ####

SR.CP <- data.frame(matrix(0, nrow=4, ncol=2))


## ADD ENERGY PRICE ONLY ##
SR.CP[1,1] <- "Energy Only Revenue"
SR.CP[1,2] <- sum(CP.MP.Revenue)
SR.CP[2,1] <- "Full Cost CRF"
SR.CP[3,1] <- "Half Cost CRF"
SR.CP[4,1] <- "Fully Dep CRF"




####### COE/CRF at 100% #######


## Coal
PJM.Plant.Data$CRF.Hour  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Coal",
                                ((Coal.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                PJM.Plant.Data$Fixed.O.M.Cost....)/
                                PJM.Plant.Data$CalcMWH,
                                0)


## CCs
PJM.Plant.Data$CRF.Hour  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Gas",
                                   ((NG.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                      PJM.Plant.Data$Fixed.O.M.Cost....)/
                                     PJM.Plant.Data$CalcMWH,
                                   PJM.Plant.Data$CRF.Hour)


## Nuclear

PJM.Plant.Data$CRF.Hour  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Nuclear",
                                   ((Nuc.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                      PJM.Plant.Data$Fixed.O.M.Cost....)/
                                     PJM.Plant.Data$CalcMWH,
                                   PJM.Plant.Data$CRF.Hour)


## Hydro
PJM.Plant.Data$CRF.Hour  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Water",
                                   ((Hyd.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                      PJM.Plant.Data$Fixed.O.M.Cost....)/
                                     PJM.Plant.Data$CalcMWH,
                                   PJM.Plant.Data$CRF.Hour)

## Biomass
PJM.Plant.Data$CRF.Hour  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Biomass",
                                   ((Bio.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                      PJM.Plant.Data$Fixed.O.M.Cost....)/
                                     PJM.Plant.Data$CalcMWH,
                                   PJM.Plant.Data$CRF.Hour)


## Wind
PJM.Plant.Data$CRF.Hour  <-        ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Wind",
                                   ((Wind.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                   PJM.Plant.Data$Fixed.O.M.Cost....)/
                                   PJM.Plant.Data$CalcMWH,
                                   PJM.Plant.Data$CRF.Hour)

## Solar
PJM.Plant.Data$CRF.Hour  <-         ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Solar",
                                   ((Solar.Cap*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                      PJM.Plant.Data$Fixed.O.M.Cost....)/
                                     PJM.Plant.Data$CalcMWH,
                                   PJM.Plant.Data$CRF.Hour)




#######  COE/CRF  50% #######


  ## Coal
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Coal",
                                          ((Coal.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          0)
  
  
  ## CCs
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Gas",
                                          ((NG.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.5)
  
  
  ## Nuclear
  
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Nuclear",
                                          ((Nuc.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.5)
  
  
  ## Hydro
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Water",
                                          ((Hyd.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.5)
  
  ## Biomass
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Biomass",
                                          ((Bio.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.5)

  ## Solar
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Solar",
                                       ((Solar.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                          PJM.Plant.Data$Fixed.O.M.Cost....)/
                                         PJM.Plant.Data$CalcMWH,
                                       PJM.Plant.Data$CRF.Hour.5)

  ## Wind
PJM.Plant.Data$CRF.Hour.5  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Wind",
                                       ((Wind.Cap*.5*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                          PJM.Plant.Data$Fixed.O.M.Cost....)/
                                         PJM.Plant.Data$CalcMWH,
                                       PJM.Plant.Data$CRF.Hour.5)


### Fully depreciated ###


  ## Coal
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Coal",
                                          ((Coal.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          0)
  
  
  ## CCs
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Gas",
                                          ((NG.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.0)
  
  
  ## Nuclear
  
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Nuclear",
                                          ((Nuc.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.0)
  
  
  ## Hydro
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Water",
                                          ((Hyd.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.0)
  
  ## Biomass
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Biomass",
                                          ((Bio.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                             PJM.Plant.Data$Fixed.O.M.Cost....)/
                                            PJM.Plant.Data$CalcMWH,
                                          PJM.Plant.Data$CRF.Hour.0)

  ## Solar
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Solar",
                                       ((Solar.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                          PJM.Plant.Data$Fixed.O.M.Cost....)/
                                         PJM.Plant.Data$CalcMWH,
                                       PJM.Plant.Data$CRF.Hour.0)

  ## Wind
PJM.Plant.Data$CRF.Hour.0  <- ifelse(PJM.Plant.Data$Primary.Fuel.Type == "Wind",
                                       ((Wind.Cap*0*1000*PJM.Plant.Data$Operating.Capacity*CRF)+
                                          PJM.Plant.Data$Fixed.O.M.Cost....)/
                                         PJM.Plant.Data$CalcMWH,
                                       PJM.Plant.Data$CRF.Hour.0)

CP.CRF.Revenue <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))  
CP.CRF.Revenue.5 <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))
CP.CRF.Revenue.0 <- data.frame(matrix(0, ncol=8760, nrow=length(PJM.Plant.Data$Power.Plant)))





## Which generators get paid?







## REBUILD WITH CRF DATA
for(i in 1:8760){
  Supply.Curve[[i]] <- PJM.Plant.Data
  Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")] <- VRE.Factors$Wind[which(VRE.Factors$Hour == i)]
  Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")] <- VRE.Factors$Solar[which(VRE.Factors$Hour == i)]
  Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")] <- Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")]*Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Wind")]
  Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")] <- Supply.Curve[[i]]$Operating.Capacity[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")]*Supply.Curve[[i]]$Capacity.Factor....[which(PJM.Plant.Data$Primary.Fuel.Type == "Solar")]
  Supply.Curve[[i]]$Capacity <- cumsum(Supply.Curve[[i]]$Operating.Capacity)
  Supply.Curve[[i]]$Capacity <- Supply.Curve[[i]]$Capacity*.9
}





for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  # marg.price <- max(marg.plants$Marginal.Price)
  if(i<=2160){
    marg.price <- as.numeric(predict(cube.fit.q1, newdata=data.frame(Total=c(load))))
  }  else if(i<=4344 & i>2160){
    marg.price <- as.numeric(predict(cube.fit.q2, newdata=data.frame(Total=c(load))))
  }  else if(i<=6552 & i>4344){
    marg.price <- as.numeric(predict(cube.fit.q3, newdata=data.frame(Total=c(load))))
  }  else
    marg.price <- as.numeric(predict(cube.fit.q4, newdata=data.frame(Total=c(load))))
  
  comp.revenue[,i] <- ifelse(Cur.Curve$Capacity <= load, marg.price*Supply.Curve[[i]]$Operating.Capacity, 0)
}




### Which Generators Get Paid? ###

### 100% ###
for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  CP.Operating[,i] <- ifelse(Cur.Curve$Capacity<=load, 1, 0)
  CP.CRF.Revenue[,i] <- ifelse(Cur.Curve$Capacity<=load, Cur.Curve$CRF.Hour*Cur.Curve$Operating.Capacity, as.numeric(0))
}


### 50% ###
for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  CP.Operating[,i] <- ifelse(Cur.Curve$Capacity<=load, 1, 0)
  CP.CRF.Revenue.5[,i] <- ifelse(Cur.Curve$Capacity<=load, Cur.Curve$CRF.Hour.5*Cur.Curve$Operating.Capacity, as.numeric(0))
}

### FUlly ###

for(i in 1:8760){
  load <- load.2015[i,"Total"]
  Cur.Curve <- data.frame(Supply.Curve[[i]])
  marg.plants <- Cur.Curve[which(Cur.Curve$Capacity <= load.2015[i,"Total"]),]
  CP.Operating[,i] <- ifelse(Cur.Curve$Capacity<=load, 1, 0)
  CP.CRF.Revenue.0[,i] <- ifelse(Cur.Curve$Capacity<=load, Cur.Curve$CRF.Hour.0*Cur.Curve$Operating.Capacity, as.numeric(0))
}








### Sum for system revenue ###
SR.CP[2,2] <- sum(CP.CRF.Revenue)









##  Sum for system revenue
SR.CP[3,2] <- sum(CP.CRF.Revenue.5)

SR.CP[4,2] <- sum(CP.CRF.Revenue.0)







##### PLANT BASED REVENUES #####
PR.CP <- data.frame(matrix(0, nrow=nrow(PJM.Plant.Data), ncol=4))
colnames(PR.CP) <- c("Plant Name", "Energy Payments", "100% CR Payments", "50% CR Payments")

PR.CP[,"Plant Name"] <- unique(PJM.Plant.Data$Power.Plant)
PR.CP[,"Energy Payments"] <- apply(CP.MP.Revenue, 1, sum)
PR.CP[,"100% CR Payments"] <- apply(CP.CRF.Revenue, 1, sum)
PR.CP[,"50% CR Payments"] <- apply(CP.CRF.Revenue.5, 1, sum)







#### TECHNOLOGY BASED REVENUES ####

TR.CP <- data.frame(unique(PJM.Plant.Data$Primary.Fuel.Type))


for(i in unique(PJM.Plant.Data$Primary.Fuel.Type)){
  TR.CP[which(TR.CP$unique.PJM.Plant.Data.Primary.Fuel.Type.==i), "V2"] <- sum(CP.MP.Revenue[which(PJM.Plant.Data$Primary.Fuel.Type == i),])
  TR.CP[which(TR.CP$unique.PJM.Plant.Data.Primary.Fuel.Type.==i), "V3"] <- sum(CP.CRF.Revenue[which(PJM.Plant.Data$Primary.Fuel.Type == i),])
  TR.CP[which(TR.CP$unique.PJM.Plant.Data.Primary.Fuel.Type.==i), "V4"] <- sum(CP.CRF.Revenue.5[which(PJM.Plant.Data$Primary.Fuel.Type == i),])
}





