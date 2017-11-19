#predicting sales data ranking and return values
source('RealEstateRanking/supportFunctions.R')
#estimate assessed return rate of rental property
aRData <- read.csv("dataset/salesPropForAnalysis.csv")
sales <- read.csv("dataset/streetSalesData.csv")
salesGeo <- sales[,c("Longitude","Latitude")]
#result$assessedReturn <- aRData$assessedReturn
assessedValue <- apply(salesGeo,1,FUN=function(x)findAssessedReturn(estateGeo=x,poi=aRData,threshold = 300))
sales$assessedValue <- assessedValue
#find Borough index for rental data
#load in training data, compare the coordinates of two datasets,
#among properties in the training set, the Borough index of the closest one to a property
#in sales data is assigned as the borough index of the rental property.
bor <- c()
for (i in 1:dim(salesGeo)[1]){
    d <- distMeeus(salesGeo[i,],netdata[,c("Longitude","Latitude")])
    b <- netdata$BOROUGH[which(d == min(d))]
    bor <- c(bor,b[1])
}
sales$BOROUGH <- as.integer(bor)
write.csv(sales,file='dataset/streetSalesData.csv',row.names = F)
sales <- read.csv('dataset/streetSalesData.csv')
#write.csv(aRData,"dataset/salesPropForAnalysis.csv",row.names = F)

#find value
#prepare testing data
sales_selection <- c("BOROUGH","Longitude","Latitude",
                    "BusCounts","BusDist","SubCounts","SubDist","ComplaintCounts",
                    "CrimeCounts","SerPhi","SchPhi","ArtsPhi","RestPhi","RecPhi",
                    "UnivPhi","SerCounts","SchCounts","ArtsCounts","RestCounts",
                    "RecCounts","UnivCounts","assessedValue","Families","Condos","Coops","Rentals")
salesdata <- sales[,sales_selection]
salesdata <- salesdata %>%
    mutate(logBusDist = log(BusDist + 1), logSubCounts = log(SubCounts + 1),
           logSubDist = log(SubDist + 1), logSerCounts = log(SerCounts + 1),
           logNoiseCounts = log(ComplaintCounts + 1),logCrimeCounts = log(CrimeCounts + 1),
           logSchCounts = log(SchCounts + 1),logArtsCounts = log(ArtsCounts+1),
           logRecCounts = log(RecCounts+1), logRestCounts = log(RestCounts+1), logUnivCounts = log(UnivCounts+1))

#normalize features
norm.fun = function(x){ 
    (x - min(x))/(max(x) - min(x)) 
}
salesdata <- salesdata %>%
    mutate(BusCounts = norm.fun(BusCounts),logBusDist = norm.fun(logBusDist),
           logSubCounts = norm.fun(logSubCounts),logSubDist = norm.fun(logSubDist),
           logSerCounts = norm.fun(SerCounts), logSchCounts = norm.fun(SchCounts),
           logNoiseCounts = norm.fun(logNoiseCounts),logCrimeCounts = norm.fun(logCrimeCounts),
           logArtsCounts = norm.fun(ArtsCounts),logRecCounts = norm.fun(RecCounts),
           logRestCounts = norm.fun(RestCounts),logRestCounts = norm.fun(logRestCounts))

#repreentation of property value gamma
sales_pd_selection <- c("BusCounts","logBusDist", "logSubCounts","logSubDist",
                       "logSerCounts","logSchCounts","logArtsCounts",
                       "logRecCounts","logRestCounts","logNoiseCounts",
                       "logCrimeCounts","logUnivCounts","Families","Condos","Coops","Rentals")
sales_pd <- salesdata[,sales_pd_selection] # 30 UnivCounts
sales_pd <- cbind(rep(1,dim(sales_pd)[1]),sales_pd)
sales_pd <- as.matrix(sales_pd)
#calculate gamma for sales data
sales_gamma =  t(final_q %*% final_weights %*% t(sales_pd)) + sales$assessedValue
sales_rho <- apply(sales[,c("Longitude","Latitude")],1,FUN=function(x) Cal_rho(estateGeo = x,centers = final_r_mu[,1:2],etas = final_r_eta[,1], d0 = 3000))
sales$gamma <- as.vector(sales_gamma)
sales$rho <- sales_rho
final_phi_data <- phi_data[,c("SerPhi","SchPhi","ArtsPhi","restPhi","recPhi","UnivPhi")]
final_phi_data <- cbind(final_phi_data, final_rt)
names(final_phi_data) <- c("SerPhi","SchPhi","ArtsPhi","restPhi","recPhi","UnivPhi","r")



#predict delta for testing data
Predict_delta <- function(data,r_mark,final_phi_data,final_r_eta){
    sales_phi_data <- data[,c("SerPhi","SchPhi","ArtsPhi","RestPhi","RecPhi","UnivPhi")]
    eta_sum <- sum(final_r_eta[r_mark,1])
    sales_delta <- c()
    for (r_i in r_mark){
        sales_phi_data$r <- r_i
        delta <- apply(sales_phi_data,1, FUN=function(x)Cal_delta(estate=x,dataset = final_phi_data))
        sales_delta <- cbind(sales_delta,delta*final_r_eta[r_i,1]/eta_sum)
    }
    result <- apply(sales_delta,1,sum)
    return(result)
}


# test_phi_data <- sales_BQ[7:15,c("SerPhi","SchPhi","ArtsPhi","RestPhi","RecPhi","UnivPhi")]
# test_sales_delta <- c()
# for (r_i in 6:15){
#     test_phi_data$r <- r_i
#     test_delta <- apply(test_phi_data,1, FUN=function(x)Cal_delta(estate=x,dataset = final_phi_data))
#     test_sales_delta <- cbind(test_sales_delta, test_delta)
#     }



sales_man <- sales %>% filter(BOROUGH == 1)
sales_BQ <- sales %>% filter(BOROUGH == 3)
sales_bronx <- sales %>% filter(BOROUGH == 2)
sales_staten <- sales %>% filter(BOROUGH == 5)


sales_man_delta <- Predict_delta(sales_man,1:5,final_phi_data,final_r_eta)
sales_BQ_delta <- Predict_delta(sales_BQ,6:15,final_phi_data,final_r_eta)
sales_bronx_delta <- Predict_delta(sales_bronx,16:20,final_phi_data,final_r_eta)
sales_staten_delta <- Predict_delta(sales_staten,21:25,final_phi_data,final_r_eta)
sales_man$delta <- sales_man_delta
sales_BQ$delta <- sales_BQ_delta
sales_bronx$delta <- sales_bronx_delta
sales_staten$delta <- sales_staten_delta
test <- rbind(sales_man,sales_BQ,sales_bronx,sales_staten)
sales <- test

sales$annual_return <- sales$gamma+sales$rho+sales$delta
map <- get_map(location='New York City', zoom = 10,color = "bw",maptype = 'roadmap')
prediction_visualize(sales)
test <- sales %>% arrange(desc(annual_return))

#rank the sales annual return rates into four hierarchical levels
#(-inf, mean - mean abs dv),(mean abs dv, mean),(mean,mean + mean abs dv),(mean + mean abs dv, inf)
ranking_sales <- function(result){
    result_mean = mean(result$annual_return)
    result_std = mean(sapply(result$annual_return,FUN = function(x){abs(x-result_mean)}))
    result_rank <- c()
    for(i in 1:dim(result)[1]){
        if(result$annual_return[i] < result_mean - result_std){
            result_rank <- c(result_rank,"low")
        }else if (result$annual_return[i] >= result_mean - result_std & result$annual_return[i] < result_mean){
            result_rank <- c(result_rank,"medium")
        }else if(result$annual_return[i] >= result_mean & result$annual_return[i] < result_mean + result_std){
            result_rank <- c(result_rank,"high")
        }else if(result$annual_return[i] >=result_mean + result_std){
            result_rank <- c(result_rank,"very high")
        }
    }
    return(result_rank)
}
ranking <- ranking_sales(sales)
sales$value_ranking <- ranking
###############################################################
##### pridict rental prices ########
################
#
#clean sales neighborhood data, reconcile its neighborhood names with those in training data
easy <- read.csv("dataset/ProcessedRentalsData.csv")
a <- data.frame(a = sort(unique(easy$Neighborhood)))
neighborhood <- as.character(a$a)

sales <- read.csv('dataset/streetSalesData.csv')
sales$Neighborhood <- as.character(sales$Neighborhood)
salesNeighborhood <- sales$Neighborhood
salesGeo <- sales[,c("Longitude","Latitude")]
for (i in 1:dim(salesGeo)[1]){
    neig = salesNeighborhood[i]
    if (!(neig %in% neighborhood)){
        d <- distMeeus(salesGeo[i,],easy[,c("Longitude","Latitude")])
        b <- easy$Neighborhood[which(d == min(d))]
        salesNeighborhood[i] = as.character(b[1])
    }
}
sales$Neighborhood <- salesNeighborhood
sales$Highlights <- as.numeric(sales$Highlights)
sales$BuildingAmenity <- as.numeric(sales$BuildingAmenity)
sales$InternalAmenity <- as.numeric(sales$InternalAmenity)
sales$OutdoorSpace <- as.numeric(sales$OutdoorSpace)


neighbor_label <- as.character(sales$Neighborhood)
NMatrix <- c()
for(i in 1:length(neighbor_label)){
    n <- neighbor_label[i]
    neighbor_row <- as.integer(n == neighborhood)
    NMatrix <- rbind(NMatrix,neighbor_row)
}
NMatrix <- as.data.frame(NMatrix)
names(NMatrix) <- neighborhood

#tranform data
data = sales
data <- data %>%
    mutate(logBusCounts = log(BusCounts+1),logBusDist = log(BusDist + 1),
           logSubCounts = log(SubCounts + 1), logSize = log(Size),
           logSubDist = log(SubDist + 1), logSerPhi = log(SerPhi + 1),
           logSchPhi = log(SchPhi + 1),logArtsPhi = log(ArtsPhi+1),
           logRecPhi = log(RecPhi+1), logRestPhi = log(RestPhi+1))


data <- data %>%
    mutate(logBusCounts = scale(logBusCounts),logBusDist = scale(logBusDist),
           logSubCounts = scale(logSubCounts), logSize = scale(logSize),
           logSubDist = scale(logSubDist), logSerPhi = scale(logSerPhi),
           logSchPhi = scale(logSchPhi),logArtsPhi = scale(logArtsPhi),
           logRecPhi = scale(logRecPhi), logRestPhi = scale(logRestPhi),
           Bedroom = scale(Bedroom), Bathroom = scale(Bathroom),
           Highlights = scale(Highlights),BuildingAmenity = scale(BuildingAmenity),
           InternalAmenity = scale(InternalAmenity),OutdoorSpace = scale(OutdoorSpace))


ridge_selection <- c("logBusCounts","logBusDist",
                     "logSubCounts","logSize","logSubDist",
                     "logSerPhi", "logSchPhi", "logArtsPhi",
                     "logRecPhi","logRestPhi", 
                     "Bedroom", "Bathroom", "Highlights",
                     "BuildingAmenity", "Parking", "Gym",
                     "SwimmingPool","InternalAmenity", "OutdoorSpace")


# ridge_selection <- c("Price","logBusCounts",
#                      "logSubCounts","logSize",
#                      "logRecPhi", "Neighborhood",
#                      "Bedroom", "Bathroom", "Highlights",
#                      "BuildingAmenity", "Parking", "Gym",
#                      "SwimmingPool","InternalAmenity", "OutdoorSpace")
ridge_data <- data[,ridge_selection]
row.names(NMatrix) <- row.names(ridge_data)
ridge_data <- cbind(ridge_data,NMatrix)
ridge_data <- as.matrix(ridge_data)
#mm <- model.matrix(Price ~., data = ridge_data)
logPrice <- predict(glmmod,ridge_data)
Price <- exp(logPrice)
sales$rentalPrice <- Price
rentToPrice <- Price*12/sales$Price
sales$rTpRatio <- rentToPrice
RatioRank <- RentToPrice_Rank(rentToPrice)
sales$RatioRank <- RatioRank


write.csv(sales,file='streetSalesData.csv',row.names = F)



#####################
####describe a property's history on rental website
####
history <- read.csv('rentalHistory.csv')

#get rid of duplicated rows
historyDup <- history %>% group_by(Address) %>%
    filter(n() >1)
historyDup <- historyDup %>%
    arrange(Address,Borough, desc(Page))
historyDup <- historyDup[!duplicated(historyDup[,c('Address','Borough')]),]
history <- history %>% group_by(Address) %>%
    filter(n() <2)
history <- rbind(history,historyDup)





h <- history$History
rent_out <- c()
ro_score <- c()
for(i in 1:length(h)){
    if(h[i] <= 7){
        rent_out <- c(rent_out,"very fast")
        ro_score <- c(ro_score,4)
    }
    else if(h[i] > 7 & h[i] <= 14){
        rent_out <- c(rent_out, "fast")
        ro_score <- c(ro_score, 3)
    }
    else if (h[i] > 14 & h[i] <=30){
        rent_out <- c(rent_out, "normal")
        ro_score <- c(ro_score, 2)
    }
    else if(h[i] > 30 & h[i] <= 60){
        rent_out <- c(rent_out, "slow")
        ro_score <- c(ro_score, 1)
    }
    else if (h[i] > 60){
        rent_out <- c(rent_out, "very slow")
        ro_score <- c(ro_score, 0)
    }
}

history$rent_out <- rent_out
history$ro_score <- ro_score

history <- tbl_df(history)
n_popularity <- history %>%
    group_by(Neighborhood)%>%
    filter(n()> 10) %>%
    summarise(popular_score=mean(ro_score))%>%
    arrange(Neighborhood)




map <- get_map(location='New York City', zoom = 10,color = "bw",maptype = 'roadmap')
Ratio_visualize(sales)
firstTry <- sales %>%
    filter(Price <= 300000)%>%
    filter(value_ranking == "very high") %>%
    filter(RatioRank == "very high")
firstTry <- sales[sales$Price < 300000,]
firstTry <- firstTry[firstTry$value_ranking == "very high",]
firstTry <- firstTry[firstTry$RatioRank == "very high",]

neighborhood_name <- as.character(n_popularity$Neighborhood)
neighborhood_score <- as.numeric(n_popularity$popular_score)
candidate_neighborhood <- as.character(firstTry$Neighborhood)
neighbor_score <- c()
for (i in 1:length(candidate_neighborhood)){
    nhd <- neighborhood_score[neighborhood_name == candidate_neighborhood[i]]
    if(length(nhd)>= 1){
        neighbor_score <- c(neighbor_score,nhd)
    }
    else{
        neighbor_score <- c(neighbor_score,"missing")
    }
}
firstTry$Popular_Score <- neighbor_score

####################################################
secondTry <- sales %>%
    filter(Price <= 2200000 & Price >= 1500000)%>%
    filter(value_ranking == "very high") %>%
    filter(RatioRank == "very high")
secondTry <- sales[sales$Price >= 1500000 & sales$Price <= 2200000,]
secondTry <- secondTry[secondTry$value_ranking == "very high"|secondTry$value_ranking == "high",]
secondTry <- secondTry[secondTry$RatioRank == "very high" | secondTry$RatioRank == "high",]
secondTry <- secondTry[,c("Page","Address","Borough","Units","Year","Building",
                          "Neighborhood","Price","Size","Bedroom","Bathroom","annual_return",
                          "value_ranking","rentalPrice","RatioRank","rTpRatio")]

write.csv(secondTry,file='propertybtw150to220.csv',row.names = F)
