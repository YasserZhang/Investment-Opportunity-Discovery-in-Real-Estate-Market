#training ridge regression model for rental prices
source('RealEstateRanking/supportFunctions.R')
source('RealEstateRanking/supportFunctions.R')
source('RealEstateRanking/Gradient_Descent.R')



library(glmnet)

#clean sales neighborhood data, reconcile its neighborhood names with those in training data
a <- data.frame(a = sort(unique(easy$Neighborhood)))
neighborhood <- as.character(a$a)
sales <- read.csv('dataset/streetSalesData.csv')
sales$Neighborhood <- as.character(sales$Neighborhood)
salesGeo <- sales[,c("Longitude","Latitude")]

salesNeighborhood <- sales$Neighborhood
salesNeighborhood <- salesNeighborhood
for (i in 1:dim(salesGeo)[1]){
    neig = salesNeighborhood[i]
    if (!(neig %in% neighborhood)){
        d <- distMeeus(salesGeo[i,],easy[,c("Longitude","Latitude")])
        b <- easy$Neighborhood[which(d == min(d))]
        salesNeighborhood[i] = as.character(b[1])
    }
}
sales$Neighborhood <- salesNeighborhood


neighbor <- sales[,c("Price","Bedroom","Bathroom","Neighborhood")]
lm0 <- lm(Price ~ Neighborhood, data = neighbor)

easy <- read.csv("dataset/ProcessedRentalsData.csv")
easy$Highlights <- as.numeric(easy$Highlights)
easy$BuildingAmenity <- as.numeric(easy$BuildingAmenity)
easy$InternalAmenity <- as.numeric(easy$InternalAmenity)
easy$OutdoorSpace <- as.numeric(easy$OutdoorSpace)

#select features for training
# selection <- c("Neighborhood","Price","Size","Bedroom","Bathroom","Highlights",
#                "BuildingAmenity","Parking", "Gym", "SwimmingPool",
#                "InternalAmenity", "OutdoorSpace","BusCounts","BusDist",
#                "SubCounts", "SubDist","SerPhi", "SchPhi",
#                "ArtsPhi","RestPhi", "RecPhi", "b1925",
#                "b1925_1973", "b1973_2016")


# transform_selection <- c("Price","Size","Bedroom","Bathroom","Highlights",
#                "BuildingAmenity","BusCounts","BusDist",
#                "SubCounts", "SubDist","SerPhi", "SchPhi",
#                "ArtsPhi","RestPhi", "RecPhi")

# rentals <- easy[,selection]
# cor(rentals[,numeric_selection])
#create Neighborhood matrix
a <- data.frame(a = sort(unique(easy$Neighborhood)))
neighborhood <- as.character(a$a)
neighbor_label <- as.character(easy$Neighborhood)
training_NMatrix <- c()
for(i in 1:length(neighbor_label)){
    n <- neighbor_label[i]
    neighbor_row <- as.integer(n == neighborhood)
    training_NMatrix <- rbind(training_NMatrix,neighbor_row)
}
training_NMatrix <- as.data.frame(training_NMatrix)
names(training_NMatrix) <- neighborhood
#tranform data
data = easy
data <- data %>%
    mutate(logPrice = log(Price), logBusCounts = log(BusCounts+1),logBusDist = log(BusDist + 1),
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
# ridge_selection <- c("logPrice","logBusCounts",
#                      "logSubCounts","logSize", "Neighborhood",
#                      "Bedroom", "Bathroom", "Highlights",
#                      "BuildingAmenity", "Parking", "Gym",
#                      "SwimmingPool","InternalAmenity", "OutdoorSpace")

ridge_selection <- c("logBusCounts","logBusDist",
                     "logSubCounts","logSize","logSubDist",
                     "logSerPhi", "logSchPhi", "logArtsPhi",
                     "logRecPhi","logRestPhi",
                     "Bedroom", "Bathroom", "Highlights",
                     "BuildingAmenity", "Parking", "Gym",
                     "SwimmingPool","InternalAmenity", "OutdoorSpace")
ridge_data <- data[,ridge_selection]
row.names(training_NMatrix) <- row.names(ridge_data)
ridge_data <- cbind(ridge_data,training_NMatrix)
ridge_data <- as.matrix(ridge_data)

#mm <- model.matrix(logPrice ~., data = ridge_data)

#plot(glmmod, xvar="lambda")
cv.glmmod <- cv.glmnet(ridge_data, y=data$logPrice, alpha=0,family = "gaussian")
plot(cv.glmmod)
min.lambda = cv.glmmod$lambda.min
glmmod <- glmnet(ridge_data, y=data$logPrice, alpha=0, family="gaussian",lambda = min.lambda)
logPrice <- predict(glmmod,ridge_data)
#logPrice <- norm.reverse(p_logPrice, min_value,max_value)
Price <- exp(logPrice)
mse(easy$Price,Price)





#logPrice <- norm.reverse(p_logPrice, min_value,max_value)



