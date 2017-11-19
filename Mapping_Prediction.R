################
#mapping
#############################################################################
## the codes reproduce density contour maps for ground truth and prediction ######
###########################################################################
#load in predicted results of training data
#result <- read.csv('Predicted_Results.csv')
result <- read.csv('PredictedResults2.csv')
map <- get_map(location='New York City', zoom = 10,color = "bw",maptype = 'roadmap')
prediction_visualize(result)

########discretize truth and prediction
#discretize ground truth return rate
truth_mean = mean(result$annual_return)
truth_std = mean(sapply(result$annual_return,FUN = function(x){abs(x-truth_mean)}))
truth_rank <- c()
for(i in 1:dim(result)[1]){
    if(result$annual_return[i] < truth_mean - truth_std){
        truth_rank <- c(truth_rank,"low")
    }else if (result$annual_return[i] >= truth_mean - truth_std & result$annual_return[i] < truth_mean){
        truth_rank <- c(truth_rank,"medium")
    }else if(result$annual_return[i] >= truth_mean & result$annual_return[i] < truth_mean + truth_std){
        truth_rank <- c(truth_rank,"high")
    }else if(result$annual_return[i] >=truth_mean + truth_std){
        truth_rank <- c(truth_rank,"very high")
    }
}
result$Ground_Truth_Rank <- truth_rank
result$Ground_Truth_Rank <- factor(result$Ground_Truth_Rank,levels = c("very high","high","medium","low"))


#discretize predicted return rates
pred_mean = mean(result$predValue)
pred_std = mean(sapply(result$predValue,FUN = function(x){abs(x-pred_mean)}))
pred_rank <- c()
for(i in 1:dim(result)[1]){
    if(result$predValue[i] < pred_mean - pred_std){
        pred_rank <- c(pred_rank,"low")
    }else if (result$predValue[i] >= pred_mean - pred_std & result$predValue[i] < pred_mean){
        pred_rank <- c(pred_rank,"medium")
    }else if(result$predValue[i] >= pred_mean & result$predValue[i] < pred_mean + pred_std){
        pred_rank <- c(pred_rank,"high")
    }else if(result$predValue[i] >=pred_mean + pred_std){
        pred_rank <- c(pred_rank,"very high")
    }
}
result$Predicted_Rank <- pred_rank
result$Predicted_Rank <- factor(result$Predicted_Rank,levels = c("very high","high","medium","low"))
#

#mapping the ground truth return rate
map <- get_map(location='New York City', zoom = 10,color = "bw",maptype = 'roadmap')
mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = Ground_Truth_Rank), size = 1.5,data = result, alpha = 0.8)+
    scale_colour_manual(values = heat.colors(20)[c(1,6,11,17)])
mapPoints

#mapping the predicted return value
map <- get_map(location='New York City', zoom = 10,color = "bw",maptype = 'roadmap')
mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = Predicted_Rank), size = 1.5,data = result, alpha = 0.8)+
    scale_colour_manual(values = heat.colors(20)[c(1,6,11,17)])
mapPoints
