#functions
library(neuralnet)
library(ggmap)
library(dplyr)
library(geosphere)
library(mvtnorm)
library(cluster)
library(mnormt)
library(colorspace)
#calculate annual rate
annualReturn <- function(return, year){
    ar <- (return+1)^(1/year) - 1
}

#calculate monthly return rate
monthlyReturn <- function(return, month){
    mr <- (return+1)^(1/month) - 1
}


#calculate distance and count the nearest places
CalKNNstations <- function(point,busStation, threshold = 100){
    distance <- distMeeus(point, busStation)
    counts <- sum(distance < threshold)
    nearestDist <- min(distance)
    return(c(counts, nearestDist))
}

#calculate distance and count the nearest places
CalKNNcrime <- function(point,crimeVenue, threshold = 1000){
    distance <- distMeeus(point, crimeVenue)
    counts <- sum(distance < threshold)
    return(counts)
}

#extract geo information
extractGeoLocation <- function(address){
    address <- as.character(address)
    t <- strsplit(as.character(address),split="\n")
    indx <- grep('[()]',t[[1]])
    t <- t[[1]][indx]
    t <- gsub('[()]','',t)
    t <- strsplit(t,split=',')
    t <- t[[1]]
    t <- as.numeric(t)
    return(t)
}
#using geocode to invoke google map api to extract geo information
get_geocode <- function(address){
    geocode(address,output='latlona')
    
}
#modify extracted information
wrap_up <- function(data){
    geoInfo <- sapply(data$comp_address, get_geocode)
    geoData <- t(geoInfo)
    geoData <- as.data.frame(geoData)
    geoData <- tbl_df(geoData)
    names(geoData) <- c("Longitude","Latitude","GeoAddress")
    return(geoData)
}

#normalize the data
norm.fun <- function(x){ 
    (x - min(x))/(max(x) - min(x)) 
}

norm.reverse <- function(y, min_value, max_value){
    y*(max_value - min_value) + min_value
}
Pvisit <- function(p1,p2 = taxi_trace,b1 = beta1, b2 = beta2){
    d <- distMeeus(p1,p2)
    px <- (b1/b2)*d*exp(1-d/b2)
    return(sum(px))
}

findPOI <- function(estateGeo,poi,threshold = 1000){
    poiGeo <- poi[,c("Longitude","Latitude")]
    distance <- distMeeus(estateGeo, poiGeo)
    phi <- sum(poi$visitProb[distance <= 1000])
}
countPOI <- function(estateGeo,poi,threshold = 1000){
    poiGeo <- poi[,c("Longitude","Latitude")]
    distance <- distMeeus(estateGeo, poiGeo)
    count <- sum(distance <=1000)
    return(count)
}

Cal_rho <- function(estateGeo,centers,etas,d0 = 1){
    distance <- distMeeus(estateGeo,centers)
    rho <-  (d0/(d0 + distance))^(exp(1))*etas
    return(sum(rho)/sum(etas))
}
#estate is a dataset only having phi data
Cal_delta <- function(estate,dataset){
    r_i = estate[length(estate)]
    subdata <- dataset[dataset$r == as.numeric(r_i),]
    max_array <- apply(subdata[,1:(dim(subdata)[2]-1)],2,max)
    estate_phi = estate[1:(length(estate)-1)]
    delta = sum(estate_phi/(max_array+1))/(length(estate)-1)
    return(delta)
}


#calculate sigma for each r
#rt_list is a vector
Cal_sigma <- function(r, geoData, rt_list){
    subset <- geoData[rt_list==r,]
    sigma <- cov(subset)
    return(sigma)
}

#update eta for each r
#returnData is a vector
Cal_eta <- function(r,returnData, rt_list, k = 10){
    sum_eta <- sum(returnData)
    sub_eta <-  returnData[rt_list == r]
    eta <- (sum(sub_eta) + 1/k)/(sum_eta + 1) + 0.01
    return(eta)
}
findAssessedReturn <- function(estateGeo,poi,threshold = 1000){
    poiGeo <- poi[,c("Longitude","Latitude")]
    distance <- distMeeus(estateGeo, poiGeo)
    assessedR <- poi$assessedReturn[distance <= 1000]
    phi <- sum(assessedR)/length(assessedR)
    return(phi)
}

prediction_visualize <- function(result){
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
    result$Prediction <- truth_rank
    result$Prediction <- factor(result$Prediction,levels = c("very high","high","medium","low"))
    mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = Prediction), size = 1.2,data = result, alpha = 0.5)+
        scale_colour_manual(values = heat.colors(20)[c(1,6,11,17)])
    mapPoints
}

groundTruth_visualize <- function(result){
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
    mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = Ground_Truth_Rank), size = 1.2,data = result, alpha = 0.5)+
        scale_colour_manual(values = heat.colors(20)[c(1,6,11,17)])
    mapPoints
}

Ratio_visualize <- function(data){
    data$RatioRank <- factor(data$RatioRank, levels = c("very high","high","medium","low","very low"))
    mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = RatioRank), size = 1.6,data = data, alpha = 0.8)+
        scale_colour_manual(values = diverge_hcl(5)[5:1])
    mapPoints
}
  
mse <- function(g, p){
    mean((g-p)^2)
}

#ranking sales return rates and rent to price ratio
RentToPrice_Rank <- function(ratio){
    rank <- c()
    quant <- quantile(ratio,probs=seq(0,1,0.2), na.rm=T,names = T)
    for (i in 1:length(ratio)){
        if (ratio[i]<= quant[2]){
            rank <- c(rank,"very low")
        }
        else if (ratio[i] > quant[2] & ratio[i] <= quant[3]){
            rank <- c(rank,"low")
        }
        else if(ratio[i] > quant[3] & ratio[i] <= quant[4]){
            rank <- c(rank, "medium")
        }
        else if(ratio[i] > quant[4] & ratio[i] <= quant[5]){
            rank <- c(rank, "high")
        }
        else if(ratio[i] >quant[5]){
            rank <- c(rank,"very high")
        }
    }
    return(rank)
}




#extract POI counts and
estimateTimeCost <- function(duration,rowN,testedRows){
    t <- (duration*rowN/testedRows)/3600
    print("estimated time cost for all rows in hours")
    print(t)
}
