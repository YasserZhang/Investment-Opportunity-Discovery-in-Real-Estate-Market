#testing and application
#setwd("E:/Dropbox/URclasses/DataMining/final project")
source('RealEstateRanking/supportFunctions.R')
source('RealEstateRanking/supportFunctions.R')
source('RealEstateRanking/Gradient_Descent.R')
source('RealEstateRanking/kendall_test.R')
source('RealEstateRanking/Estep.R')
#############################################################
#### execute the codes only after EMSteps.R is executed. ####
#############################################################



#predicted result
result <- cbind(netdata[,c("Longitude","Latitude","annual_return","r")],final_ft,final_rt)
names(result) <- c("Longitude","Latitude","annual_return","r","predValue","predR")
result$predEta <- final_eta

#get predicted means and covariances of business areas
k=25
final_r_mu = matrix(rep(0,2*k),ncol=2)
for (i in 1:k){
    #mu_r = mean of all li in area r
    final_r_mu[i,] <- apply(result[result$predR == i,c("Longitude","Latitude")],2,mean)
}
final_r_mu <- cbind(final_r_mu,c(rep(1,5),rep(3,10),rep(2,5),rep(5,5)))
final_r_sigma = lapply(1:k,FUN=function(x)Cal_sigma(x,geoData = result[,c("Longitude","Latitude")], rt_list = result$predR))



##valiation using testing data

#prepare testing data
test_selection <- c("BOROUGH","SALE.PRICE", "mean_price","difference","date_diff",
               "year_diff","annual_return","Longitude","Latitude","GeoAddress",
               "id","BusCounts","BusDist","subCounts","subDist","compCounts",
               "crimeCount","SerPhi","SchPhi","ArtsPhi","restPhi","recPhi",
               "UnivPhi","SerCounts","SchCounts","ArtsCounts","RestCounts",
               "RecCounts","UnivCounts","assessedReturn","r","Families","Condos","Coops","Rentals")

testdata <- test[,test_selection]
#create geographical features for gamma calculation
testdata <- testdata %>%
    mutate(logBusDist = log(BusDist + 1), logSubCounts = log(subCounts + 1),
           logSubDist = log(subDist + 1), logSerCounts = log(SerCounts + 1),
           logNoiseCounts = log(compCounts + 1),logCrimeCounts = log(crimeCount + 1),
           logSchCounts = log(SchCounts + 1),logArtsCounts = log(ArtsCounts+1),
           logRecCounts = log(RecCounts+1), logRestCounts = log(RestCounts+1), logUnivCounts = log(UnivCounts+1))

#normalize the features for neural networks algorithm
norm.fun = function(x){ 
    (x - min(x))/(max(x) - min(x)) 
}
#
testdata <- testdata %>%
    mutate(BusCounts = norm.fun(BusCounts),logBusDist = norm.fun(logBusDist),
           logSubCounts = norm.fun(logSubCounts),logSubDist = norm.fun(logSubDist),
           logSerCounts = norm.fun(SerCounts), logSchCounts = norm.fun(SchCounts),
           logNoiseCounts = norm.fun(logNoiseCounts),logCrimeCounts = norm.fun(logCrimeCounts),
           logArtsCounts = norm.fun(ArtsCounts),logRecCounts = norm.fun(RecCounts),
           logRestCounts = norm.fun(RestCounts),logRestCounts = norm.fun(logRestCounts))

test_pd_selection <- c("BusCounts","logBusDist", "logSubCounts","logSubDist",
                  "logSerCounts","logSchCounts","logArtsCounts",
                  "logRecCounts","logRestCounts","logNoiseCounts",
                  "logCrimeCounts","logUnivCounts","Families","Condos","Coops","Rentals")
test_pd <- testdata[,test_pd_selection] # 30 UnivCounts
test_pd <- cbind(rep(1,dim(test_pd)[1]),test_pd)
test_pd <- as.matrix(test_pd)
#calculate gamma for test data
test_gamma =  t(final_q %*% final_weights %*% t(test_pd)) + test$annual_return
#calculate rho
final_phi_data <- phi_data[,c("SerPhi","SchPhi","ArtsPhi","restPhi","recPhi","UnivPhi")]
final_phi_data <- cbind(final_phi_data, final_rt)

test_rho <- apply(test[,c("Longitude","Latitude")],1,FUN=function(x) Cal_rho(estateGeo = x,centers = final_r_mu[,1:2],etas = final_r_eta[,1], d0 = 3000))
#calculate delta
#test_phi_data <- test[,c("BOROUGH","SerPhi","SchPhi","ArtsPhi","restPhi","recPhi","UnivPhi")]
#names(test_phi_data) <- c("BOROUGH","SerPhi","SchPhi","ArtsPhi","RestPhi","RecPhi","UnivPhi")
names(test)[c(41,42)] <- c("RestPhi","RecPhi")
names(test)


test_man <- test %>% filter(BOROUGH == 1)
test_BQ <- test %>% filter(BOROUGH == 3)
test_bronx <- test %>% filter(BOROUGH == 2)
test_staten <- test %>% filter(BOROUGH == 5)


test_man_delta <- Predict_delta(test_man,1:5,final_phi_data,final_r_eta)
test_BQ_delta <- Predict_delta(test_BQ,6:15,final_phi_data,final_r_eta)
test_bronx_delta <- Predict_delta(test_bronx,16:20,final_phi_data,final_r_eta)
test_staten_delta <- Predict_delta(test_staten,21:25,final_phi_data,final_r_eta)
test_man$delta <- test_man_delta
test_BQ$delta <- test_BQ_delta
test_bronx$delta <- test_bronx_delta
test_staten$delta <- test_staten_delta
temp <- rbind(test_man,test_BQ,test_bronx,test_staten)
test <- temp
test$gamma <- as.vector(test_gamma)
test$rho <- test_rho
test$predictRR <- test$gamma+test$rho+test$delta

rm(temp,test_BQ,test_bronx,test_staten,test_man)




##########
#######predict return rates for testing dataset
#########change it!!!
test$test_r <- 0
test$test_f <- 0
for (zone in c(1,2,3,5)){
    subsets <- test[test$BOROUGH == zone,]
    sub_phi <- test_phi_data[test$BOROUGH == zone,]
    sub_gamma <- test_gamma[test$BOROUGH == zone]
    sub_rho <- test_rho[test$BOROUGH == zone]
    ranges = unique(test[test$BOROUGH == zone,]$r)
    ranges = sort(ranges)
    start = min(ranges)
    last = max(ranges)
    k = length(ranges)
    test_final_f <- c()
    test_final_r <- c()
    for (i in 1:dim(subsets)[1]){
        test_prob_id <- c()
        test_prob_pd <- c()
        test_prob_zd <- c()
        test_p_etat <- c()
        t_f <- c()
        #t_r <- c()
        for (t in start:last){
            phi_data_i = c(sub_phi[i,],t)
            subdata <- final_phi_data[final_phi_data$final_rt == t,]
            max_array <- apply(subdata[,1:(dim(subdata)[2]-1)],2,max)
            L= length(sub_phi) -1
            estate_phi = sub_phi[i, 1:L]
            delta_i = sum(estate_phi/(max_array+1))/(L-1)
            test_fi = sub_gamma[i] + sub_rho[i] + delta_i
            t_f <- c(t_f,test_fi)
            test_prob_id_t <- dmnorm(subsets[i,c("Longitude","Latitude")],mean = final_r_mu[t,1:2],final_r_sigma[[t]])
            test_prob_id <- c(test_prob_id,log(test_prob_id_t))
            fh = result[result$predR==t,]$predValue
            test_prob_pd_t <- sum(log(1/(1+exp(fh - test_fi))))
            test_prob_pd <- c(test_prob_pd, test_prob_pd_t/length(fh)^0.65)
            eta_i <- final_r_eta[t,1]
            etah <- result[result$predR!=t,]$predEta
            test_prob_zd_t <- sum(log(1/(1+exp(-(eta_i - etah)))))
            test_prob_zd <- c(test_prob_zd, test_prob_zd_t/length(etah)^0.65)
            test_p_etat <- c(test_p_etat, log(eta_i/sum(final_r_eta[,1])))
            
        }
        test_p_r <- test_prob_id + test_prob_pd + test_p_etat + test_prob_zd
        final_ft
        test_r_i <- which(test_p_r == max(test_p_r))
        test_final_r <- c(test_final_r,ranges[test_r_i])
        test_final_f <- c(test_final_f,t_f[test_r_i])
    }
    test[test$BOROUGH == zone,]$test_f <- test_final_f
    test[test$BOROUGH == zone,]$test_r <- test_final_r
}

#calculate Kendall's Tau coefficient, which is 0.3736967 in this case.
sortedTest <- test %>%
    arrange(desc(annual_return))
kendall_test(sortedTest$annual_return,sortedTest$test_f)

######################################################
#### Model Application to rental property data   ######
######################################################
#create features for rental property data
easy <- read.csv('easySample.csv',header=T)
easyGeo <- easy[,c("Longitude","Latitude")]
university <- read.csv('dataset/POI/educations/Cleaned_university.csv')
university[18,]$Longitude <- -73.9626
university[18,]$Latitude <- 40.8075
uniGeo <- university[,c("Longitude","Latitude")]
taxi <- read.csv('dataset/taxi/yellow_sample1.csv')
taxGeo <- taxi[,3:4]
beta1=0.8
beta2=60
taxi_trace = taxGeo
uniTotal <- apply(uniGeo,1,FUN=function(x) Pvisit(p1=x))
university$visitProb <- uniTotal
UnivCounts <- apply(easyGeo,1,FUN = function(x)countPOI(estateGeo=x,poi=university))
easy$UnivPhi <- apply(easyGeo,1,FUN = function(x)findPOI(estateGeo=x,poi=university,threshold = 1500))
easy$UnivCounts <- UnivCounts
SerCounts <- apply(easyGeo,1,FUN = function(x)countPOI(estateGeo=x,poi=service))
SchCounts <- apply(easyGeo,1,FUN = function(x)countPOI(estateGeo=x,poi=schools))
ArtsCounts <- apply(easyGeo,1,FUN = function(x)countPOI(estateGeo=x,poi=arts))
RestCounts <- apply(easyGeo,1,FUN = function(x)countPOI(estateGeo=x,poi=restaurants))
RecCounts <- apply(easyGeo,1,FUN = function(x)countPOI(estateGeo=x,poi=recreations))
easy <- cbind(easy,SerCounts,SchCounts,ArtsCounts,RestCounts,RecCounts)

#estimate assessed return rate of rental property
result$assessedReturn <- netdata$assessedReturn
easy$assessedValue <- apply(easyGeo,1,FUN=function(x)findAssessedReturn(estateGeo=x,poi=result,threshold = 300))

#find Borough index for rental data
#load in training data, compare the coordinates of two datasets,
#among properties in the training set, the Borough index of the closest one to a property
#in rental data is assigned as the borough index of the rental property.
bor <- c()
for (i in 1:dim(easyGeo)[1]){
    d <- distMeeus(easyGeo[i,],netdata[,c("Longitude","Latitude")])
    b <- netdata$BOROUGH[which(d == min(d))]
    bor <- c(bor,b[1])
}
easy$BOROUGH <- as.integer(bor)
#find value
#prepare testing data
easy_selection <- c("BOROUGH","Longitude","Latitude",
                    "BusCounts","BusDist","SubCounts","SubDist","ComplaintCounts",
                    "CrimeCounts","SerPhi","SchPhi","ArtsPhi","RestPhi","RecPhi",
                    "UnivPhi","SerCounts","SchCounts","ArtsCounts","RestCounts",
                    "RecCounts","UnivCounts","assessedValue")
easydata <- easy[,easy_selection]
easydata <- easydata %>%
    mutate(logBusDist = log(BusDist + 1), logSubCounts = log(SubCounts + 1),
           logSubDist = log(SubDist + 1), logSerCounts = log(SerCounts + 1),
           logNoiseCounts = log(ComplaintCounts + 1),logCrimeCounts = log(CrimeCounts + 1),
           logSchCounts = log(SchCounts + 1),logArtsCounts = log(ArtsCounts+1),
           logRecCounts = log(RecCounts+1), logRestCounts = log(RestCounts+1), logUnivCounts = log(UnivCounts+1))

#normalize features
norm.fun = function(x){ 
    (x - min(x))/(max(x) - min(x)) 
}
easydata <- easydata %>%
    mutate(BusCounts = norm.fun(BusCounts),logBusDist = norm.fun(logBusDist),
           logSubCounts = norm.fun(logSubCounts),logSubDist = norm.fun(logSubDist),
           logSerCounts = norm.fun(SerCounts), logSchCounts = norm.fun(SchCounts),
           logNoiseCounts = norm.fun(logNoiseCounts),logCrimeCounts = norm.fun(logCrimeCounts),
           logArtsCounts = norm.fun(ArtsCounts),logRecCounts = norm.fun(RecCounts),
           logRestCounts = norm.fun(RestCounts),logRestCounts = norm.fun(logRestCounts))

#repreentation of property value gamma
easy_pd_selection <- c("BusCounts","logBusDist", "logSubCounts","logSubDist",
                       "logSerCounts","logSchCounts","logArtsCounts",
                       "logRecCounts","logRestCounts","logNoiseCounts",
                       "logCrimeCounts","logUnivCounts")
easy_pd <- easydata[,easy_pd_selection] # 30 UnivCounts
easy_pd <- cbind(rep(1,dim(easy_pd)[1]),easy_pd)
easy_pd <- as.matrix(easy_pd)
#calculate gamma for easy data
easy_gamma =  t(final_q %*% final_weights %*% t(easy_pd)) + easy$assessedValue

final_phi_data <- phi_data[,c("SerPhi","SchPhi","ArtsPhi","restPhi","recPhi","UnivPhi")]
final_phi_data <- cbind(final_phi_data, final_rt)
easy_phi_data <- easy[,c("SerPhi","SchPhi","ArtsPhi","RestPhi","RecPhi","UnivPhi")]
easy_rho <- apply(easy[,c("Longitude","Latitude")],1,FUN=function(x) Cal_rho(estateGeo = x,centers = final_r_mu[,1:2],etas = final_r_eta[,1], d0 = 3000))

#predict annual return rate.
easy$BOROUGH <- as.factor(easy$BOROUGH)
easy$easy_r <- 0
easy$easy_f <- 0
for (zone in c(1,2,3,5)){
    subsets <- easy[easy$BOROUGH == zone,]
    sub_phi <- easy_phi_data[easy$BOROUGH == zone,]
    sub_gamma <- easy_gamma[easy$BOROUGH == zone]
    sub_rho <- easy_rho[easy$BOROUGH == zone]
    ranges = unique(test[test$BOROUGH == zone,]$r)
    ranges = sort(ranges)
    start = min(ranges)
    last = max(ranges)
    k = length(ranges)
    easy_final_f <- c()
    easy_final_r <- c()
    for (i in 1:dim(subsets)[1]){
        easy_prob_id <- c()
        easy_prob_pd <- c()
        easy_prob_zd <- c()
        easy_p_etat <- c()
        e_f <- c()
        #t_r <- c()
        for (t in start:last){
            phi_data_i = c(sub_phi[i,],t)
            subdata <- final_phi_data[final_phi_data$final_rt == t,]
            max_array <- apply(subdata[,1:(dim(subdata)[2]-1)],2,max)
            L= length(sub_phi) -1
            estate_phi = sub_phi[i, 1:L]
            delta_i = sum(estate_phi/(max_array+1))/(L-1)
            easy_fi = sub_gamma[i] + sub_rho[i] + delta_i
            e_f <- c(e_f,easy_fi)
            easy_prob_id_t <- dmnorm(subsets[i,c("Longitude","Latitude")],mean = final_r_mu[t,1:2],final_r_sigma[[t]])
            easy_prob_id <- c(easy_prob_id,log(easy_prob_id_t))
            fh = result[result$predR==t,]$predValue
            easy_prob_pd_t <- sum(log(1/(1+exp(fh - easy_fi))))
            easy_prob_pd <- c(easy_prob_pd, easy_prob_pd_t/length(fh)^0.65)
            eta_i <- final_r_eta[t,1]
            etah <- result[result$predR!=t,]$predEta
            easy_prob_zd_t <- sum(log(1/(1+exp(-(eta_i - etah)))))
            easy_prob_zd <- c(easy_prob_zd, easy_prob_zd_t/length(etah)^0.65)
            easy_p_etat <- c(easy_p_etat, log(eta_i/sum(final_r_eta[,1])))
            
        }
        easy_p_r <- easy_prob_id + easy_prob_pd + easy_p_etat + easy_prob_zd
        easy_r_i <- which(easy_p_r == max(easy_p_r))
        easy_final_r <- c(easy_final_r,ranges[easy_r_i])
        easy_final_f <- c(easy_final_f,e_f[easy_r_i])
    }
    easy[easy$BOROUGH == zone,]$easy_f <- easy_final_f
    easy[easy$BOROUGH == zone,]$easy_r <- easy_final_r
}

# write.csv(easy,file='dataset/Final_Rental_Property.csv',row.names = F)
# write.csv(result,file='dataset/Predicted_Results.csv',row.names = F)
# write.csv(test,file = 'dataset/Predicted_Test_Data.csv',row.names = F)

### exploring easy data
#####
### using mean absolute deviation
#####
easyfmean <- mean(easy$easy_f)
easystd <- mean(sapply(easy$easy_f,FUN = function(x){abs(x - easyfmean)}))
pred_rank <- c()
for(i in 1:dim(easy)[1]){
    if(easy$easy_f[i] < easyfmean - easystd){
        pred_rank <- c(pred_rank,"low")
    }else if (easy$easy_f[i] >= easyfmean - easystd & easy$easy_f[i] < easystd){
        pred_rank <- c(pred_rank,"medium")
    }else if(easy$easy_f[i] >= easyfmean & easy$easy_f[i] < easyfmean + easystd){
        pred_rank <- c(pred_rank,"high")
    }else if(easy$easy_f[i] >=easyfmean + easystd){
        pred_rank <- c(pred_rank,"very high")
    }
}
easy$prediction <- pred_rank

#mapping predicted data
easy$prediction <- factor(easy$prediction,levels = c("very high","high","medium","low"))
map <- get_map(location='New York City', zoom = 11,color = "bw",maptype = 'roadmap')
mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = prediction), size = 1.5,data = easy, alpha = 0.8)+
    scale_colour_manual(values = heat.colors(25)[c(1,6,14,24)])
mapPoints

##History threshold
history_mean = mean(log(easy$History+1))
history_std = sd(log(easy$History+1))
Rent.Out.Speed <- c()

for(i in 1:dim(easy)[1]){
    if(hist_log[i] < history_mean - history_std){
        Rent.Out.Speed <- c(Rent.Out.Speed,"very fast")
    }else if (hist_log[i] >= history_mean - history_std & hist_log[i] < history_mean){
        Rent.Out.Speed <- c(Rent.Out.Speed,"fast")
    }else if(hist_log[i] >= history_mean& hist_log[i] < history_mean + history_std){
        Rent.Out.Speed <- c(Rent.Out.Speed,"medium")
    }else if(hist_log[i] >=history_mean + history_std){
        Rent.Out.Speed <- c(Rent.Out.Speed,"slow")
    }
}

easy$Rent.Out.Speed <- Rent.Out.Speed
easy$Rent.Out.Speed <- factor(easy$Rent.Out.Speed, levels = c("very fast","fast","medium","slow"))
mapPoints <- ggmap(map) +geom_point(aes(x = Longitude, y = Latitude, colour = Rent.Out.Speed), size = 1.5,data = easy, alpha = 0.8)+
    scale_colour_manual(values = heat.colors(20)[c(1,6,11,17)])
mapPoints

#high rank in value and history
filteredData <- c()
for (i in 1:dim(easy)[1]){
    if(easy$Rent.Out.Speed[i] %in% c("very fast","fast") & easy$prediction[i] %in% c("very high","high")){
        filteredData <- rbind(filteredData,easy[i,])
    }
}

map <- get_map(location=c(-73.98594, 40.72278), zoom = 12,color = "bw",maptype = 'roadmap')
mapPoints <- ggmap(map)+
    geom_density2d(data = filteredData, 
                   aes(x = Longitude, y = Latitude), size = 0.3)+
    stat_density2d(data = filteredData,
                   aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                   size = 0.01,bins = 25, geom = "polygon") +
    scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.5), guide = FALSE)
mapPoints
#medium rank in value and high rank in history
mediumRankData <- c()
for (i in 1:dim(easy)[1]){
    if(easy$Rent.Out.Speed[i] %in% c("very fast","fast") & easy$prediction[i] %in% c("medium")){
        mediumRankData <- rbind(mediumRankData,easy[i,])
    }
}
#maping filtered data on density contour map
mapPoints <- ggmap(map)+
    geom_density2d(data = mediumRankData, 
                   aes(x = Longitude, y = Latitude), size = 0.3)+
    stat_density2d(data = mediumRankData,
                   aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                   size = 0.01,bins = 25, geom = "polygon") +
    scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.5), guide = FALSE)
mapPoints
