###############################################
#############   training algorithm    ##################
###############################################
#setwd("~/Dropbox/URclasses/DataMining/final project")
#
setwd("E:/Dropbox/URclasses/DataMining/final project")
######
#### please set the working directory under the folder "final project"
######
source('RealEstateRanking/supportFunctions.R')
source('RealEstateRanking/Gradient_Descent.R')
source('RealEstateRanking/kendall_test.R')
source('RealEstateRanking/Estep.R')

options(digits = 10)
options(scipen=999)
###############################################
############     prepare data   ###############
###############################################
#load in preprocessed data
df <- read.csv('salesPropForAnalysis.csv',header = T)
#extract features out of BUILDING CLASS CATEGORY
class <- df$BUILDING.CLASS.CATEGORY
class <- as.character(class)
class <- sapply(class,FUN = function(x){strsplit(x,split=" ")[[1]][1]})
df <- cbind(df,class)
# [1] 13  CONDOS - ELEVATOR APARTMENTS            
# [2] 10  COOPS - ELEVATOR APARTMENTS             
# [3] 07  RENTALS - WALKUP APARTMENTS             
# [4] 02  TWO FAMILY HOMES                        
# [5] 15  CONDOS - 2-10 UNIT RESIDENTIAL          
# [6] 09  COOPS - WALKUP APARTMENTS               
# [7] 01  ONE FAMILY HOMES                        
# [8] 17  CONDOPS                                 
# [9] 08  RENTALS - ELEVATOR APARTMENTS           
# [10] 16  CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT 
# [11] 12  CONDOS - WALKUP APARTMENTS              
# [12] 04  TAX CLASS 1 CONDOS                      
# [13] 03  THREE FAMILY HOMES                      
# [14] 14  RENTALS - 4-10 UNIT                     
# [15] 17  CONDO COOPS                             
# [16] 02  TWO FAMILY DWELLINGS                    
# [17] 03  THREE FAMILY DWELLINGS                  
# [18] 01  ONE FAMILY DWELLINGS                    
# [19] 05  TAX CLASS 1 VACANT LAND                 
# [20] 06  TAX CLASS 1 - OTHER                     
# [21] 11A CONDO-RENTALS    
family <- c("02","01","03","05","06")
condos <- c("04","12","13","15","16")
coops <- c("09","10","17")
rentals <- c("07", "08","14","11A")
Families <- c()
Condos <- c()
Coops <- c()
Rentals <- c()
for (i in 1:length(class)){
    if (class[i] %in% family){
        Families <- c(Families,1)
        Condos <- c(Condos, 0)
        Coops <- c(Coops, 0)
        Rentals <- c(Rentals,0)
    }
    else if (class[i] %in% condos){
        Families <- c(Families,0)
        Condos <- c(Condos, 1)
        Coops <- c(Coops, 0)
        Rentals <- c(Rentals,0)
    }
    else if (class[i] %in% coops){
        Families <- c(Families,0)
        Condos <- c(Condos, 0)
        Coops <- c(Coops, 1)
        Rentals <- c(Rentals,0)
    }
    else if (class[i] %in% rentals){
        Families <- c(Families,0)
        Condos <- c(Condos, 0)
        Coops <- c(Coops, 0)
        Rentals <- c(Rentals,1)
    }
}
df <- cbind(df,Families,Condos,Coops,Rentals)


df <- df[df$difference > -0.3 & df$date_diff > 365,]
df <- tbl_df(df)
boxplot(annual_return ~ Families + Condos + Coops + Rentals, data=df)

set.seed(123)
df <- df[df$GeoAddress != 'brooklyn, ny, usa',]
dfman <- df[df$BOROUGH==1,]
dfbrook <- df[df$BOROUGH == 3,]
dfbronx <- df[df$BOROUGH ==2,]
dfqueens <- df[df$BOROUGH ==4,]
dfstaten <- df[df$BOROUGH ==5,]
dfBQ <- df[df$BOROUGH == 3 | df$BOROUGH ==4,]
#further clean data points with wrong geo locations
dfman <- dfman %>%
    filter(!grepl('bronx|brooklyn',GeoAddress))
b <- dfBQ %>%
    filter(!grepl('brooklyn',GeoAddress))
b <- b %>%
    filter(!grepl('new york',GeoAddress))

dfBQ <- dfBQ %>%
    filter(grepl('brooklyn',GeoAddress))
dfBQ <- rbind(dfBQ,b)
rm(b)
st <- dfstaten %>%
    filter(!grepl('staten', GeoAddress))

dfstaten <- dfstaten %>%
    filter(grepl('staten', GeoAddress))
rm(st)
dfbronx <- dfbronx %>%
    filter(!grepl('staten',GeoAddress))

#clustering each borough using kmeans
manGeo <- dfman[,c("Longitude","Latitude")]
BQGeo <- dfBQ[,c("Longitude","Latitude")]
bronxGeo <- dfbronx[,c("Longitude","Latitude")]
statenGeo <- dfstaten[,c("Longitude","Latitude")]
KmeanMan <- kmeans(manGeo,centers = 5)
KmeanBrooQueen <-kmeans(BQGeo,centers=10)
Kmeanbronx <- kmeans(bronxGeo,centers=5)
Kmeanstaten <- kmeans(statenGeo,centers=5)

#indexing clusters
dfman$r <- KmeanMan$cluster
dfBQ$r <- KmeanBrooQueen$cluster
dfBQ <- tbl_df(dfBQ)
dfBQ <- dfBQ %>%
    mutate(r=r+5)
dfbronx$r <- Kmeanbronx$cluster
dfbronx <- tbl_df(dfbronx)
dfbronx <- dfbronx %>%
    mutate(r = r + 15)
dfstaten$r <- Kmeanstaten$cluster
dfstaten <- tbl_df(dfstaten)
dfstaten <- dfstaten %>%
    mutate(r = r + 20)

set.seed(1234)
#sampling, separate data into training and testing datasets
indxman <- sample(1:dim(dfman)[1],round(dim(dfman)[1]/10))
trainman <- dfman[-indxman,]
testman <- dfman[indxman,]
indxBQ <- sample(1:dim(dfBQ)[1],round(dim(dfBQ)[1]/10))
trainBQ <- dfBQ[-indxBQ,]
testBQ <- dfBQ[indxBQ,]
indxbronx <- sample(1:dim(dfbronx)[1],round(dim(dfbronx)[1]/10))
trainbronx <- dfbronx[-indxbronx,]
testbronx <- dfbronx[indxbronx,]
indxstaten <- sample(1:dim(dfstaten)[1],round(dim(dfstaten)[1]/10))
trainstaten <- dfstaten[-indxstaten,]
teststaten <- dfstaten[indxstaten,]
train <- rbind(trainman,trainBQ,trainbronx,trainstaten)
test <- rbind(testman,testBQ,testbronx,teststaten)

#combine Queens and Brooklyn, their latent areas can interact with each other since 
# there is no river between the two boroughs
for (i in 1:dim(train)[1]){
    if (train$BOROUGH[i] == 4){
        train$BOROUGH[i] = 3
    }
}
for (i in 1:dim(test)[1]){
    if (test$BOROUGH[i] == 4){
        test$BOROUGH[i] = 3
    }
}


########
###get POI counts
######
trainGeo <- train[,c("Longitude","Latitude")]
##############
train <- train %>%
    arrange(desc(annual_return))
#clutering data
c <- pam(train$annual_return,k=3)

####  discretize the return rate  ####
train$rank1 <- 1
train$rank1 <- as.integer(train$rank1 == c$clustering)
train$rank2 <- 2
train$rank2 <- as.integer(train$rank2 == c$clustering)
train$rank3 <- 3
train$rank3 <- as.integer(train$rank3 == c$clustering)
###########
#prepare the training data for training
####single layer perceptron  ####
selection <- c("BOROUGH","SALE.PRICE", "mean_price","difference","date_diff",
               "year_diff","annual_return","Longitude","Latitude","GeoAddress",
               "id","BusCounts","BusDist","subCounts","subDist","compCounts",
               "crimeCount","SerPhi","SchPhi","ArtsPhi","restPhi","recPhi",
               "UnivPhi","SerCounts","SchCounts","ArtsCounts","RestCounts",
               "RecCounts","UnivCounts","assessedReturn","r","rank1","rank2",
               "rank3", "Families","Condos","Coops","Rentals")

netdata <- train[,selection]
netdata <- netdata %>%
    mutate(logBusDist = log(BusDist + 1), logSubCounts = log(subCounts + 1),
           logSubDist = log(subDist + 1), logSerCounts = log(SerCounts + 1),
           logNoiseCounts = log(compCounts + 1),logCrimeCounts = log(crimeCount + 1),
           logSchCounts = log(SchCounts + 1),logArtsCounts = log(ArtsCounts+1),
           logRecCounts = log(RecCounts+1), logRestCounts = log(RestCounts+1), logUnivCounts = log(UnivCounts+1))


netdata <- netdata %>%
    mutate(BusCounts = norm.fun(BusCounts),logBusDist = norm.fun(logBusDist),
           logSubCounts = norm.fun(logSubCounts),logSubDist = norm.fun(logSubDist),
           logSerCounts = norm.fun(SerCounts), logSchCounts = norm.fun(SchCounts),
           logNoiseCounts = norm.fun(logNoiseCounts),logCrimeCounts = norm.fun(logCrimeCounts),
           logArtsCounts = norm.fun(ArtsCounts),logRecCounts = norm.fun(RecCounts),
           logRestCounts = norm.fun(RestCounts),logRestCounts = norm.fun(logRestCounts))

#use single layer neuralnet to calculate W (M by N), where M is 5, N the number of feature
format_form <- rank1 + rank2 + rank3 ~ BusCounts +
    logBusDist + logSubCounts + logSubDist +
    logSerCounts + logSchCounts + logArtsCounts +
    logRecCounts +logRestCounts +logNoiseCounts +
    logCrimeCounts + logUnivCounts + Families + Condos + Coops + Rentals  # 
net <- neuralnet(format_form,data=netdata,hidden=0,stepmax = 1e5)
weights <- unlist(net$weights)
weights <- matrix(weights,ncol = 3)
#raw features of estates E
pd_selection <- c("BusCounts","logBusDist", "logSubCounts","logSubDist",
                  "logSerCounts","logSchCounts","logArtsCounts",
                  "logRecCounts","logRestCounts","logNoiseCounts",
                  "logCrimeCounts","logUnivCounts", "Families","Condos","Coops","Rentals")
pd <- netdata[,pd_selection] # 30 UnivCounts
pd <- cbind(rep(1,dim(pd)[1]),pd)
pd <- as.matrix(pd)
results <- pd %*% weights
#########
#linear aggregation for linear coefficients q
aR <- train$annual_return
results <- cbind(results,aR)
results <- as.data.frame(results)
linearModel <- lm(aR~ -1 + V1+V2+V3,data=results)

#######################################
#####  EM   ITERATIONS      ###########
########################################
#out of EM steps
#parameters needing no updates
y <- netdata$annual_return
netGeo <- as.matrix(netdata[,c("Longitude","Latitude")])
end = dim(netdata)[1]
assessedReturn <- netdata$assessedReturn
borough <- netdata$BOROUGH
#hyperparameters
k=25
std_y=sd(y)
mu_q = 0
std_q = 35
mu_w = 0
std_w = 35
#####################
#initialize parameters #####
initial_r_mu = matrix(rep(0,2*k),ncol=2)
for (i in 1:k){
    #mu_r = mean of all li in area r
    initial_r_mu[i,] <- apply(netGeo[netdata$r == i,],2,mean)
}
initial_r_mu <- cbind(initial_r_mu,c(rep(1,5),rep(3,10),rep(2,5),rep(5,5)))
initial_r_sigma = lapply(1:k,FUN=function(x)Cal_sigma(x,geoData = netGeo, rt_list = netdata$r))

#q
initial_q <- linearModel$coefficients
#W
initial_weights <- unlist(net$weights)
initial_weights <- matrix(initial_weights,ncol = 3)
initial_weights = t(initial_weights)
#delta i
initial_phi_data <- netdata[,c("SerPhi","SchPhi","ArtsPhi","restPhi","recPhi","UnivPhi","r")]
initial_phi_data$r <- as.numeric(initial_phi_data$r)
#initial_phi_data$r <- as.factor(initial_phi_data$r)
initial_delta <- apply(initial_phi_data,1, FUN=function(x)Cal_delta(estate=x,dataset = initial_phi_data))
#eta
initial_r_eta = rep(1/k,k)
initial_r_eta <- cbind(initial_r_eta,c(rep(1,5),rep(3,10),rep(2,5),rep(5,5)))
# rho
initial_rho <- apply(netGeo,1,FUN=function(x) Cal_rho(estateGeo = x,centers = initial_r_mu[,1:2],etas = initial_r_eta[,1], d0 = 3000))
# gama
linearPred <- predict(linearModel)
initial_gama <- predict(linearModel) + netdata$assessedReturn
initial_f <- initial_gama + initial_rho + initial_delta
netdata$f <- initial_f
netdata$eta <- 1/k
#step size
ss = 10^-7

#######################################
#parameters needing updates
phi_data <- initial_phi_data
q <- initial_q
rt <- as.numeric(netdata$r)
ft <- initial_f
r_eta <- initial_r_eta
etat <- netdata$eta
r_mu <- initial_r_mu
r_sigma <- initial_r_sigma
delta <- initial_delta
rho <- initial_rho
gama <- initial_gama
weights = initial_weights

######################

#records
records_r <- c()
records_q <- c()
records_error <- c()
records_etat <- c()
records_weights <- c()
records_ft <- c()
records_dlq <<- c()
records_dlw <<- c()
##########
#initial data, ft, rt, etat
kenScore <- c()
iteration = 15
############

tick1=Sys.time()
for (z in 1:iteration){
    tick=Sys.time()
    prob_pd <<- c()
    rt_plusOne <- Estep(netGeo,ft,rt,r_mu,etat,r_eta,borough)
    #update rt
    rt <- rt_plusOne
    phi_data$r <- rt_plusOne
    print(paste("rt in step ", z, sep = ""))
    print(table(rt))
    print("finish E step, update rt")
    ##update r value to the dataset
    #netdata$r = rt_plusOne
    #update delta
    ######################################
    ####### Done with E step !!!! ########
    ######################################
    
    #####################################
    #######    M step   #################
    ######################################
    ###update delta ####
    delta <- apply(phi_data,1, FUN=function(x)Cal_delta(estate=x,dataset = phi_data))
    print(paste("delta in step ", z, sep = ""))
    print(summary(delta))
    print("I am in M step, just finish delta part")
    #####update center of each business area r  ####
    #for each r
    for (i in 1:k){
        if(sum(rt_plusOne == i) > 0){
            #mu_r = mean of all li in area r
            r_mu[i,1:2] <- apply(netGeo[rt_plusOne == i,],2,mean)
            sub_set <- netGeo[rt_plusOne == i,]
            r_sigma[[i]] <- cov(sub_set)
        }
    }
    print("r_mu updated")
    #update sigma  ####
    #r_sigma = lapply(1:k,FUN=function(x)Cal_sigma(r = x,geoData = netGeo,rt_list = rt_plusOne))
    print("NA in r_sigma: ")
    print(sum(is.na(r_sigma)))
    #r_eta = (sum of (y_i in r) + 1/K)/(sum of y_i + 1)  ######
    r_eta[,1] = sapply(1:k,FUN=function(x)Cal_eta(x,returnData = y, rt_list = rt_plusOne))
    for (i in 1:k){
        etat[rt_plusOne == i] = r_eta[i]
    }
    print(paste("etat in step ", z, sep = ""))
    print(summary(etat))
    #######  update rho   ########
    rho <- apply(netGeo,1,FUN=function(x) Cal_rho(x, centers = r_mu[,1:2], etas = r_eta[,1], d0 = 3000))
    if(sum(is.na(rho))>0){
        print("rho has NA values!!!")
        break
    }
    print(paste("rho in step ", z, sep = ""))
    print(summary(rho))
    print("I am above div q and W")
    ##### update q and W ####
    #gradiant descent function
    combined_output <- gradient_descent(ini_q = q,ini_W = weights,ini_f = ft,ini_r = rt_plusOne,y = netdata$annual_return,stdy = std_y,stdw = std_w,stdq = std_q,step_size = ss)
    q <- combined_output[,dim(combined_output)[2]]
    weights <- combined_output[,1:(dim(combined_output)[2]-1)]
    
    print(paste("weights in step ", z, sep = ""))
    print(weights)
    ##################
    ##update fi ######
    ##################
    # fi = gama_i+rho_i+delta_i
    ############
    gama =  t(q %*% weights %*% t(pd)) + assessedReturn
    #gama = gama + adjusted return rate
    ft = gama + delta + rho
    print(paste("ft in step ", z, sep = ""))
    print(summary(ft))
    #netdata$f = ft
    #### repeat the process


    records_error <- rbind(records_error,sum((y-ft)^2)/length(y))
    records_r <- cbind(records_r,rt)
    records_q <- rbind(records_q,q)
    records_etat <- cbind(records_etat,etat)
    records_weights <- rbind(records_weights,weights)
    records_ft <- cbind(records_ft,ft)
    kenScore <- c(kenScore,kendall_test(y,ft))
    print("Kendall Tau Test")
    print(kenScore)
    tock = Sys.time()
    print(paste("step ", z,sep=""))
    print(tock - tick)
} # z for loop
tock1 = Sys.time()
tock1 - tick1

#illustrate Kendall's Tau Coefficient of each iteration
ken <- data.frame(iteration = 1:30, coefficient = kenScore)
qplot(x = iteration, y = kenScore, data = ken, geom = "line",  ylab="coefficient",main= "Kendall's Tau Coefficient")
#predicted property value f and latent business area
final_ft = ft #records_ft[,59]
final_rt = rt #records_r[,59]
#parameters
final_q = q  #records_q[59,]
final_weights = weights #records_weights[175:177,]
final_eta = etat #records_etat[,59]
#predicted prosperity weights
final_r_eta <- r_eta
results <- cbind(train[,c("Longitude","Latitude","SALE.DATE")],final_ft,final_rt)
names(results) <- c("Longitude","Latitude","SALE.DATE","annual_return","r")
results <- results %>%
    arrange(SALE.DATE)
map <- get_map(location='New York City', zoom = 10,color = "bw",maptype = 'roadmap')
prediction_visualize(results)
netdataVis <- train %>%
    arrange(SALE.DATE)
groundTruth_visualize(netdataVis)
write.csv(results,file="PredictedResults2.csv",row.names = F)
