Estep <- function(netGeo,ft,rt, r_mu,etat,r_eta,borough){
    print("I am in E step")
    backup_rt <- rt
    for (zone in c(1,2,3,5)){
        netGeo1 <- netGeo[borough == zone,]
        ft1 <- ft[borough==zone]
        rt1 <- rt[borough==zone]
        etat1 <- etat[borough==zone]
        r_mu1 <- r_mu[r_mu[,3] == zone,1:2]
        r_eta1 <- r_eta[r_eta[,2]==zone,1]
        ranges = unique(rt1)
        ranges = sort(ranges)
        start = min(ranges)
        last = max(ranges)
        k = length(ranges)
        print("k")
        print(k)
        r_sigma1 <- lapply(start:last,FUN=function(x)Cal_sigma(x,geoData = netGeo1, rt_list = rt1))
        end = length(ft1)
        rt_plusOne <- c()
        for (i in 1:(end-1)){
            prob_id <- c()
            prob_pd <- c()
            prob_zd <- c()
            p_etat <- c()
            rest_part <- rt1[i:end]
            if (sum(rt1[(i+1):end] == rt1[i]) > 2){
                for (j in 1:k){
                    prob_id_j <- dmnorm(netGeo1[i,],mean = r_mu1[j,],r_sigma1[[j]])
                    if (!is.infinite(log(prob_id_j))){
                        prob_id <- c(prob_id,log(prob_id_j))
                    }
                    else{
                        prob_id <- c(prob_id,-10^10)
                    }
                    if (length(prob_id)>100){
                        print("zone")
                        print(zone)
                        print("netGeo1")
                        print(netGeo1[i,])
                        print("r_mu1")
                        print(r_mu1[j])
                        print(r_sigma1[[j]])
                        print("j")
                        print(j)
                        print("i")
                        print(i)
                    }
                #P(i --> h)
                #subdata <- netdata[(i+1):dim(netdata)[1],]
                    ft_sub <- ft1[rt1[(i+1):min((i+10),(end-i))] == ranges[j]]#subdata[subdata$r==j,]$phi
                #l_ft = length(ft_sub)
                #print(length(ft_sub))
                    if (length(ft_sub) != 0) {
                        prob_pd_j <- sum(log(1/(1+exp(ft_sub - ft1[i])))) # it should be saved
                        prob_pd <- c(prob_pd,prob_pd_j)
                    }else{
                        prob_pd <- c(prob_pd,0)
                        #print("I am in prob_pd when ft_sub is length 0")
                    }
                    etat_sub <- etat1[rt1[(i+1):min((i+10),(end-i))] != j]   #subdata[subdata$r !=j,]$eta
                    etat_i <- r_eta1[j]
                    #l_eta <-  length(etat_sub)
                    if(length(etat_sub) !=0){
                        prob_zd_not_j <-  sum(log(1/(1+exp(-(etat_i - etat_sub)))))
                        prob_zd <- c(prob_zd, prob_zd_not_j)
                    }else{
                        prob_zd <- c(prob_zd, 0)
                    }
                
                # if(length(etat_sub) != 0){
                #     prob_zd_not_j <- sum(log(1/(1+exp(-(etat[i]-etat_sub))))) # save
                #     prob_zd <- c(prob_zd, prob_zd_not_j)
                # }else{
                #     prob_zd <- c(prob_zd, 0)
                #     #print("I am in prob_zd when etat_sub is length 0")
                # }
                
                    if (r_eta1[j] > 0){
                        eta_i <- log(r_eta1[j]/sum(r_eta[,1]))
                        p_etat <- c(p_etat, eta_i)
                    }else{
                        p_etat <- c(p_etat,-10^10)
                    }
                
                }#end for loop
                if (sum(is.nan(prob_id))>0){
                    print("prob_id has NaN values!!!")
                }
                if (sum(is.nan(prob_pd))>0){
                    print("prob_pd has NaN values!!!")
                }
                if(sum(is.nan(prob_zd))>0){
                    print("prob_zd has NaN values!!!")
                }
                if(sum(is.nan(p_etat))>0){
                    print("p_etat has NaN values!!!")
                }
                #P(y_i|f_i, delta^2)
                p_r <- prob_id + prob_pd + p_etat + prob_zd
                r_i <- which(p_r == max(p_r))
                r_i = ranges[r_i]
                rt_plusOne <- c(rt_plusOne,r_i)
                if (length(r_i) > 1){
                    print("prob_id length")
                    print(length(prob_id))
                    print("prob_pd length")
                    print(length(prob_pd))
                    break
                }

                if (sum(is.na(rt_plusOne))>0){
                    print("rt_plusOne has NA values!!!")
                    break
                }
            }else{
                rt_plusOne <- c(rt_plusOne,rt1[i])
                }
            }
        
            rt_plusOne <- c(rt_plusOne,rt1[end])
            print("rt_plusOne length")
            print(length(rt_plusOne))
            backup_rt[borough==zone] = rt_plusOne
            print("backup rt length")
            print(length(backup_rt[borough==zone]))

    }
        return(backup_rt)
}
