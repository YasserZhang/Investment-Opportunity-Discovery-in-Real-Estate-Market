gradient_descent <- function(ini_q,ini_W,ini_f,ini_r,y,stdy,stdw,stdq,step_size){
    q = ini_q
    weights = ini_W
    ft = ini_f
    rt_plusOne = ini_r
    std_y = stdy
    std_w = stdw
    std_q = stdq
    for (n in 1:1){
        dLq <- c()
        dLw <- matrix(rep(0,dim(weights)[1]*dim(weights)[2]), nrow = 3,byrow = T)
        dLqm_part2 <- sum(mu_q - q)/std_q^2
        dLwm_part2 <- apply(weights, 2, FUN=function(x){sum((mu_w - x))/std_w^2})
        dLwm_part2 <- matrix(rep(dLwm_part2,length(q)),nrow = length(q), byrow = T)
        for (m in 1:length(q)){
            dLqm_part3 =0
            dLwm_part3 =rep(0,dim(weights)[2])
            product =  pd %*% weights[m,]
            dLqm_part1 <- sum(product*(y - ft))/std_y^2
            #print("problem1")
            #dW_mn
            dLwm_part1 <- apply(pd,2,FUN = function(x){t(y - ft) %*% x})
            dLwm_part1 <- dLwm_part1*q[m]/std_y^2
            for (i in 1:(end-1)){
                f_i = ft[i]
                r_i = rt_plusOne[i]
                #P(i --> h)
                sub_f <- ft[(i+1):end]
                sub_rt_plusOne <- rt_plusOne[(i+1):end]
                sub_pd <- pd[(i+1):end,]
                if (sum(sub_rt_plusOne == r_i)>1 ) {
                    #f_h
                    f_h <- sub_f[sub_rt_plusOne == r_i]
                    #e_h
                    e_h <- sub_pd[sub_rt_plusOne == r_i,]
                    deduction <- apply(e_h,1,FUN = function(x){pd[i,] - x})
                    #print("problem2")
                    mm <- weights[m,] %*% deduction
                    i_h <- exp(f_h - f_i)/(1 + exp(f_h - f_i))
                    result_q <- sum(mm*i_h)
                    dLqm_part3 = dLqm_part3 + result_q
                    deduction = t(deduction)
                    result_Wm <- apply(deduction,2,FUN=function(x){x*i_h})
                    result_Wm <- result_Wm*q[m]
                    #sum by column
                    result_Wm <- apply(result_Wm,2,sum)
                    dLwm_part3 = dLwm_part3 + result_Wm
                }
                else if (sum(sub_rt_plusOne == r_i) == 1) {               #outer if
                    f_h <- sub_f[sub_rt_plusOne == r_i]
                    #sub_pd <- pd[(i+1):end,]
                    e_h <- sub_pd[sub_rt_plusOne == r_i]
                    deduction <- pd[i,] - e_h #apply(e_h,1,FUN = function(x){pd[i,] - x})
                    #print("problem2")
                    mm <- weights[m,] %*% deduction  
                    i_h <- exp(f_h - f_i)/(1 + exp(f_h - f_i))
                    result_q <- sum(mm*i_h)
                    dLqm_part3 = dLqm_part3 + result_q
                    result_Wm <- deduction * i_h 
                    result_Wm <- result_Wm*q[m]
                    dLwm_part3 = dLwm_part3 + result_Wm
                }
            }
            dLq <- c(dLq, dLqm_part1 + dLqm_part3)
            dLw[m,] = dLw[m,] + dLwm_part3 + dLwm_part1
        } #for
        dLw = dLw + dLwm_part2
        dLq = dLq + dLqm_part2
        print(dLq)
        records_dlq <<- rbind(records_dlq,dLq)
        records_dlw <<- rbind(records_dlw,dLw)

        # update q and W
        ######
        # q = q + alpha*div_q
        # W = W + alpha*W_div
        ####
        q = q + step_size*dLq
        weights = weights + step_size*dLw
        }
    return(cbind(weights,q))
}
