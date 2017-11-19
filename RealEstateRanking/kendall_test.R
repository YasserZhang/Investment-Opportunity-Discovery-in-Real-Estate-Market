kendall_test <- function(y,f){
    concordant = 0
    discordant = 0
    for (i in 1:(length(y)-1)){
        for (j in (i+1):length(y)){
            if (y[i] >y[j] & f[i] > f[j]){
                concordant = concordant + 1
            } else if (y[i] == y[j] & f[i] == f[j]){
                concordant = concordant + 1
            }
            else{
                discordant = discordant + 1
            }
        }
    }
    tau = (concordant - discordant)/(concordant+discordant)
    return(tau)
}
