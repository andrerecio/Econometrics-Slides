
#' Bernoulli sampling function
#' 
#' n is the number of draws taken for each sample mean
bernoulli_sampler <- function(n,p){
    # we perform m = 500 trails each time
    m = 5000
    x = replicate(m,
            mean(sample(0:1, size = n, replace = TRUE, prob = c(1-p, p)))
    )
    hist(x, xlim = c(0,1) )
    
}