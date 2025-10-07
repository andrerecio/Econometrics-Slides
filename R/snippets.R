
#' Bernoulli sampling function
#' 
#' n is the number of draws taken for each sample mean
bernoulli_sampler <- function(n,p){
    # we perform m = 5000 trials each time
    m = 5000
    x = replicate(m,
            mean(sample(0:1, size = n, replace = TRUE, prob = c(1-p, p)))
    )
    
    range_x <- c(0, 1)
    # Improved binning
    if (n <= 5) {
        n_bins <- 10      # for small n, one bin per possible mean
    } else if (n <= 50) {
        n_bins <- 25          # medium n
    } else {
        n_bins <- 100          # large n, smooth curve
    }    
    # Compute histogram over [0, 1]
    h <- hist(
        x,
        breaks = seq(range_x[1], range_x[2], length.out = n_bins + 1),
        plot = FALSE
    )
    
    # Convert counts to probabilities
    h$prob <- h$counts / sum(h$counts)
    
    bp <- barplot(
        h$prob,
        names.arg = FALSE,
        xlab = "x",
        ylab = "Probability",
        main = paste("Sampling Dist with n =", n),
        col = "red",
        border = "black",
        ylim = c(0, max(h$prob) * 1.1),
        space = 0,      # no gaps between bars
        axes = FALSE    # suppress all default axes
    )
    
    # Add custom axes
    axis(1, 
         at = seq(0, 1, by = 0.2) * max(bp),   # scaled tick positions
         labels = seq(0, 1, by = 0.2))
    # Add your custom tick
    axis(1,
         at = p * max(bp),   # scale position
         labels = p,
         tick = TRUE,
         col.ticks = "red",
         col.axis = "red",
         cex.axis = 0.9)
    axis(2)
}

save_bernoullis <- function(n_values,p){
    png("img/bernoulli_grid.png", width = 1600, height = 900, res = 150)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
    for (n in n_values) bernoulli_sampler(n, p)
    # mtext(paste("Sampling Distributions of Bernoulli(p =", p, ") Means"),
          # outer = TRUE, cex = 1.4, line = 0.5)
    dev.off()
}

