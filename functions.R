ACF.merMod <- function(object, maxLag, resType = c("pearson", "response",
    "deviance", "raw"), scaled = TRUE, re = names(object@flist[1]),
    ...) {
    resType <- match.arg(resType)
    res <- resid(object, type = resType, scaled = TRUE)
    res = split(res, object@flist[[re]])
    if (missing(maxLag)) {
        maxLag <- min(c(maxL <- max(lengths(res)) - 1, as.integer(10 *
            log10(maxL + 1))))
    }
    val <- lapply(res, function(el, maxLag) {
        N <- maxLag + 1L
        tt <- double(N)
        nn <- integer(N)
        N <- min(c(N, n <- length(el)))
        nn[1:N] <- n + 1L - 1:N
        for (i in 1:N) {
            tt[i] <- sum(el[1:(n - i + 1)] * el[i:n])
        }
        array(c(tt, nn), c(length(tt), 2))
    }, maxLag = maxLag)
    val0 <- rowSums(sapply(val, function(x) x[, 2]))
    val1 <- rowSums(sapply(val, function(x) x[, 1]))/val0
    val2 <- val1/val1[1L]
    z <- data.frame(lag = 0:maxLag, ACF = val2)
    attr(z, "n.used") <- val0
    class(z) <- c("ACF", "data.frame")
    z
}


emm_basis.gam = function(object, trms, xlev, grid, nboot = 800, ...) {
    if (!is.null(object$gcv.ubre)) # From mgcv, not gam
        return (emm_basis.gam_mgcv(object, trms, xlev, grid, ...))
    
    result = emm_basis.lm(object, trms, xlev, grid, ...)
    old.smooth = object$smooth
    if (is.null(old.smooth))  # "just an ordinary glm" (My Fair Lady)
        return(result)
    # else we need to add-in some smoothers
    smooth.frame = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
    data = object$smooth.frame
    labs = names(data)
    w = object$weights
    resid = object$residuals
    for (i in seq_along(labs)) {
        lab = labs[i]
        sig = apply(smooth.frame[[i]], 1, paste, collapse = ":")
        usig = unique(sig)
        rows = lapply(usig, function(s) which(sig == s))
        xeval = smooth.frame[sapply(rows, "[", 1), lab]
        bsel = matrix(0, nrow = length(sig), ncol = length(usig))
        for (j in seq_along(rows))
            bsel[rows[[j]], j] = 1
        
        cl = attr(data[[i]], "call")
        cl$xeval = substitute(xeval)
        z = resid + old.smooth[, lab]
        bh = as.numeric(eval(cl))
        m = length(bh)
        n = length(result$bhat)
        result$bhat = c(result$bhat, bh)
        result$X = cbind(result$X, bsel)
        boot = replicate(nboot, {
                z = sample(resid, replace = TRUE) + old.smooth[, lab]
                as.numeric(eval(cl))
            })
        covar = if(m == 1) var(boot) 
                else       cov(t(boot))
        result$V = rbind(cbind(result$V, matrix(0, nrow = n, ncol = m)),
                         cbind(matrix(0, nrow = m, ncol = n), covar))
    }
    result
}


mcmcpvalue <- function(samp)
{
    ## elementary version that creates an empirical p-value for the
    ## hypothesis that the columns of samp have mean zero versus a
    ## general multivariate distribution with elliptical contours.
    
    ## differences from the mean standardized by the observed
    ## variance-covariance factor
    
    ## Note, I put in the bit for single terms
    if (length(dim(samp))==0) {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - mean(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/length(samp)
    }
    else {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - colMeans(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/nrow(samp)
    }
    
}
