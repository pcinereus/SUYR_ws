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
