terms <- attr(abalone.gbm$Terms,"term.labels")
abalone.int <- NULL
for (i in 1:(length(terms)-1)) {
  for (j in (i+1):length(terms)) {
    abalone.int <- rbind(abalone.int,
                         data.frame(Var1=terms[i], Var2=terms[j],
                                    "H.stat"=interact.gbm(abalone.gbm, abalone,c(i,j),
                                                          n.tree=best.iter)
                         ))
  }
}
abalone.int %>% arrange(-H.stat)


## Standardized Residuals for 
coefs = as.matrix(peake.glmP)[,1:2]
coefs = as.matrix(peake.glmP)[,c('(Intercept)','log(AREA)')]
Xmat = model.matrix(~log(AREA), data=peake)
fit = exp(coefs %*% t(Xmat))
#Pearson residuals
resid = sweep(fit, 2, peake$INDIV, "-")
# operate on rows
sd = apply(fit,2,sqrt)
resid=resid/sd
ggplot(data=NULL) +
  geom_point(aes(y=colMeans(resid), x=fitted(peake.glmP)))