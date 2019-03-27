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