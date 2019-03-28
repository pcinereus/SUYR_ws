
loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))
newdata = emmeans(loyn.rstanarmG, ~AREA|GRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)
ggplot(newdata, aes(y=response, x=AREA)) +
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=GRAZE), alpha=0.3) +
geom_line(aes(color=GRAZE)) +
theme_bw() +
scale_x_log10() + scale_y_log10()

library(tidybayes)
spaghetti = emmeans(loyn.rstanarmG, ~AREA|GRAZE, at = loyn.list, type='response') %>%
gather_emmeans_draws() %>% mutate(Fit=exp(.value))
wch = sample(1:max(spaghetti$.draw), 500,replace=FALSE)
spaghetti = spaghetti %>% filter(.draw %in% wch)
ggplot(newdata) +
geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=GRAZE,
                              group=interaction(GRAZE,.draw)), alpha=0.05) +
geom_line(aes(y=response, x=AREA, color=GRAZE)) +
theme_bw() +
scale_x_log10() + scale_y_log10()

summary(loyn.rstanarmG)
