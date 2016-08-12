#doy=predict(gamDEFAULT,data,exclude=terms[-1])-intercept
#date_dec=predict(gamDEFAULT,data,exclude=terms[-2])-intercept
#temp=predict(gamDEFAULT,data,exclude=terms[-3])-intercept

## this doesnt't work
# test=as.data.frame(cbind(mean(data$doy),mean(data$date_dec),mean(data$sio2),data$temp))
# names(test)=c("doy","date_dec","sio2","temp")
# temp=predict(gamDEFAULT,test)-intercept

#sio2=predict(gamDEFAULT,data,exclude=terms[-4])-intercept
#all=predict(gamDEFAULT,data) ## on log scale
#all4=exp(all)
#all2=predict(gamDEFAULT,data,type="response")
## all4 and all2 don't match?!

##all2 and all3 match

# plot(density(tryThis2-(intercept+doy+date_dec+temp+sio2)))
# range(tryThis2-(intercept+doy+date_dec+temp+sio2))
# exp(-3.020162e-10) ## off by close to zero --> transformed to 1
# exp(1.637090e-10)
# 
# summary(exp(tryThis2))
# summary(exp(intercept+doy+date_dec+temp+sio2)) ## this matches
# summary(gamDEFAULT$fitted.values)
# summary(data$chl) ## need an offset? detection limit?
# 
# summary(log(data$chl))
# summary(intercept+doy+date_dec+temp+sio2)
# summary(log(gamDEFAULT$fitted.values)) ## differ in min
# summary(tryThis2) ## differ in min
# plot(density(log(gamDEFAULT$fitted.values)))
# plot(density(tryThis2))
# 
# summary(exp(tryThis2))
# summary(gamDEFAULT$fitted.values)
# plot(density(exp(tryThis2)))
# plot(density(gamDEFAULT$fitted.values))
# quantile(exp(tryThis2),0.95)
# quantile(gamDEFAULT$fitted.values,0.95)
# 
# summary(exp(intercept+doy+date_dec+temp+sio2))
# plot(density(exp(intercept+doy+date_dec+temp+sio2)))
# 
# quantile(exp(intercept+doy+date_dec+temp+sio2),0.95)
# 
# ## trim those weird ones off to avoid this issue