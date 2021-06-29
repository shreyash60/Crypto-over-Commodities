library(PortfolioAnalytics)
library(RiskPortfolios)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(readxl)
library(fPortfolio)
library(rmgarch)
library(devtools)
#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)

#### EDA ####
library(readxl)
data.b <- read_excel("C:/Users/Shreya Nayak/Desktop/SC 3/SEMESTER-2-PRJCT/G&B_Portfolio.xlsx", 
                     sheet = "Bitcoin_Portfolio")
#View(data.b)
str(data.b)
class(data.b)

par(mfrow=c(2,3))

sbi.ts=ts(data.b$`SBI Close`)
sbi_returns = ts(data.b$`SBI Returns`)
class(sbi.ts)
plot(sbi.ts)

mo.ts=ts(data.b$`MO NASDAQ Close`)
mo_returns = ts(data.b$`MO Returns`)
class(mo.ts)
plot(mo.ts)

nif.ts=ts(data.b$`NIFTY 50 Close`)
nif_returns = ts(data.b$`NIFTY Returns`)
class(nif.ts)
plot(nif.ts)

S.P.ts=ts(data.b$`S&P 500 ETF Close`)
S.P_returns = ts(data.b$`S&P Returns`) 
class(S.P.ts)
plot(S.P.ts)

btc.ts=ts(data.b$`BTC Closing Price (USD)`)
btc.returns=ts(data.b$`BTC Returns`)
class(btc.ts)
plot(btc.ts)
plot(btc.returns)


summary(data.b[,7:11])

#summary statistics of return percent
library(pastecs)
stat.desc(data.b[,7:11]*100)

# return do not follow normality
# they have fat tails
qqnorm(data.b$`SBI Returns`, pch = 1, frame = FALSE)
qqline(data.b$`SBI Returns`, col = "steelblue", lwd = 2)

qqnorm(data.b$`MO Returns`, pch = 1, frame = FALSE)
qqline(data.b$`MO Returns`, col = "steelblue", lwd = 2)

qqnorm(data.b$`NIFTY Returns`, pch = 1, frame = FALSE)
qqline(data.b$`NIFTY Returns`, col = "steelblue", lwd = 2)

qqnorm(data.b$`S&P Returns`, pch = 1, frame = FALSE)
qqline(data.b$`S&P Returns`, col = "steelblue", lwd = 2)

qqnorm(data.b$`BTC Returns`, pch = 1, frame = FALSE)
qqline(data.b$`BTC Returns`, col = "steelblue", lwd = 2)


colSums(is.na(data.b))
library(tseries)

# return series is stationary
adf.test(sbi_returns[2:1824])
kpss.test(sbi_returns)

adf.test(mo_returns[2:1824])
kpss.test(mo_returns)

adf.test(nif_returns[2:1824]) 
kpss.test(nif_returns) 

adf.test(S.P_returns[2:1824])
kpss.test(S.P_returns)

adf.test(btc.returns[-1])
kpss.test(btc.returns)

#### Data Preparation ####

#Input Data

data <- read_excel("C:/Users/Shreya Nayak/Desktop/SC 3/SEMESTER-2-PRJCT/G&B_Portfolio.xlsx", 
                   sheet = "Bitcoin_Portfolio")
data<-data[,1:6]
#View(data)

#Convert to a timeSeries
data$Date<-as.Date(data$Date)
data1<-as.timeSeries(data)
data.ts<-xts(data[,2:6],data$Date)
class(data1)
class(data.ts)

data<-as.timeSeries(data.ts)
class(data)
#Added holiday data
sort(data)
rev(sort(data))
sort(data, decreasing = TRUE)

nrow(data)
aligned <- align(x = data, by = "1d", method = "before", include.weekends = FALSE)
nrow(aligned)

colSums(is.na(aligned))
data<-aligned

summary(data)

#Calculate Returns

data.r<-Return.calculate(data)

head(data.r)

basicStats(data.r[, 1:5])
summary(data.r)

colSums(is.na(data.r))
head(data.r)
data.r<-na.omit(data.r)
#View(data.r)

data.ret<- 100*data.r[,1:5]

data.ret.1<-as.data.frame(data.ret)
# View(data.ret.1)

#### Covariance Matrices ####

#Normal
b.data.ret<-as.matrix(data.ret)

mu.ret.1<-meanEstimation(b.data.ret)

cov.1<-covEstimation(b.data.ret)

#### DCC ####

uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

spec1 = dccspec(uspec = multispec( replicate(5, uspec) ),  dccOrder = c(1,1),  distribution = "mvnorm")

fit1 = dccfit(spec1, data = b.data.ret, fit.control = list(eval.se=T))

cov.dcc.b = rcov(fit1)
cov.dcc.b<-as.matrix(cov.dcc.b[,,dim(cov.dcc.b)[3]])

w.dcc.1<-as.data.frame(optimalPortfolio(mu = mu.ret.1, Sigma = cov.dcc.b
                                        ,control = list(type = 'mv')))
w.dcc.1.l.b = as.data.frame(optimalPortfolio(mu = mu.ret.1, Sigma = cov.dcc.b, control = list(constraint = 'lo')))
# View(w.dcc.1)
# View(w.dcc.1.l)
w.dcc.1

p.dcc1s<- globalMin.portfolio(as.vector(mu.ret.1), cov.dcc.b, shorts = TRUE)
p.dcc1l<- globalMin.portfolio(as.vector(mu.ret.1), cov.dcc.b, shorts = FALSE)
p.dcc1s
p.dcc1l

#### ADCC ####

uspec2 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

spec2 = dccspec(uspec = multispec( replicate(5, uspec2) ),  dccOrder = c(1,1),  distribution = "mvnorm", model = 'aDCC')

fit2 = dccfit(spec2, data = b.data.ret, fit.control = list(eval.se=T))

#adcc.forecast=dccforecast(fit2, n.ahead = 1, n.roll = 0)

cov.adcc.b = rcov(fit2)
cov.adcc.b<-as.matrix(cov.adcc.b[,,dim(cov.adcc.b)[3]])

w.adcc.1<-as.data.frame(optimalPortfolio(mu = mu.ret.1, Sigma = cov.adcc.b))
w.adcc.1.l = as.data.frame(optimalPortfolio(mu = mu.ret.1, Sigma = cov.adcc.b, control = list(constraint = 'lo')))

p.adcc1s<- globalMin.portfolio(as.vector(mu.ret.1), cov.adcc.b, shorts = TRUE)
p.adcc1l<- globalMin.portfolio(as.vector(mu.ret.1), cov.adcc.b, shorts = FALSE)

w.adcc.1
w.adcc.1.l.b
p.adcc1s
p.adcc1l


#### GO GARCH ####

uspec3 = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

spec3 = gogarchspec(mean.model = list(model = c("constant", "AR", "VAR"), robust = FALSE,
                                      lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"),
                                      external.regressors = NULL,
                                      robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL,
                                          variance.targeting = FALSE), distribution.model = c("mvnorm", "manig", "magh"),
                    ica = c("fastica", "radical"),
                    ica.fix = list(A = NULL, K = NULL))

fit3 = gogarchfit(spec = spec3, data = b.data.ret,
                  out.sample = 50, gfun = "tanh")


cov.go.b = rcov(fit3)
cov.go.b<-as.matrix(cov.go.b[,,dim(cov.go.b)[3]])

w.go.1<-as.data.frame(optimalPortfolio(mu = mu.ret.1, Sigma = cov.go.b))
w.go.1.l.b = as.data.frame(optimalPortfolio(mu = mu.ret.1, Sigma = cov.go.b, control = list(constraint = 'lo')))

p.go1s<- globalMin.portfolio(as.vector(mu.ret.1), cov.go.b, shorts = TRUE)
p.go1l<- globalMin.portfolio(as.vector(mu.ret.1), cov.go.b, shorts = FALSE)

w.go.1
p.go1s
p.go1l

b.weights<-do.call("cbind", list(as.data.frame(w.dcc.1),
                                 as.data.frame(w.adcc.1),
                                 as.data.frame(w.go.1)))
b.weights
b.weights.l<-do.call("cbind", list(as.data.frame(w.dcc.1.l),as.data.frame(w.adcc.1.l),as.data.frame(w.go.1.l)))
b.weights.l
                     
write.csv(mu.ret.1,file.choose())

#### Sharpe Ratio ####

results.dcc.b <- Return.portfolio(data.ret,weights =c(0.4357550,0.2189356,-0.3587854,0.6351690,0.0689259),
                            rebalance_on="days",verbose=T,geometric = F)
results.adcc.b <- Return.portfolio(data.ret,weights =c(0.40106173,0.20140538,-0.30442899,0.63445930,0.06750259),
                            rebalance_on="days",verbose=T,geometric = F)
results.go.b <- Return.portfolio(data.ret,weights =c(0.93008998,0.15500045,-0.35843295,0.23527326,0.03806925),
                            rebalance_on="days",verbose=T,geometric = F)

s1.b = SharpeRatio(results.dcc.b$returns)
s2.b = SharpeRatio(results.adcc.b$returns)
s3.b = SharpeRatio(results.go.b$returns)
# if sharpe ratio is more, its better 


#### Mean and Std of portfolio ####

# expected returns of bitcoin portfolio GMV
w.dcc.b = weighted.mean(x = c(colMeans(data.ret)), w = c(0.4357550,0.2189356,-0.3587854,0.6351690,0.0689259))
w.adcc.b = weighted.mean(x = c(colMeans(data.ret)), w = c(0.40106173,0.20140538,-0.30442899,0.63445930,0.06750259))
w.go.b = weighted.mean(x = c(colMeans(data.ret)), w = c(0.93008998,0.15500045,-0.35843295,0.23527326,0.03806925))

r1.b = as.numeric(results.dcc.b$returns)
r2.b = as.numeric(results.adcc.b$returns)
r3.b = as.numeric(results.go.b$returns)

#The standard deviation shows the volatility of an investment's returns relative to its average return, 
#with greater standard deviations reflecting wider returns, 
#and narrower standard deviations implying more concentrated returns. T



#### Turnover ####

bop.dcc.b <- results.dcc.b$BOP.Weight #beginning of period weights
eop.dcc.b <- results.dcc.b$EOP.Weight #end of period weights

bop.adcc.b <- results.adcc.b$BOP.Weight #beginning of period weights
eop.adcc.b <- results.adcc.b$EOP.Weight #end of period weights

bop.go.b <- results.go.b$BOP.Weight #beginning of period weights
eop.go.b <- results.go.b$EOP.Weight #end of period weights

f1.b<-abs(bop.dcc.b-eop.dcc.b)
f2.b<-abs(bop.adcc.b-eop.adcc.b)
f3.b<-abs(bop.go.b-eop.go.b)

## DCC 
YourTurnover.dcc.b=sum(f1.b)*(1/(nrow(data.ret)-1))
#SanityCheck.dcc.b=sum(abs(eop.dcc.b-1/ncol(data.ret)))/(nrow(data.ret)-1)

turnover.year.dcc.b = YourTurnover.dcc.b*252
turnover.year.dcc.b

tradingcost.dcc.5.b = turnover.year.dcc.b*10
trad.dcc.5.b = tradingcost.dcc.5.b/10000
trad.dcc.5.b

# tradingcost.dcc.10.b = turnover.year.dcc.b*350
# trad.dcc.10.b = tradingcost.dcc.10.b/10000
# trad.dcc.10.b

tradingcost.dcc.20.b = turnover.year.dcc.b*20
trad.dcc.20.b = tradingcost.dcc.20.b/10000
trad.dcc.20.b

## ADCC
YourTurnover.adcc.b=sum(f2.b)*(1/(nrow(data.ret)-1))
#SanityCheck.adcc.b=sum(abs(eop.adcc.b-1/ncol(data.ret.b)))/(nrow(data.ret.b)-1)

turnover.year.adcc.b = YourTurnover.adcc.b*252
turnover.year.adcc.b

tradingcost.adcc.5.b = turnover.year.adcc.b*10
trad.adcc.5.b = tradingcost.adcc.5.b/10000
trad.adcc.5.b

# tradingcost.adcc.10.b = turnover.year.adcc.b*350
# trad.adcc.10.b = tradingcost.adcc.10.b/10000
# trad.adcc.10.b

tradingcost.adcc.20.b = turnover.year.adcc.b*20
trad.adcc.20.b = tradingcost.adcc.20.b/10000
trad.adcc.20.b

## GOGARCH
YourTurnover.go.b=sum(f3.b)*(1/(nrow(data.ret)-1))
#SanityCheck.go.b=sum(abs(eop.go.b-1/ncol(data.ret.b)))/(nrow(data.ret.b)-1)

turnover.year.go.b = YourTurnover.go.b*252
turnover.year.go.b

tradingcost.go.5.b = turnover.year.go.b*10
trad.go.5.b = tradingcost.go.5.b/10000
trad.go.5.b

# tradingcost.go.10.b = turnover.year.go.b*350
# trad.go.10.b = tradingcost.go.10.b/10000
# trad.go.10.b

tradingcost.go.20.b = turnover.year.go.b*20
trad.go.20.b = tradingcost.go.20.b/10000
trad.go.20.b

