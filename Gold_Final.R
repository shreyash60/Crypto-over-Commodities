
library(PortfolioAnalytics)
library(RiskPortfolios)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(readxl)
library(fPortfolio)
library(rmgarch)
library(devtools)
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)

#Input Data
library(readxl)
data.g <- read_excel("C:/Users/Shreya Nayak/Desktop/SC 3/SEMESTER-2-PRJCT/G&B_Portfolio.xlsx", 
                     sheet = "Gold_Portfolio")
View(data.g)
#### EDA ####
str(data.g)
class(data.g)

par(mfrow=c(2,3))

sbi.ts=ts(data.g$`SBI Close`)
sbi_returns = ts(data.g$`SBI Returns`)
class(sbi.ts)
plot(sbi.ts)

mo.ts=ts(data.g$`MO NASDAQ Close`)
mo_returns = ts(data.g$`MO Returns`)
class(mo.ts)
plot(mo.ts)

nif.ts=ts(data.g$`NIFTY 50 Close`)
nif_returns = ts(data.g$`NIFTY Returns`)
class(nif.ts)
plot(nif.ts)

S.P.ts=ts(data.g$`S&P 500 ETF Close`)
S.P_returns = ts(data.g$`S&P Returns`) 
class(S.P.ts)
plot(S.P.ts)

gold.ts=ts(data.g$Gold_Nippon)
gold_returns = ts(data.g$Gold_Returns)
class(gold.ts)
plot(gold.ts)

summary(data.g[,7:11])

#summary statistics of return percent
install.packages("pastecs")
library(pastecs)
stat.desc(data.g[,7:11]*100)

# return do not follow normality
# they have fat tails
qqnorm(data.g$`SBI Returns`, pch = 1, frame = FALSE)
qqline(data.g$`SBI Returns`, col = "steelblue", lwd = 2)

qqnorm(data.g$`MO Returns`, pch = 1, frame = FALSE)
qqline(data.g$`MO Returns`, col = "steelblue", lwd = 2)

qqnorm(data.g$`NIFTY Returns`, pch = 1, frame = FALSE)
qqline(data.g$`NIFTY Returns`, col = "steelblue", lwd = 2)

qqnorm(data.g$`S&P Returns`, pch = 1, frame = FALSE)
qqline(data.g$`S&P Returns`, col = "steelblue", lwd = 2)

qqnorm(data.g$Gold_Returns, pch = 1, frame = FALSE)
qqline(data.g$Gold_Returns, col = "steelblue", lwd = 2)


colSums(is.na(data.g))
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

adf.test(gold_returns[2:1824])
kpss.test(gold_returns)

#### DATA PREPARATION ####
data.g<-data.g[,1:6]

#Convert to a timeSeries
data.g$Date<-as.Date(data.g$Date)

data2<-as.timeSeries(data.g)
data.ts.g<-xts(data.g[,2:6],data.g$Date)
class(data2)
class(data.ts.g)

data.g<-as.timeSeries(data.ts.g)
class(data.g)
#Added holiday data
sort(data.g)
rev(sort(data.g))
sort(data.g, decreasing = TRUE)

nrow(data.g)
aligned.g <- align(x = data.g, by = "1d", method = "before", include.weekends = FALSE)
nrow(aligned.g)


colSums(is.na(aligned.g))
data.g<-aligned.g

summary(data.g)

#Calculate Returns

data.r.g<-Return.calculate(data.g)

head(data.r.g)

basicStats(data.r.g[, 1:5])
summary(data.r.g)

colSums(is.na(data.r.g))

data.r.g<-na.omit(data.r.g)

#Measuring Portfolio Risk
data.ret.g<- 100*data.r.g[,1:5]

data.ret.1.g<-as.data.frame(data.ret.g)

#### Covariance Matrices ####

#Normal

g.data.ret<-as.matrix(data.ret.g)

mu.ret.1.g<-meanEstimation(g.data.ret)

cov.1.g<-covEstimation(g.data.ret)

#### DCC ####

#Specifying the model 5 times

uspec1.g = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

spec1.g = dccspec(uspec = multispec( replicate(5, uspec1.g) ),  dccOrder = c(1,1),  distribution = "mvnorm")

fit1.g = dccfit(spec1.g, data = g.data.ret, fit.control = list(eval.se=T))

cov.dcc.g = rcov(fit1.g)
cov.dcc.g<-as.matrix(cov.dcc.g[,,dim(cov.dcc.g)[3]])

w.dcc.1.g<-as.data.frame(optimalPortfolio(mu = mu.ret.1.g, Sigma = cov.dcc.g))
w.dcc.1.l.g = as.data.frame(optimalPortfolio(mu = mu.ret.1.g, Sigma = cov.dcc.g, control = list(constraint = 'lo')))
p.dcc1s.g<- globalMin.portfolio(as.vector(mu.ret.1.g), cov.dcc.g, shorts = TRUE)
p.dcc1l.g<- globalMin.portfolio(as.vector(mu.ret.1.g), cov.dcc.g, shorts = FALSE)

w.dcc.1.g

#### ADCC ####

uspec2.g = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

spec2.g = dccspec(uspec = multispec( replicate(5, uspec2.g) ),  dccOrder = c(1,1),  distribution = "mvnorm", model = 'aDCC')

fit2.g = dccfit(spec2.g, data = g.data.ret, fit.control = list(eval.se=T))


cov.adcc.g = rcov(fit2.g)
cov.adcc.g<-as.matrix(cov.adcc.g[,,dim(cov.adcc.g)[3]])

w.adcc.1.g<-as.data.frame(optimalPortfolio(mu = mu.ret.1.g, Sigma = cov.adcc.g))
w.adcc.1.l.g = as.data.frame(optimalPortfolio(mu = mu.ret.1.g, Sigma = cov.adcc.g, control = list(constraint = 'lo')))

p.adcc1s.g<- globalMin.portfolio(as.vector(mu.ret.1.g), cov.adcc.g, shorts = TRUE)
p.adcc1l.g<- globalMin.portfolio(as.vector(mu.ret.1.g), cov.adcc.g, shorts = FALSE)
sum(p.adcc1l.g$weights)

#### GO GARCH ####

uspec3.g = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

spec3.g = gogarchspec(mean.model = list(model = c("constant", "AR", "VAR"), robust = FALSE,
                                        lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"),
                                        external.regressors = NULL,
                                        robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL,
                                            variance.targeting = FALSE), distribution.model = c("mvnorm", "manig", "magh"),
                      ica = c("fastica", "radical"),
                      ica.fix = list(A = NULL, K = NULL))

fit3.g = gogarchfit(spec = spec3.g, data = g.data.ret,
                    out.sample = 50, gfun = "tanh")


cov.go.g = rcov(fit3.g)
cov.go.g<-as.matrix(cov.go.g[,,dim(cov.go.g)[3]])

w.go.1.g<-as.data.frame(optimalPortfolio(mu = mu.ret.1.g, Sigma = cov.go.g))
w.go.1.l.g = as.data.frame(optimalPortfolio(mu = mu.ret.1.g, Sigma = cov.go.g, control = list(constraint = 'lo')))

p.go1s.g<- globalMin.portfolio(as.vector(mu.ret.1.g), cov.go.g, shorts = TRUE)
p.go1l.g<- globalMin.portfolio(as.vector(mu.ret.1.g), cov.go.g, shorts = FALSE)

g.weights.g<-do.call("cbind", list(as.data.frame(w.dcc.1.g),as.data.frame(w.adcc.1.g), as.data.frame(w.go.1.g)))
g.weights.l.g<-do.call("cbind", list(as.data.frame(w.dcc.1.l.g),as.data.frame(p.adcc1l.g$weights),as.data.frame(w.go.1.l.g)))
g.weights.g
g.weights.l.g

#### Sharpe Ratio ####
results.dcc.g <- Return.portfolio(data.ret.g ,weights =c(0.3568038,0.1582201,-0.2021080,0.5647936,0.1222906),
                            rebalance_on="days",verbose=T,geometric = F)
results.adcc.g <- Return.portfolio(data.ret.g ,weights =c(0.3818496,0.1626060,-0.2280616,0.5775171,0.1060889),
                                  rebalance_on="days",verbose=T,geometric = F)
results.go.g <- Return.portfolio(data.ret.g ,weights =c(0.8315807,0.1313824,-0.3407777,0.2171263,0.1606883),
                                  rebalance_on="days",verbose=T,geometric = F)

s1.g = SharpeRatio(results.dcc.g$returns)
s2.g = SharpeRatio(results.adcc.g$returns)
s3.g = SharpeRatio(results.go.g$returns)


#### Mean and Std of portfolio ####

# expected returns of bitcoin portfolio GMV
w.dcc.g = weighted.mean(x = c(colMeans(data.ret.g)), w = c(0.3568038,0.1582201,-0.2021080,0.5647936,0.1222906))
w.adcc.g = weighted.mean(x = c(colMeans(data.ret.g)), w = c(0.3818496,0.1626060,-0.2280616,0.5775171,0.1060889))
w.go.g = weighted.mean(x = c(colMeans(data.ret.g)), w = c(0.8315807,0.1313824,-0.3407777,0.2171263,0.1606883))

r1.g = as.numeric(results.dcc.g$returns)
r2.g = as.numeric(results.adcc.g$returns)
r3.g = as.numeric(results.go.g$returns)

Mean.g = do.call("rbind",list(mean(r1.g)*100,mean(r2.g)*100,mean(r3.g)*100) ) 
Mean.b = do.call("rbind",list(mean(r1.b)*100,mean(r2.b)*100,mean(r3.b)*100) ) 

Mean = do.call("cbind",list(Mean.b,Mean.g))
Mean
rownames(Mean)=c("DCC","ADCC","GO-GARCH")
colnames(Mean)=c("Bitcoin","Gold")

Std.dev.b = do.call("rbind",list(sd(r1.b),sd(r2.b),sd(r3.b)))
Std.dev.g = do.call("rbind",list(sd(r1.g),sd(r2.g),sd(r3.g)))
Std.dev = do.call("cbind",list(Std.dev.b,Std.dev.g))
rownames(Std.dev)=c("DCC","ADCC","GO-GARCH")
colnames(Std.dev)=c("Bitcoin","Gold")
Std.dev

#The standard deviation shows the volatility of an investment's returns relative to its average return, 
#with greater standard deviations reflecting wider returns, 
#and narrower standard deviations implying more concentrated returns. T

#### Turnover ####

bop.dcc.g <- results.dcc.g$BOP.Weight #beginning of period weights
eop.dcc.g <- results.dcc.g$EOP.Weight #end of period weights

bop.adcc.g <- results.adcc.g$BOP.Weight #beginning of period weights
eop.adcc.g <- results.adcc.g$EOP.Weight #end of period weights

bop.go.g <- results.go.g$BOP.Weight #beginning of period weights
eop.go.g <- results.go.g$EOP.Weight #end of period weights

f1.g<-abs(bop.dcc.g-eop.dcc.g)
f2.g<-abs(bop.adcc.g-eop.adcc.g)
f3.g<-abs(bop.go.g-eop.go.g)

## DCC 
YourTurnover.dcc.g=sum(f1.g)*(1/(nrow(data.ret.g)-1))
#SanityCheck.dcc.g=sum(abs(eop.dcc.g-1/ncol(data.ret.g)))/(nrow(data.ret.g)-1)

turnover.year.dcc.g = YourTurnover.dcc.g*252
turnover.year.dcc.g

tradingcost.dcc.5.g = turnover.year.dcc.g*10
trad.dcc.5.g = tradingcost.dcc.5.g/10000
trad.dcc.5.g

# tradingcost.dcc.10.g = turnover.year.dcc.g*350
# trad.dcc.10.g = tradingcost.dcc.10.g/10000
# trad.dcc.10.g

tradingcost.dcc.20.g = turnover.year.dcc.g*20
trad.dcc.20.g = tradingcost.dcc.20.g/10000
trad.dcc.20.g

## ADCC
YourTurnover.adcc.g=sum(f2.g)*(1/(nrow(data.ret.g)-1))
#SanityCheck.adcc.g=sum(abs(eop.adcc.g-1/ncol(data.ret.g)))/(nrow(data.ret.g)-1)

turnover.year.adcc.g = YourTurnover.adcc.g*252
turnover.year.adcc.g

tradingcost.adcc.5.g = turnover.year.adcc.g*10
trad.adcc.5.g = tradingcost.adcc.5.g/10000
trad.adcc.5.g

# tradingcost.adcc.10.g = turnover.year.adcc.g*350
# trad.adcc.10.g = tradingcost.adcc.10.g/10000
# trad.adcc.10.g

tradingcost.adcc.20.g = turnover.year.adcc.g*20
trad.adcc.20.g = tradingcost.adcc.20.g/10000
trad.adcc.20.g

## GOGARCH
YourTurnover.go.g=sum(f3.g)*(1/(nrow(data.ret.g)-1))
#SanityCheck.go.g=sum(abs(eop.go.g-1/ncol(data.ret.g)))/(nrow(data.ret.g)-1)

turnover.year.go.g = YourTurnover.go.g*252
turnover.year.go.g

tradingcost.go.5.g = turnover.year.go.g*10
trad.go.5.g = tradingcost.go.5.g/10000
trad.go.5.g
# 
# tradingcost.go.10.g = turnover.year.go.g*350
# trad.go.10.g = tradingcost.go.10.g/10000
# trad.go.10.g

tradingcost.go.20.g = turnover.year.go.g*20
trad.go.20.g = tradingcost.go.20.g/10000
trad.go.20.g



