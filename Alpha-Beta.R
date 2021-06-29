#### alpha and beta ####

# take gold as benchmark and then compare it with bitcoin port

a1 = CAPM.alpha(results.dcc.b$returns, results.dcc.g$returns)
b1 = CAPM.beta(results.dcc.b$returns, results.dcc.g$returns)

a2 = CAPM.alpha(results.adcc.b$returns,results.adcc.g$returns)
b2 = CAPM.beta(results.adcc.b$returns, results.adcc.g$returns)

a3 = CAPM.alpha(results.go.b$returns, results.go.g$returns)
b3 = CAPM.beta(results.go.b$returns, results.go.g$returns)

alpha = do.call("rbind",list(a1,a2,a3)) 
beta =  do.call("rbind",list(b1,b2,b3))
colnames(alpha) = "ALPHA"
rownames(alpha) = c("DCC","ADCC","GO-GARCH")
colnames(beta) = "BETA"
rownames(beta) = c("DCC","ADCC","GO-GARCH")
alpha
beta

##An alpha of 1.0 means the investment outperformed its benchmark index by 1%.
## A beta of less than 1 means that the security is less volatile than
## the market, while a beta greater than 1 indicates that its price is more volatile than the market
#### T-test ####
t.test(s1.g , s1.b)
t.test(s2.g , s2.b)
t.test(s3.g , s3.b)

sharpe.ratio = do.call("cbind",list(s1.b,s1.g,s2.b,s2.g,s3.b,s3.g))
sharpe.ratio

