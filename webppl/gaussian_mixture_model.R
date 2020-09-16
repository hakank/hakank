#
#   
# Gaussian mixture model.
# R version
# See gaussian_mixture_model.wppl for a plain webppl model
#
#
# cf ~/blog/gaussian_mixture_model.blog
#     ~/psi/gaussian_mixture_model.psi
#  
#

library(rwebppl)
library(R2jags)
library(coda)

model <- '

var model = function() {
    var xs = [0.2,1.0,0.5,0.6];
    
    var p = beta(0.5, 1); 
    var z = function(i) {
        return Bernoulli({p:p});
    }
    
    var a = uniform(-1, 1);  
    var b = uniform(-1, 1);  
    
    var x = function(i)  {
        if (sample(z(i)) == 1) {
            return Gaussian({mu:a, sigma:1.0})
        } else {
            return Gaussian({mu:b, sigma:1.0});
        }
    }

    observe(x(0),0.2);  
    observe(x(1),1.0);  
    observe(x(3),0.5);  
    observe(x(4),0.6);
    
    return {
        aGreaterThanB:a>b,
        aMinusB:a-b,            
        a:a,
        b:b,
        p:p
    };

}

// var d = Infer({method:"rejection",samples:10000},model);
// var d = Infer({method:"MCMC",kernel:"MH",sample:100000},model);
// var d = Infer({method:"SMC",particles:10000},model);
var d = Infer(model);
// display(d);
d;
'

sample = webppl(model, sort_by=T,chains=1)

str(sample)
# sample$Parameter
# Note: The values are in one single list
# One have to split according to the factoris in Parameter!
lev = levels(sample$Parameter)
# a=sample$value[sample$Parameter=="a"]
# b=sample$value[sample$Parameter=="b"]
# agtb=sample$value[sample$Parameter=="aGreaterThanB"]
# v = as.mcmc(cbind(theta,thetaPrior))
# This works.
# TODO: Incorporate the different chains for gelman.* to work...
nIterations = attr(sample,"nIterations")
nIterations
nChains = attr(sample,"nChains")
nChains
nLevels = length(levels(sample$Parameter))
vv = array(sample$value,c(nIterations,nChains,nLevels))
attr
# vv
str(vv)
t = cbind()
# sample$Iteration
# sample$Chain
for(lev in levels(sample$Parameter)) {
  print(lev)
  # tt = c()
  # for(c in 1:nChains) {
  #   tt = c(tt,sample$value[sample$Parameter==lev && sample$Chain==c])
  # }
  t = cbind(t,sample$value[sample$Parameter==lev])
  # t = cbind(t,tt)
}
str(t)
colnames(t) <- levels(sample$Parameter)
# str(t)

v = as.mcmc(t)
# v = as.mcmc(as.matrix(vv))

plot(v)
summary(v)
# coda::summary.mcmc(v)
# mean(theta)
# mean(thetaPrior)
apply(v,2,mean)

raftery.diag(v)

# gelman.plot(v)
gelman.diag(v)
