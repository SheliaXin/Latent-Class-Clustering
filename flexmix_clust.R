library(flexmix)
library(ggplot2)
library(fpc)
library(MASS)

#####################
# clustering
#####################

## 2 discrete (multinomial) + 2 continue ##
set.seed(112233)
options(digits=3)
data(Cars93)
Cars934 <- Cars93[,c(3,5,8,10)]
names(Cars934) <- names(Cars93)[c(3,5,8,10)]
cc <- discrete.recode(Cars934, xvarsorted = FALSE, continuous = c(2,3), discrete = c(1,4))
fcc <- flexmix(cc$data~1,k = 2,
               model = lcmixed(continuous=2, discrete  = 2, ppdim = c(6,3), diagonal=TRUE))
summary(fcc)
para = parameters(fcc)
para
fcc

plot(para[,1], xaxt="n", xlab="",type = "b")
lines(para[,2], type = "b",col="red")
axis(1,at = 1:length(para[,1]), labels=rownames(para))


library("reshape2")
library(plotly)
data <- cbind("id"= rownames(para), as.data.frame(para))
data_long <- melt(data, id = "id")
ggplot(data = data_long,  
       aes(x = id, y = value, group = variable, col = variable)) +
  geom_line() +
  geom_point() +
  geom_line(data = as.data.frame(data_long[data_long$variable == 'Comp.2',]), size=1) +
  geom_point(data = as.data.frame(data_long[data_long$variable == 'Comp.2',]), size=2)
ggplotly()

ggplot(data=data) +
  geom_point(data = data_long,  
             aes(x = id, y = value, group = variable, col=variable)) +
  #geom_line()



temp <- data.frame(temperature = c(32,33,43,37), 
                   place = paste("Place", 1:4))
# Create a scatterplot, with an hidden x axis
plot(temp$temperature, pch=20, ylim=c(0, 50), 
     xaxt="n", xlab="Place", ylab="Temperature")
# Plot the axis separately
axis(1, at=1:4, labels=temp$place)

### or ### 
library(fpc)
fr <- flexmixedruns(cc$data,
                    continuous=2,discrete=2,simruns=2,n.cluster=2:3,allout=FALSE)
print(fr$optimalk)
print(fr$optsummary)
print(fr$flexout@cluster)
print(fr$flexout@components)



## 1 normal + 1 binomial ##
x1 <- cbind(rnorm(300),
            sample(0:1, 300, replace = TRUE, prob = c(0.25, 0.75)))
x2 <- cbind(rnorm(300, mean = 2, sd = 0.5),
            sample(0:1, 300, replace = TRUE, prob = c(0.75, 0.25)))
x <- rbind(x1, x2)
f1 <- flexmix(x ~ 1, k = 2, model = FLXMCmvcombi())
parameters(f1)
table(clusters(f1), rep(1:2, c(300,300)))
x <- cbind(x, rnorm(600)) # add a noise
f2 <- flexmix(x ~ 1, k = 2, model = FLXMCmvcombi())
parameters(f2)
table(clusters(f2), rep(1:2, c(300,300)))



## 2d normal with/without local dependence ##
library(ellipse)
data("Nclus", package = "flexmix")
eqscplot(Nclus)
ex1 <- flexmix(Nclus ~ 1, k = 4, model = FLXMCmvnorm()) # assume local independence
print(ex1)
plotEll(ex1, Nclus)
ex2 <- flexmix(Nclus ~ 1, k = 6, model = FLXMCmvnorm(diagonal = FALSE)) # with local dependence, full, 
print(ex2)
plotEll(ex2, Nclus)
parameters(ex2, component = 1) # Get parameters of first component
ok <- sample(1:nrow(Nclus), 10) # Have a look at the posterior probabilies of 10 random observations
p <- posterior(ex2)[ok, ]
max.col(p)          # The following two should be the same
clusters(ex2)[ok]
plot(density(Nclus[, 1]))  # Now try the univariate case
ex3 <- flexmix(Nclus[, 1] ~ 1, cluster = cut(Nclus[, 1], 3),
               model = FLXMCnorm1())
ex3
parameters(ex3)



###########################
data("NPreg", package = "flexmix")
plot(NPreg$x, NPreg$yn,col = NPreg$class)
plot(NPreg$yn, NPreg$yp, col = NPreg$class)

ex1 <- flexmix(yn ~ 1, data = NPreg, k = 2,
               control = list(verb = 5, iter = 100))

ex1
parameters(ex1)
table(NPreg$class, clusters(ex1))
summary(ex1)
plot(ex1)
plot(NPreg$x, NPreg$yn, col = ex1@cluster)
plot(NPreg$yn, NPreg$yp, col = ex1@cluster)

## test for significance of regression coefficients
rex1 <- refit(ex1)
summary(rex1) 

## one Gaussian response and one Poisson
ex2 <- flexmix( ~ 1, data = NPreg, k = 2,
               model = list(FLXMRglm(yn ~ .), 
                            FLXMRglm(yp ~ ., family = "poisson")))
plot(ex2)

ex2
table(ex2@cluster, NPreg$class)
plot(NPreg$x, NPreg$yn,col = ex2@cluster)
plot(NPreg$yn, NPreg$yp, col = ex2@cluster)

## fitting a model only to the Poisson response is of course
ex3 <- flexmix(yp ~ 1, data = NPreg, k = 2,
               model = FLXMRglm(family = "poisson"))
summary(ex3)
plot(ex3)
table(ex3@cluster, NPreg$class)
plot(NPreg$x, NPreg$yn,col = ex3@cluster)
plot(NPreg$yn, NPreg$yp, col = ex3@cluster)

## if observations are grouped, i.e., we have several observations per
## individual, fitting is usually much faster:
ex4 <- flexmix( ~1|id2, data = NPreg, k = 2,
               model = list(FLXMRglm(yn ~ .), 
                            FLXMRglm(yp ~ ., family = "poisson")))
ex4
summary(ex4)
table(ex4@cluster, NPreg$class)
plot(NPreg$x, NPreg$yn,col = ex4@cluster)
plot(NPreg$yn, NPreg$yp, col = ex4@cluster)


ex5 <- initFlexmix( ~ 1 | id2, data = NPreg, k = 2,
                   model = list(FLXMRglm(yn ~ .), 
                                FLXMRglm(yp ~ ., family = "poisson"),
                                FLXMRglm(cbind(yb, 1 - yb)~ ., family = "binomial")),
                   nrep = 5)
table(NPreg$class, clusters(ex5))
plot(NPreg$x, NPreg$yn,col = ex5@cluster)
plot(NPreg$yn, NPreg$yp, col = ex5@cluster)

# boostrap
data("NPreg", package = "flexmix")
fitted <- initFlexmix(yn ~ 1 | id2, data = NPreg, k = 2)
lrtest <- LR_test(fitted, alternative = "greater", R = 20,
                  verbose = 1)
lrtest



####
data("dmft", package = "flexmix")
dmft_flx <- initFlexmix(End ~ 1, data = dmft, k = 2,
                        model = FLXMRglmfix(family = "poisson",
                                            fixed = ~ Gender + Ethnic + Treatment))
dmft_flx
head(dmft)


## with concomitant (multinomial)
set.seed(2807)
data("NregFix", package = "flexmix")
Model <- FLXMRglm(~ x2 + x1)
fittedModel <- stepFlexmix(y ~ 1, model = Model, nrep = 3, k = 3,
                          data = NregFix, concomitant = FLXPmultinom(~ w))

summary(fittedModel)
parameters(fittedModel)
fittedModel <- relabel(fittedModel, "model", "x1")
summary(refit(fittedModel))

Model2 <- FLXMRglmfix(fixed = ~ x2, nested = list(k = c(1, 2),
                       formula = c(~ 0, ~ x1)), varFix = TRUE)
fittedModel2 <- flexmix(y ~ 1, model = Model2,
                        cluster = posterior(fittedModel), data = NregFix,
                        concomitant = FLXPmultinom(~ w))
BIC(fittedModel)
BIC(fittedModel2)
summary(refit(fittedModel2))
par(mfrow = c(2,2))
plot(NregFix$x1, NregFix$y, col = NregFix$class)
plot(NregFix$x1, NregFix$y, col = fittedModel@cluster)
plot(NregFix$x1, NregFix$y, col = fittedModel2@cluster)


## clustering try
hist(NregFix$y)
Model_c <- FLXMCmvnorm(diagonal = FALSE)
fittedModel_c <- stepFlexmix(x1+y ~ 1, model = Model_c, nrep = 5, k = 3,
                           data = NregFix, concomitant = FLXPmultinom(~ w))
summary(fittedModel_c)
parameters(fittedModel_c)
par(mfrow = c(2,2))
plot(NregFix$x1, NregFix$y, col = NregFix$class)
plot(NregFix$x1, NregFix$y, col = fittedModel_c@cluster)

# fixed effect, betablocker
data("betablocker", package = "flexmix")
head(betablocker)
betaMix <- initFlexmix(cbind(Deaths, Total - Deaths) ~ 1 | Center,
                       data = betablocker, k = 3, nrep = 5,
                       model = FLXMRglmfix(family = "binomial",
                                           fixed = ~Treatment))
betaMix <- flexmix(cbind(Deaths, Total - Deaths) ~ 1,
                       data = betablocker, k = 3, 
                       model = FLXMRglmfix(family = "binomial",
                                           fixed = ~Treatment),
                   concomitant = FLXPmultinom(~ Treatment) )
parameters(betaMix)
summary(betaMix)
plot(betablocker$Deaths, betablocker$Total- betablocker$Deaths,col= betablocker$Treatment, pch = betablocker$Center)
plot(betablocker$Deaths, betablocker$Total- betablocker$Deaths,col = betaMix@cluster)


#########

data("dmft", package = "flexmix")
dmft_flx <- initFlexmix(End ~ 1, data = dmft, k = 2:3,
                        model = FLXMRglmfix(family = "poisson",
                                            fixed = ~ Gender + Ethnic + Treatment))
unique(dmft_flx)

d <- capture.output(dmft_flx)
dd <- tail(d, 3)
tt <- strsplit(dd,"\\s+")

t <- do.call(rbind, lapply(seq_along(tt), function(i){tt[[i]]}))
colnames(t) <- t[1,]
t <- t[-1,]
xtable(t)



parameters(dmft_flx[1])
plot(dmft$Begin,dmft$End,col=dmft$Ethnic)
plot(dmft$Begin,dmft$End,col=dmft_flx@cluster)

############





## random effect 
data("salmonellaTA98", package = "flexmix")
salmonMix <- initFlexmix(y ~ 1,
                         data = salmonellaTA98,
                         model = FLXMRglmfix(family = "poisson",
                                             fixed = ~ x + log(x + 10)),
                         k = 2, nrep = 5)
salmonMix.pr <- predict(salmonMix, newdata = salmonellaTA98)
plot(y ~ x, data = salmonellaTA98,
     pch = as.character(clusters(salmonMix)),
     ylim = range(c(salmonellaTA98$y, unlist(salmonMix.pr))))
for (i in 1:2) lines(salmonellaTA98$x, salmonMix.pr[[i]], lty = i)

###########
# mixture Poisson

data("fabricfault", package = "flexmix")
fabricMix <- stepFlexmix(Faults ~ 1, model = FLXMRglmfix(family="poisson", 
                                                         fixed  = ~ log(Length)), data = fabricfault, 
                         k = 2,  nrep = 3)
summary(fabricMix)
parameters(fabricMix)

summary(refit(fabricMix))

Lnew <- seq(0, 1000, by = 50)
fabricMix.pred <- predict(fabricMix, newdata = data.frame(Length = Lnew))

## the intercept of the first component is not significantly different from zero
## for a signficance level of 0.05. Therefore fit a modified model where the intercept 
## is a priori set to zero for first component

fabricMix2 <- flexmix(Faults ~ 0, data = fabricfault,
                      cluster = posterior(fabricMix),
                      model = FLXMRglmfix(family = "poisson", fixed = ~ log(Length),
                                          nested = list( k = c(1,1), formula = list(~0,~1))))
summary(fabricMix2)
summary(refit(fabricMix2))




############
## simple example in 2d
beta <- matrix(c(1, 2, 3, -1), ncol = 2)
sigma <- c(.5, 1)
df1 <- ExLinear(beta, 100, sd = sigma, min = -1, max = 2)
plot(y~x1, df1, col = attr(df1, "clusters"))
## add a second explanatory variable with exponential distribution
beta2 <- rbind(beta, c(-2, 2))
df2 <-  ExLinear(beta2, 100, sd = c(.5, 1),
                 xdist = c("runif", "rexp"),
                 xdist.args = list(list(min = -1, max = 2),
                                   list(rate = 3)))
summary(df2)
opar = par("mfrow")
par(mfrow = 1:2)
hist(df2$x1)
hist(df2$x2)
par(opar)
f1 <- flexmix(y ~ ., data = df2, k = 2)
## sort components on Intercept
f1 <- relabel(f1, "model", "Intercept")
## the parameters should be close to the true beta and sigma
round(parameters(f1), 3)
rbind(beta2, sigma)
### A simple Poisson GLM
df3 <- ExLinear(beta/2, 100, min = -1, max = 2, family = "poisson")
plot(y ~ x1, df3, col = attr(df3, "clusters"))
f3 <- flexmix(y ~ ., data = df3, k = 2,
              model = FLXMRglm(family = "poisson"))
round(parameters(relabel(f3, "model", "Intercept")), 3)
beta/2



data("NPreg", package = "flexmix")
ex1 <- flexmix(yn ~ x + I(x^2), data = NPreg, k = 2)
matplot(NPreg$x, fitted(ex1), pch = 16, type = "p")
points(NPreg$x, NPreg$yn)

posterior(ex1)
