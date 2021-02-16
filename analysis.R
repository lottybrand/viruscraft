
library(rethinking)

viruscraft <- read.csv("viruscraft_clean.csv", na.strings=c("","NA"))

#name participants 1 - 42
viruscraft$ppt <- c(1:42)

# higherEdu = 1 if have either undergrad or postgrad degree
viruscraft$higherEdu <- ifelse((viruscraft$edu=="Postgraduate Degree"),1,
                              ifelse((viruscraft$edu=="Undergraduate Degree"),1,0))
     
hist(viruscraft$higherEdu)

table(viruscraft$age)


#### Exploring confidence ####

#confidence1 is before playing game, confidence2 is after playing. 
hist(viruscraft$confidence1)
hist(viruscraft$confidence2)

conf1Edu <- tapply(viruscraft$confidence1, list(viruscraft$higherEdu),mean, na.rm=TRUE)
conf1Edu
conf2Edu <- tapply(viruscraft$confidence2, list(viruscraft$higherEdu),mean, na.rm=TRUE)
conf2Edu

ggplot(data = viruscraft) + 
  geom_bar(mapping = aes(x = confidence1)) +
  facet_wrap(~ higherEdu)

confSet <- subset(viruscraft, select=c(ppt, higherEdu, confidence1, confidence2))

confData <- reshape(confSet, idvar = "ppt", 
                     varying = list(c(3,4)),
                     v.names = c("Confidence"), 
                     direction = "long")

colnames(confData)[3] <- "afterGame"
confData$afterGame <- confData$afterGame -1

confData$afterGame <- ifelse((confData$afterGame ==0),"Before Playing","After Playing")
confData$higherEdu <- ifelse((confData$higherEdu==0),"No Higher Education","Has Higher Education")

confData <- na.omit(confData)

confData$afterGame = factor(confData$afterGame, levels=c("Before Playing","After Playing"), labels=c("Before","After"))
confData$higherEdu = factor(confData$higherEdu, levels=c("Has Higher Education","No Higher Education"), labels=c("Has Higher Ed","No Higher Ed"))

ggplot(data = confData, aes(x= Confidence)) + 
  geom_bar(stat="count",fill="aquamarine4") +
  facet_wrap(higherEdu ~ afterGame) + 
  xlab("Confidence rating") + ylab("No. of Participants") +
  scale_y_continuous(breaks = seq(0,10, by=1)) + scale_x_continuous(breaks = seq(0,7,by=1)) +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  theme(strip.text = element_text(colour = 'yellow3', size=12))


viruscraft$confChange <- viruscraft$confidence2 - viruscraft$confidence1
table(viruscraft$confChange,viruscraft$higherEdu)


confChangeSet <- subset(viruscraft, select=c(ppt, higherEdu, confChange))

confChange <- reshape(confChangeSet, idvar = "ppt", 
                    varying = list(c(3)),
                    v.names = c("ConfChange"), 
                    direction = "long")

confChange$time <- NULL
confChange <- na.omit(confChange)

ggplot(data = confChange) + 
  geom_bar(mapping = aes(x = ConfChange)) +
  facet_wrap(~ higherEdu)

#################################################
############## Virus Infection ##################
#################################################

viruscraft$virusInfectCorrect <- ifelse((viruscraft$virusInfect=="A protein on the virus surface must match the shape of the host's cell"),1,0)
viruscraft$virusInfectCorrect2 <- ifelse((viruscraft$virusInfect2=="A protein on the virus surface must match the shape of the host's cell"),1,0)

hist(viruscraft$virusInfectCorrect)
hist(viruscraft$virusInfectCorrect2)
table(viruscraft$virusInfectCorrect)
table(viruscraft$virusInfectCorrect2)

test1 <- subset(viruscraft, select=c(ppt, virusInfectCorrect, virusInfectCorrect2))

test1long <- reshape(test1, idvar = "ppt", 
                 varying = list(c(2,3)),
                 v.names = c("InfectCorrect"), 
                 direction = "long")

colnames(test1long)[2] <- "afterGame"
test1long$afterGame <- test1long$afterGame -1


test1long<- as.data.frame(test1long)
test1long <- na.omit(test1long)

#simple binomial
model1 <- map2stan(
  alist(
    InfectCorrect ~ dbinom(1, p),
    logit(p) <- a + b*afterGame,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=test1long, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model1, prob=0.95)

#################################################
############## Virus Mutation ##################
#################################################


viruscraft$virusMutateCorrect <- ifelse((viruscraft$virusMutate=="Virus mutations happen mostly randomly"),1,0)
viruscraft$virusMutateCorrect2 <- ifelse((viruscraft$virusMutate2=="Virus mutations happen mostly randomly"),1,0)


test2 <- subset(viruscraft, select=c(ppt, virusMutateCorrect, virusMutateCorrect2))

test2long <- reshape(test2, idvar = "ppt", 
                     varying = list(c(2,3)),
                     v.names = c("MutateCorrect"), 
                     direction = "long")

colnames(test2long)[2] <- "afterGame"
test2long$afterGame <- test2long$afterGame -1


test2long<- as.data.frame(test2long)
test2long <- na.omit(test2long)

#simple binomial
modelMutate <- map2stan(
  alist(
    MutateCorrect ~ dbinom(1, p),
    logit(p) <- a + b*afterGame,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=test2long, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(modelMutate, prob=0.95)

#################################################
############## Virus Evolution ##################
#################################################

viruscraft$virusNewHostCorrect <- ifelse((viruscraft$virusNewHost=="When viruses reproduce, some randomly have different proteins, which might match a new host species' cells"),1,0)
viruscraft$virusNewHostCorrect2 <- ifelse((viruscraft$virusNewHost2=="When viruses reproduce, some randomly have different proteins, which might match a new host species' cells"),1,0)


test3 <- subset(viruscraft, select=c(ppt, virusNewHostCorrect, virusNewHostCorrect2))

test3long <- reshape(test3, idvar = "ppt", 
                     varying = list(c(2,3)),
                     v.names = c("NewHostCorrect"), 
                     direction = "long")

colnames(test3long)[2] <- "afterGame"
test3long$afterGame <- test3long$afterGame -1


test3long<- as.data.frame(test3long)
test3long <- na.omit(test3long)

#simple binomial
modelNewHost <- map2stan(
  alist(
    NewHostCorrect ~ dbinom(1, p),
    logit(p) <- a + b*afterGame,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=test3long, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(modelNewHost, prob=0.95)


