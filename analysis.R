
library(rethinking)

viruscraft <- read.csv("viruscraft_clean.csv")

hist(viruscraft$confidence1)
hist(viruscraft$confidence2)

viruscraft$higherEdu <- ifelse((viruscraft$edu=="Postgraduate Degree"),1,
                              ifelse((viruscraft$edu=="Undergraduate Degree"),1,0))
     
hist(viruscraft$higherEdu)
table(viruscraft$higherEdu)

viruscraft$virusInfectCorrect <- ifelse((viruscraft$virusInfect=="A protein on the virus surface must match the shape of the host's cell"),1,0)
viruscraft$virusInfectCorrect2 <- ifelse((viruscraft$virusInfect2=="A protein on the virus surface must match the shape of the host's cell"),1,0)

hist(viruscraft$virusInfectCorrect)
hist(viruscraft$virusInfectCorrect2)

table(viruscraft$virusInfectCorrect2)
conf1Edu <- tapply(viruscraft$confidence1, list(viruscraft$higherEdu),mean, na.rm=TRUE)
conf1Edu
conf2Edu <- tapply(viruscraft$confidence2, list(viruscraft$higherEdu),mean, na.rm=TRUE)
conf2Edu
