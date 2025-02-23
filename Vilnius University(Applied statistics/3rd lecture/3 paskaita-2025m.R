library(carData)
data(Davis)
Davis
attach(Davis)
plot(weight[sex=="M"],height[sex=="M"])

plot(weight[sex=="M"],height[sex=="M"],xlim =c(50,120),ylim=c(150,200),
main="Vyrø ûgio ir svorio pasiskirstymas")
lm(height[sex=="M"] ~ weight[sex=="M"])
abline(lm(Davis$height[sex=="M"]~Davis$weight[sex=="M"]))

plot(weight,height,xlim =c(50,120),ylim=c(150,200),
pch=as.character(sex))
lm(height ~ weight)
abline(lm(Davis$height~Davis$weight))

plot(weight,height,xlim =c(50,120),ylim=c(150,200),
pch=as.character(sex),col=as.numeric(sex))



cor(weight[sex=="M"],height[sex=="M"], method ="pearson")

rank(weight[sex=="M"])
rank(height[sex=="M"])

cor(weight[sex=="M"],height[sex=="M"], method ="spearman")

cor(rank(weight[sex=="M"]),rank(height[sex=="M"]), method ="pearson")

cor(weight[sex=="M"],height[sex=="M"], method ="kendall")

Davis[1:20,]
cor(Davis[, c(2,3, 4, 5)], method ="pearson")


opcijos

use = "pairwise.complete.obs"
use = "complete.obs"

cor(Davis[, c(2,3, 4, 5)], method ="pearson",use = "pairwise.complete.obs")

cor(Davis[, c(2,3, 4, 5)], method ="pearson",use = "complete.obs")


library(corrplot)

corrplot-nubraiþo koreliacijø matricà. 

opcijos
method
number
type
main
mar
bg

korel_matrica<-cor(Davis[, c(2,3, 4, 5)], method ="pearson",
use = "complete.obs")
korel_matrica
corrplot(korel_matrica)
corrplot(korel_matrica,method="number")
corrplot(korel_matrica,method="number",type = "upper")
corrplot(korel_matrica,method="number",type = "upper",
main="Pirsono koreliacijos")
corrplot(korel_matrica,method="number",type = "upper",
main="Pirsono koreliacijos",mar=c(0,0,5,0))
corrplot(korel_matrica,method="number",type = "upper",
main="Pirsono koreliacijos",mar=c(0,0,5,0),bg="5")



