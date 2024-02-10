#construindo gráficos
library(tidyverse)
l= matrix(c(1,2,3,4,5,2,4,6,8,10),5,2)
m=lm(l[,2]~l[,1]) #regressão linear de x,y
plot(l[,2]~l[,1]) #plotar x e y dispersão (padrão)

plot(l[,2]~l[,1],xlab="x",ylab="y") #dar nome aos eixos
plot(l[,2]~l[,1],xlab="x",ylab="y" ,col="red" ,main="gráfico") #definindo cor e título
plot(l[,2]~l[,1],xlab="x",ylab="y" ,col="red" ,main="Gráfico exemplo",pch=18,type="b") #definindo simbolo e linha
boxplot(l) #boxplot

#gráfico em pizza
n=c(10,20,30,40)
o=c("laranja","banana","caju","maçã") 
pie(n,o)

#gráfico em barra
barplot(n)
barplot(n, name=o, col="green") #inserir o nome e cor das barras
barplot(n, name=o, las=2) #inserir rótulos verticais

#ajuste da reta
abline(lm(l[,2]~l[,1]))
plot(fitted(m), residuals(m),xlab="Valores ajustados",ylab="Resíduos")
abline(h=0) #reta dos resíduos
hist(residuals(m), xlab = "Resíduos",ylab = "Frequência",main = "")




#usando ggplot2
library(ggplot2)
install.packages("ggalt")
library(ggalt)
data("midwest", packages="ggplot2") #dados de exemplo
options(scipen=999) #caso queira retirar notação científica
view(midwest)
ggplot(midwest, aes(x=area,y=poptotal))

#gráfico de densidade populacional por área
ggplot(midwest, aes(x=area,y=poptotal))+geom_point(aes(col=state,size=popdensity))

#delimitando
ggplot(midwest, aes(x=area,y=poptotal))+geom_point(aes(col=state,size=popdensity))+ xlim(c(0,0.07))+ylim(0,500000)

#usando esquisse para Gui de plotagem

install.packages("esquisse")
library(esquisse)
library(tidyverse)

guerra_nas_estrelas = starwars

#gráficos animados
install.packages("animation")
library(animation)
exemplo$x
for(i in 1:5){
  plot(exemplo$x[1:i], exemplo$y[1:i], xlim=range(exemplo$x), ylim=range(exemplo$y))
}
