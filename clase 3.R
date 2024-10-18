library(wooldridge)
library(model sumary)
View(bwght)

#Reg ing familiar
reg1<-lm(bwght ~ faminc, data=bwght)
#reg peso con los cigs
reg2<-lm(bwght ~ cigs, data= bwght)
#reg peso con ing fami y cigs
reg3<-lm(bwght~cigs+faminc,data=bwght)
modelos<- list("modelo1" = reg1,"modelo2"= reg2,"modelo3"=reg3,"modelo4"=reg4,"modelo5"=reg5,"modelo6"=reg6)
modelsummary:: modelsummary (modelos)
#Transformar las variables ahora en onzas 
reg4 <-lm(bwghtlbs~cigs+faminc,data=bwght)
#transformar la variable indep
reg5<-lm(bwght~packs+faminc,data=bwght)


#Segundo ejercicio
library(dplyr)
#creando la data para pasar todo a estandarizados a una misma unidad
bwght_alt<-bwght|>mutate(zbw=(bwght-mean(bwght))/sd(bwght),
                          zfi=(faminc-mean(faminc))/sd(faminc),
                          zcigs=(cigs-mean(cigs))/sd(cigs))
View(bwght_alt)
#Modelos con las variables que creamos
reg6<-lm(zbw~zfi+zcigs,data=bwght_alt)

sigma_cigs<- sd(bwght$cigs)
sigma_bwght<-sd(bwght$bwght)
#aqui todos los resultados son en desviaciones estandar