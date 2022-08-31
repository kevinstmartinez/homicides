library(sqldf)
library(ggplot2)


valle_data <- sqldf("SELECT * FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'")
View(valle_data)


# Movil Victima con mayores homicidios haciendo distincion de genero

test <- sqldf("SELECT [Móvil Victima], sexo,  count(sexo) AS CONCA FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)' group by sexo, [Móvil Victima]")
View(test)

ggplot(test, aes(x = `Móvil Victima`, y=CONCA, fill =Sexo))+geom_col(position='dodge')



#Top 5 Barrios con mas homicidios de la ciudad de CALI

test2 <- sqldf("SELECT Barrio, Count(Barrio) AS CONCA FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)' Group By Barrio Order By 2 DESC limit 5")
View(test2)

ggplot(test2,aes(x=Barrio, y=CONCA, fill=Barrio))+geom_col(position='dodge')

