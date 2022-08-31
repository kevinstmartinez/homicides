library(sqldf)


valle_data <- sqldf("SELECT * FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'")
View(valle_data)

sqldf("SELECT [Móvil Agresor] FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)' AND [Móvil Agresor] = 'A PIE'")

test <- sqldf("SELECT sexo|| ' - ' ||[Móvil Victima] AS CONCA FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'")
View(test)

test1 <- sqldf("SELECT [País de Nacimiento] AS CONCA FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'")
View(test1)

windows()
x <- table(valle_data$`Móvil Victima`)
y <- table(valle_data$`Móvil Victima`)
z <- table(valle_data$`Barrio`)
k <- table(test$`CONCA`)
g



barplot(x, xlab = 'Movil Agresor', ylab = 'Frequency', main='Transporte principal', col = '#fe04a2')  

barplot(z, xlab = 'Movil Agresor', ylab = 'Frequency', main='Transporte principal', col = '#fe04a2',  horiz = TRUE)  

barplot(k, xlab = 'Movil Agresor', ylab = 'Frequency', main='Transporte principal', col = '#ce23a5',  horiz = F)

bar <- barplot(k, xlab = 'Movil Agresor', ylab = 'Frequency', main='Transporte principal', col = '#ce23a5',  horiz = F)

text(bar , paste("n: ",  test$CONCA, sep="") ,cex=1) 

barplot(g, xlab = 'Movil Agresor', ylab = 'Frequency', main='Transporte principal', col = '#ce23a5',  horiz = F)


