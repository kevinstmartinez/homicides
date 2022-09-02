library(sqldf)
library(ggplot2)


valle_data <- sqldf("SELECT * FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'")
View(valle_data)


# Movil Victima con mayores homicidios haciendo distincion de genero

test <- sqldf("SELECT CASE WHEN [Móvil Victima] !='-' THEN [Móvil Victima] ELSE 'DESCONOCIDO' END AS [Móvil Victima] , sexo,  count(sexo) AS CONCA FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)' group by sexo, [Móvil Victima]")
#Visualizar Consulta#
View(test)

#Graficar
ggplot(test, aes(x = `Móvil Victima`, y=CONCA, fill =Sexo))+
  #Rango de datos
  scale_y_continuous(limit = c(0,1080))+
  #Separa la columna de acuerdo al género
  geom_col(position='dodge' ) +
  labs( title = "Víctimas de homicidio según el género y el tipo de movilidad\n", x= "Tipo de movilidad" , y ="Cantidad de víctimas" )+
  #Valores encima de las columnas
  geom_text(aes(y = CONCA, ymax = CONCA, label = CONCA), 
            position = position_dodge(width = 0.9), size=2.5, vjust=-1, hjust=0.5 ,col="black")+
  #Eje X vertical, y personaliza el título
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", color = "black",hjust = 0.5)
  )


#--------------------------------------




ggplot(test, aes(x = `Móvil Victima`, y=CONCA, color =Sexo))+
  +
geom_col(aes(y    =  CONCA,
             fill  =  `Móvil Victima`),position='dodge' ) + 
geom_text(aes(y = CONCA, ymax = CONCA, label = CONCA), 
                      position = position_dodge(width = 0.9), size=2.5, vjust=-1, hjust=0.5 ,col="black")+
labs( title = "Gráfica de víctimas de homicidio según el género y el modo de movilidad\n", x= "Tipo de movilidad" , y ="Cantidad de víctimas" )+
scale_color_manual(labels = c("Masculino", "Femenino"), values = c("blue", "red")) +
theme(
       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold", color = "blue")
      )




#Top 5 Barrios con mas homicidios de la ciudad de CALI

test2 <- sqldf("SELECT 2 / ( CAST( 5 AS DECIMAL(10,2))  ) ")
View(test2)

ggplot(test2,aes(x=Barrio, y=CONCA, fill=Barrio))+geom_col(position='dodge')+
geom_text(aes(y = CONCA, ymax = CONCA, label = CONCA), 
            position = position_dodge(width = 0.9), size=2.5, vjust=-1, hjust=0.5 ,col="black")+
theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 8),
  axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
  plot.title = element_text(size = 10, face = "bold", color = "blue")
)

