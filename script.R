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





cities <- sqldf("WITH A AS(SELECT COUNT(*) as counter, Municipio FROM Homicidios WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'
              UNION ALL
              SELECT COUNT(*) as counter, Municipio FROM Homicidios WHERE Departamento = 'ANTIOQUIA' AND Municipio = 'MEDELLÍN (CT)'
              UNION ALL
              SELECT COUNT(*) as counter, Municipio FROM Homicidios WHERE Departamento = 'CUNDINAMARCA' AND Municipio = 'BOGOTÁ D.C. (CT)'
              UNION ALL
              SELECT COUNT(*) as counter, Municipio FROM Homicidios WHERE Departamento = 'ATLÁNTICO' AND Municipio = 'BARRANQUILLA (CT)'
              UNION ALL
              SELECT COUNT(*) as counter, Municipio FROM Homicidios WHERE Departamento = 'SANTANDER' AND Municipio = 'BUCARAMANGA (CT)')
              SELECT * FROM A ORDER BY 1 DESC 
              ")
View(cities)



ggplot(cities,  aes(x= reorder(Municipio, counter),  y=counter, fill=Municipio))+geom_col()+  coord_flip() +
  
  labs( title = "Comparativa de homicidios de las principales ciudades del país\n", x= "Ciudad" , y ="Cantidad de víctimas" )+
  geom_text(aes(y = counter, label = counter), 
            position = position_dodge(width = 0.9), size=4, vjust=-1, hjust=0.5 ,col="black")+
  theme(
    axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold", color = "Black", hjust= .5)
  )



test3 <- sqldf("SELECT Barrio, Count(Barrio) AS CONCA,
  ROUND(Count(Barrio)   / ( SELECT (SUM(CONCA) *1.0)
  FROM
    (SELECT Barrio, Count(Barrio) AS CONCA
    FROM Homicidios
    WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'
    Group By Barrio
    Order By 2 DESC limit 5)A)*100 ,2)   AS Total
FROM Homicidios
WHERE Departamento = 'VALLE' AND Municipio = 'CALI (CT)'
Group By Barrio
Order By 2 DESC
limit 5")
View(test3)


ggplot(test3,aes(x="",y=Total, fill=Barrio))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=paste(Total, "%")),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")

ggplot(test2,aes(x=Barrio, y=Cantidad, fill=Barrio))+geom_col(position='dodge')+
  labs( title = "Top 5 Barrios con mayor cantidad de homicidios en la ciudad de Cali\n", x= "Barrio" , y ="Cantidad de víctimas" )+
  geom_text(aes(y = Cantidad,label = Cantidad), 
            position = position_dodge(width = 0.9), size=2.5, vjust=-1, hjust=0.5 ,col="black")+
  #Rango de datos
  scale_y_continuous(limit = c(0,40))+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", color = "black")
  )

