
library(shiny)
library(tidyverse)
library(here)
library(lubridate)
library(rgdal)
library(xtable)
library(DT)

load(here("Datos/personas_13_21.RData"))

pobdeptos <- read_csv(here("Datos/poblaciondeptos.csv"))

mapaine <- readOGR(here("ine_depto.shp"))

dframe_depto <- ggplot2::fortify(mapaine)


ui <- fluidPage(
    titlePanel("Trabajo grupal"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = "input.paneles==1",
                             selectInput("anios", "Año del gráfico ",
                                         c(2013, 2014, 2015, 2016, 2017,
                                           2018, 2019, 2020, 2021))
            ),
            conditionalPanel(condition = "input.paneles==2",
                             selectInput("anio", "Año del gráfico",
                                         c(2013, 2014, 2015, 2016, 2017,
                                           2018, 2019, 2020, 2021))
            ),
            conditionalPanel(condition = "input.paneles==3",
                             selectInput('variable', 'Variable en gráfico de barras',
                                         c("Rol", "Zona", "Tipo de siniestro",
                                           "Tipo de Vehiculo", "Usa cinturón", "Usa casco",
                                           "Día de la semana")),
                             selectInput("aniobarra","Año del gráfico",
                                         c(2013, 2014, 2015, 2016, 2017,
                                           2018, 2019, 2020, 2021))
            )
        ),
        mainPanel(
            tabsetPanel(type = "tabs", id = "paneles", selected = 2,
                        tabPanel("Evolución temporal",
                                 h2("Gráfico con la evolución temporal de accidentes", align="center"),
                                 plotOutput("tiempo"), value = 1),
                        tabPanel("Mapa departamental",
                                 h2("Accidentes según departamento cada 1000 habitantes",
                                    align="center"),
                                 plotOutput("mapa"),
                                 DTOutput("tabla"), value = 2),
                        tabPanel("Gráfico de Barras",
                                 h2("Cantidad de accidentes según distintas variables ", align="center"),
                                 plotOutput("barras"), value = 3),
                        tabPanel("Gravedad del accidente",
                                 h2("Cantidad de accidentes según la gravedad ", align="center"),
                                 plotOutput("gravedad"), value = 1),
                        tabPanel("Vehículos",
                                 h2("Evolución temporal de accidentes según vehículo", align="center"),
                                 plotOutput("vehiculo"), value = 1) ) 
        ) ) )
server <- function(input, output) {
    
    datostime <- reactive({personas_13_21 %>%
            mutate(Fecha=parse_date_time(Fecha,order=c("dmy","ymd")))%>% 
            mutate(Ano = year(Fecha)) %>% 
            mutate(Mes = month(Fecha))
    })
    grafico <- reactive({ datostime()%>% 
            group_by(Ano, Mes) %>% 
            summarise(cantidad = n()) %>% 
            filter(Ano == input$anios)
    })
    
    output$tiempo <- renderPlot({
        ggplot(data = grafico(), aes(x = Mes, y = cantidad, colour = "Blue")) +
            geom_line(size = 1) +
            geom_point(size = 1.5) +
            scale_color_brewer(palette = "Dark2", guide = "none") +
            scale_x_discrete(limit = c(1:12),
                             labels = c("Enero", "Febrero", "Marzo", "Abril",
                                        "Mayo", "Junio", "Julio", "Agosto",
                                        "Setiembre", "Octubre", "Noviembre",
                                        "Diciembre")) +
            theme(axis.text.x = element_text(angle = 90),
                  text = element_text(size = 10)) +
            labs(x = "Mes", y = "Cantidad", 
                 title = paste("Cantidad de accidentes en el año", input$anios))
    })
    
    deptos <-reactive({ dframe_depto %>%
        rename(Departamento = id) %>%
        mutate(Departamento=recode(Departamento, `0` = "MONTEVIDEO",
                                   `8` = "RIO NEGRO", `9` = "RIVERA",`7`="PAYSANDU",
                                   `1`="ARTIGAS",`2`="CANELONES",`19`="CERRO LARGO",
                                   `3`="COLONIA",`4`="DURAZNO",`5`="FLORIDA",`6`="LAVALLEJA",
                                   `10`="ROCHA",`11`="SALTO", `12`="SAN JOSE",`13`="SORIANO",
                                   `14`="TREINTA Y TRES", `16`="TACUAREMBO",`17`="FLORES",
                                   `18`="MALDONADO",`15`="ARTIGAS"))
    })
        datostime <- reactive({personas_13_21 %>%
        mutate(Fecha=parse_date_time(Fecha,order=c("dmy","ymd")))%>% 
        mutate(Ano = year(Fecha)) %>% 
        mutate(Mes = month(Fecha))
    })
    cantsiniestros <- reactive({datostime() %>%
        group_by(Departamento,Ano) %>%
        summarise(Cantidad = n()) %>%
        filter(Ano == input$anio)
    })
    habitantes <-reactive({merge(cantsiniestros(),pobdeptos)})
    
       tabla <- reactive({ habitantes() %>%
               group_by(Departamento) %>%
               summarise(canthabitantes =
                             (Cantidad/poblacion)*1000)
    })
    mapauru <- reactive({left_join(deptos(),
                                   tabla(), by = "Departamento")
    })
    output$mapa <- renderPlot({ mapauru() %>%
            ggplot(aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = canthabitantes)) +
            scale_fill_viridis_c() +
            labs(x="Longitud", y="Latitud",
                 fill="Cantidad de accidentes/1000 hab",
                 title=paste("Cantidad de accidentes en el año",
                 input$anio))
        
    })
    
    
    output$barras <- renderPlot({ datostime() %>%
        filter(Sexo!="SIN DATOS") %>%
        filter(Rol!="Fallecidos") %>%
        filter(Ano == input$aniobarra) %>%
        group_by(.data[[input$variable]],Sexo) %>%
        summarise(cant =n()) %>%
        ggplot(aes(x = .data[[input$variable]], y = cant, fill = Sexo)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_fill_brewer(palette = "Set1") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(y = "Cantidad", 
             title = paste("Cantidad de accidentes en el año", input$aniobarra),
             subtitle = "Diferenciado por sexo")
    })
    
    output$gravedad <- renderPlot({datostime() %>%
            filter(Ano==input$anios) %>%
            mutate(Resultado = if_else(grepl("FALLECIDO+",`Tipo de resultado`),
                                       "FALLECIDO", `Tipo de resultado`)) %>% 
            filter(`Tipo de Vehiculo` != "MONOPATIN ELECTRICO") %>% 
            ggplot(aes(Resultado, 
                       fill = `Tipo de Vehiculo`)) +
            geom_bar(position = "fill") +
            scale_y_continuous(labels = scales::percent_format()) +
            scale_fill_viridis(discrete = T, option = "A") +
            ylab("Porcentaje") +
            theme_bw()
    })
    
    personas_13_21$Mes <- month(dmy(personas_13_21$Fecha))
    personas_13_21$Ano <- year(dmy(personas_13_21$Fecha))
    personas_13_21$mes_ano <- my(paste(personas_13_21$Mes,
                                       personas_13_21$Ano, sep = "-"))
   
    output$vehiculo <- renderPlot({ personas_13_21 %>%
            filter(`Tipo de Vehiculo` != "MONOPATIN ELECTRICO") %>% 
            filter(Ano == input$anios) %>%
   ggplot(aes(x = mes_ano, color = `Tipo de Vehiculo`)) +
       geom_line(stat = "count", size = 1)+
       #stat_count(geom = "line", size = 1) +
       scale_color_brewer(palette = "Dark2") +
       theme(axis.text.x = element_text(angle = 90),
             text = element_text(size = 10)) +
       labs(x = "Fecha", y = "Cantidad") +
       theme_bw() +
       theme(legend.position = "bottom")
   })
}

shinyApp(ui = ui, server = server)


