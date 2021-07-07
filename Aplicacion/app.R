
library(shiny)
library(tidyverse)
library(here)
library(lubridate)
library(rgdal)
library(xtable)
library(DT)

Datos <- read_csv(here("Accidentes_transito_2013a2019.csv"))

mapaine <- readOGR(here("ine_depto.shp"))

dframe_depto <- ggplot2::fortify(mapaine)

deptos <- dframe_depto %>%
    rename(Departamento = id) %>%
    mutate(Departamento=recode(Departamento, `0` = "MONTEVIDEO",
                               `8` = "RIO NEGRO", `9` = "RIVERA",`7`="PAYSANDU",
                               `1`="ARTIGAS",`2`="CANELONES",`19`="CERRO LARGO",
                               `3`="COLONIA",`4`="DURAZNO",`5`="FLORIDA",`6`="LAVALLEJA",
                               `10`="ROCHA",`11`="SALTO", `12`="SAN JOSE",`13`="SORIANO",
                               `14`="TREINTA Y TRES", `16`="TACUAREMBO",`17`="FLORES",
                               `18`="MALDONADO"))
datostime <- Datos %>%
    mutate(Fecha_y_hora = dmy_hm(`Fecha y hora`)) %>% 
    mutate(Ano = year(Fecha_y_hora)) %>% 
    mutate(Mes = month(Fecha_y_hora))

ui <- fluidPage(
    titlePanel("Trabajo grupal"),
    sidebarLayout(
        sidebarPanel(
            selectInput("anios", "Año del gráfico ",
                        c(2013, 2014, 2015, 2016, 2017, 2018, 2019)),
            selectInput("digitos", "Digitos", c(1, 2, 3)),
            selectInput('variable', 'Variable en gráfico de barras',
                        c("Rol", "Jurisdiccion", "Vehiculo" ) )
        ),
    mainPanel(
        tabsetPanel(
            tabPanel("Evolución temporal",
                     h2("Gráfico con la evolución temporal por mes de cada año", align="center"),
                     plotOutput("tiempo")),
            tabPanel("Mapa departamental",
                     h2("Mapa departamental con la cantidad de accidentes por departamento y año", align="center"),
                     plotOutput("mapa"),
                     DTOutput("tabla")),
            tabPanel("Barras",
                     h2("Comparación de distintas variables por sexo ", align="center"),
                     plotOutput("barras")) ) 
        ) ) )
server <- function(input, output) {
    
    grafico <- reactive({Datos %>% 
            mutate(Fecha_y_hora = dmy_hm(`Fecha y hora`)) %>% 
            mutate(Ano = year(Fecha_y_hora)) %>% 
            mutate(Mes = month(Fecha_y_hora)) %>% 
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
                 title = paste("Evolución temporal de la cantidad de accidentes en el año", input$anios))
    })
    
       tabla <- reactive({ datostime %>%
        group_by(Departamento,Ano) %>%
        summarise(n=n()) %>%
        mutate(prop = n/sum(n, na.rm = TRUE)) %>%
        rename(Cantidad = n, Proporción = prop) %>%
        filter(Ano == input$anios)
    })
    mapauru <- reactive({left_join(deptos,
                                   tabla(), by = "Departamento")
    })
    output$mapa <- renderPlot({ mapauru() %>%
            filter(Departamento!=15) %>%
            ggplot(aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = Cantidad)) +
            scale_fill_viridis_c() +
            labs(x="Longitud", y="Latitud",
                 fill="Cantidad de accidentes",
                 title=paste("Cantidad de accidentes por departamento en el año",
                 input$anios))
        
    })
    
    tablas <- reactive({datatable(datostime %>%
                                      filter(Ano == input$anios) %>%
                                      group_by(Departamento) %>%
                                      summarise(n=n()) %>%
                                      mutate(prop = n/sum(n, na.rm = TRUE),
                                             across(where(is.numeric), ~ round(., as.numeric(input$digitos)))) %>%
                                      rename(Cantidad = n, Proporción = prop))
    })
    output$tabla <- renderDataTable({
        tablas()
    })
    
    output$barras <- renderPlot({ datostime %>%
        filter(!is.na(Sexo)) %>%
        filter(Ano == input$anios) %>%
        group_by(.data[[input$variable]],Sexo) %>%
        summarise(cant =n()) %>%
        ggplot(aes(x = .data[[input$variable]], y = cant, fill = Sexo)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_fill_brewer(palette = "Set1") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(y = "Cantidad", 
             title = paste("Cantidad de accidentes en el año", input$anios),
             subtitle = "Diferenciado por sexo")
    })
    
}

shinyApp(ui = ui, server = server)


