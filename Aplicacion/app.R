
library(shiny)
library(tidyverse)
library(here)
library(lubridate)
library(rgdal)
library(xtable)

Datos <- read_csv(here("Accidentes_transito_2013a2019.csv"))

datostime <- Datos %>% 
    mutate(Fecha_y_hora = dmy_hm(`Fecha y hora`)) %>% 
    mutate(Ano = year(Fecha_y_hora)) %>% 
    mutate(Mes = month(Fecha_y_hora))

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


tabla <- datostime %>%
    group_by(Departamento,Ano) %>%
    summarise(n=n()) %>%
    mutate(prop = n/sum(n, na.rm = TRUE)) %>%
    rename(Cantidad = n, Proporción = prop)

mapauru <- left_join(deptos, tabla, by = "Departamento")

ui <- fluidPage(
    titlePanel("Trabajo grupal"),
    sidebarLayout(
        sidebarPanel(
            selectInput('year', 'Año en mapa departamental',
                        c("2013", "2014", "2015","2016",
                          "2017","2018","2019") ),
            selectInput('variable', 'Variable en gráfico de barras',
                        c("Rol", "Jurisdiccion", "Vehiculo" ) ),
            selectInput("digitos", "Digitos", c(0, 1, 2))
        ),
    mainPanel(
        tabsetPanel(
            tabPanel("Evolución temporal",
                     h2("Gráfico...", align="center"),
                     plotOutput("tiempo")),
            tabPanel("Mapa departamental",
                     h2("Gráfico...", align="center"),
                     plotOutput("mapa")),
            tabPanel("Rol y Sexo",
                     h2("Gráfico...", align="center"),
                     plotOutput("barras")) ) 
        ) ) )
server <- function(input, output) {
    
    output$mapa <- renderPlot({ mapauru %>%
            filter(Departamento!=15) %>%
            filter(Ano %in% input$year) %>%
            ggplot(aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = Cantidad)) +
            scale_fill_viridis_c() +
            labs(x="Longitud", y="Latitud",
                 fill="Cantidad de accidentes",
                 title="Cantidad de accidentes por departamento")
        
    })
    output$barras <- renderPlot({ Datos %>%
        filter(!is.na(Sexo)) %>%
        group_by(.data[[input$variable]],Sexo) %>%
        summarise(cant =n()) %>%
        ggplot(aes(x = .data[[input$variable]], y = cant, fill = Sexo)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_fill_brewer(palette = "Set1") +
        labs(y = "Cantidad", 
             title = "Cantidad de accidentes",
             subtitle = "Diferenciado por sexo")
    })
}

shinyApp(ui = ui, server = server)


