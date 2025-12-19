library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

df <- read.csv2("Excel.csv", encoding = "UTF-8")
#tworzymy wektor z polskimi nazwami miesięcy
polskie_miesiace <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", 
                      "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")

#zamieniamy numery na nazwy i wymuszamy poprawną kolejność
df$Miesiac <- factor(df$Miesiac, levels = 1:12, labels = polskie_miesiace)
df_num <- df |> select(where(is.numeric))

#tych 4 linijek nie ruszac!
#zamiana nazw kolumn zeby nie bylo kropek etc...
df_num <- df_num |>
  rename_with(~ gsub("\\.", " ", .x))
df <- df |> 
  rename_with(~ gsub("\\.", " ", .x))

#tworzymy sidebar, wybor osi y
ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("yvar", "Wybór danych", df_num, selected = "Rain"),
    checkboxGroupInput(
      "months", "Filtrowanie po miesiącach",
      choices = unique(df$Miesiac), 
      selected = unique(df$Miesiac)
    ),
    hr(), #horizontal rule
    checkboxInput("by_months", "Podział na miesiące", TRUE),
    checkboxInput("show_margins", "Pokaż rozkład zmiennych", TRUE),
    checkboxInput("smooth", "Linia trendu", TRUE),
  ),
  plotOutput("scatter") #tworzy box na wykres o nazwie scatter
)

server <- function(input, output, session) {
  subsetted <- reactive({ #tworzy reactive expression ktora automatycznie odpala sie gdy zajda zmiany w inpucie
    req(input$months) #przynajmniej 1 miesiac musi byc zaznaczony
    df |> 
      filter(Miesiac %in% input$months) |> #filtruje po miesiacach
      droplevels() #usuwa niepotrzebne month labels aby wyswietlaly sie nazwy miesiecy a nie liczby
  })
  
  output$scatter <- renderPlot({ #tworzy wykres
    x_var_symbol <- sym("Dzien") #dzien zawsze na osi X
    p <- ggplot(subsetted(), aes(!!x_var_symbol, !!input$yvar)) + list(
      theme(legend.position = "bottom"), #wykres + legenda
      if (input$by_months) aes(color = Miesiac), #kolorowe filtrowanie po miesiacach GDY ZAZNACZONE PRZEZ USERA
      geom_point(),
      if (input$smooth) geom_smooth(), #linia trendu GDY ZAZNACZONE PRZEZ USERA
      scale_x_continuous(breaks = seq(min(subsetted()$`Dzien`), #os x podzielona co 15 dni
      max(subsetted()$`Dzien`), 
      by = 15))
    )
    
    if (input$show_margins) { #generuje wykres na marginesie GDY ZAZNACZONE PRZEZ USERA
      margin_type <- if (input$by_months) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "y",
      size = 8, groupColour = input$by_months, groupFill = input$by_months)
    }
    
    p #funkcja zwraca wykres, inaczej return p
    
  }, res = 100) #resolucja wykresu
}

shinyApp(ui, server) #wszystko laczy i odpala