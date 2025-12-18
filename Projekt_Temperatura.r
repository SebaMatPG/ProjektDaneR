library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

df <- read.csv2("Excel.csv", encoding = "UTF-8")
# Tworzymy wektor z polskimi nazwami miesięcy
polskie_miesiace <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", 
                      "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")

# Zamieniamy numery na nazwy i wymuszamy poprawną kolejność
df$Miesiac <- factor(df$Miesiac, levels = 1:12, labels = polskie_miesiace)
df_num <- df |> select(where(is.numeric))

#tych 4 linijek nie ruszac!
df_num <- df_num |>
  rename_with(~ gsub("\\.", " ", .x))
df <- df |> 
  rename_with(~ gsub("\\.", " ", .x))

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("yvar", "Wybór danych", df_num, selected = "Rain"),
    checkboxGroupInput(
      "months", "Filtrowanie po miesiącach",
      choices = unique(df$Miesiac), 
      selected = unique(df$Miesiac)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("by_months", "Podział na miesiące", TRUE),
    checkboxInput("show_margins", "Pokaż rozkład zmiennych", TRUE),
    checkboxInput("smooth", "Linia trendu", TRUE),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$months)
    df |> filter(Miesiac %in% input$months)
  })
  
  output$scatter <- renderPlot({
    x_var_symbol <- sym("Dzien")
    p <- ggplot(subsetted(), aes(!!x_var_symbol, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_months) aes(color = Miesiac),
      geom_point(),
      if (input$smooth) geom_smooth(),
      scale_x_continuous(breaks = seq(min(subsetted()$`Dzien`), 
      max(subsetted()$`Dzien`), 
      by = 15))
    )
    
    if (input$show_margins) {
      margin_type <- if (input$by_months) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "y",
                               size = 8, groupColour = input$by_months, groupFill = input$by_months)
    }
    
    p
  }, res = 100)
}

shinyApp(ui, server)