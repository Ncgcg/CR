
library(shiny)



# Define UI for application
ui <- fluidPage( 
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("ГЕНЕТИКА Модуль 2"),
  navlistPanel(
    tabPanel("Землекопи",
             "Ген А відповідає за забарвлення хвоста голого землекопа, 
             має 2 алеля, спадкується за типом повного домінування. 
             Окрім міграціїі мутації на популяцію не діють 
             інші фактори динаміки популяції.",
             numericInput("A0","Частка алелю А:",
                          min = 0, max = 1, value = 0, step = 0.01),
             numericInput("v", "Частота прямих мутацій:",
                          min = 0, max = 0.1, value = 0, step = 0.0001),
             numericInput("u", "Частота зворотніх мутацій:",
                          min = 0, max = 0.1, value = 0, step = 0.0001),
             numericInput("m", "Частка іммігрованих особин:",
                          min = 0, max = 1, value = 0, step = 0.001),
             numericInput("Ad", "Частота алелю А в другій популяції:",
                          min = 0, max = 1, value = 0, step = 0.01),
             textOutput("zc")
    ),
    tabPanel("X-Хромосома",
             "Хвороба визначаєтсья геном локалізованим в Х-хромосомі.",
             numericInput("ho","Пенетрантність у гомо- та гемізигот (%):",
                          min = 0, max = 100, value = 0, step = 1),
             numericInput("he", "Пенетрантність у гетерозигот (%):",
                          min = 0, max = 100, value = 0, step = 1),
             numericInput("p", "Хворих чоловіків в популяції (%):",
                          min = 0, max = 100, value = 0, step = 1),
             textOutput("chro")
    ),
    tabPanel("ABO",
             "Частоти генів груп крові АВО серед росіян за Н.П.Бочковським 
             (1979):",
             
             numericInput("A","алелю А: ",
                          min = 0, max = 1, value = 0, step = 0.01),
             numericInput("B", "алелю В: ",
                          min = 0, max = 1, value = 0, step = 0.01),
             textOutput("ABO")
    ),
    tabPanel("3 Алелі",
             "В популяції ген існує у вигляді трьох алелів,",
             numericInput("a1","частота алелю а1:",
                          min = 0, max = 1, value = 0, step = 0.01),
             numericInput("a2", "алелю а2:",
                          min = 0, max = 1, value = 0, step = 0.01),
             numericInput("a3", "алелю а3:",
                          min = 0, max = 1, value = 0, step = 0.01),
             textOutput("tal")
    ),
    tabPanel("Гемофіліки",
             "В рівноважній популяції",
             numericInput("h","частота гемофілії в чоловіків:",
                          min = 0, max = 1, value = 0, step = 0.01),
             textOutput("hem")
    ),
    tabPanel("Група особин",
             "Група особин складається з",
             numericInput("AA","AA (%): ",
                          min = 0, max = 100, value = 0, step = 1),
             numericInput("Aa","Aa (%): ",
                          min = 0, max = 100, value = 0, step = 1),
             textOutput("gr")
    ),
    tabPanel("Близнюки",
             "Конкордантність для",
             numericInput("mo","монозиготних близнюків (%): ",
                          min = 0, max = 100, value = 0, step = 1),
             numericInput("di","дизиготних близнюків (%): ",
                          min = 0, max = 100, value = 0, step = 1),
             textOutput("tub")
    )
  )
)

# Define server logic
server <- function(input, output){
  output$zc <- renderText({
    Am <- (1-input$m)*(input$A0)+(input$m)*(input$Ad)
    A1 <- Am-(input$v)*Am+(input$u)*(1-Am)
    paste("Частка алелю А в першій популяції через рік — ", A1)
  })
  output$chro <- renderText({
    p <- (input$p/100)/(input$ho/100)
    paste("Частота захворювання серед жінок — ", 
          (input$ho/100)*p^2+2*p*(1-p)*(input$he/100))
  })
  output$ABO <- renderText({
    i <- 1-input$A-input$B
    paste('I —', i^2, '; II —', input$A^2+2*input$A*i, '; III —', 
          input$B^2+2*input$B*i, '; IV —', 2*input$A*input$B)
  })
  output$tal <- renderText({
    paste('Частота гетерозигот в популяції —', 
          2*input$a1*input$a2+2*input$a1*input$a3+2*input$a2*input$a3)
  })
  output$hem <- renderText({
    paste('Частоти генотипів серед жінок (HH:Hh:hh) — ', 
          (1-input$h)^2, ':', 2*(1-input$h)*input$h, ':', input$h^2)
  })
  output$gr <- renderText({
    paste('Частота алелю A —', (2*input$AA+input$Aa)/200, 'та a —', 
          1-(2*input$AA+input$Aa)/200)
  })
  output$tub <- renderText({
    paste('Коефіцієнт успадковуваності —', (input$mo-input$di)/(100-input$di))
  })
}




# Run the application 
shinyApp(ui = ui, server = server)
