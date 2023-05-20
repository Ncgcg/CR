library(shiny)

# Define UI for application
ui <- fluidPage( 
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel("ГЕНЕТИКА Модуль 1"),
    navlistPanel(
        tabPanel("Землекопи",
            "Гени А та В в домінантній формі відповідають за 
            червоне забарвлення шкіри голого землекопа. 
            Взаємодіють за типом подвійного рецесивного епістазу.
            Вкажіть частку особин забарвлених в червоний від 
            схрещування F2 ААВВ х аавв.",
            numericInput("p","Пенетрантність (%):",
                min = 0, max = 100, value = 0, step = 1),
            numericInput("c", "Відстань (сМ):",
                min = 0, max = 50, value = 0, step = 1),
            textOutput("zc")
            ),
        tabPanel("Фенотипові класи", 
            "Скільки фенотипових класів 
            очікується від самозапилення гетерозиготи,  
            якщо ознаки спадкується наструпним чином:",
            numericInput("d","з повним домінуванням",
                min = 0, max = 10, value = 0, step = 1),
            numericInput("n", "з неповним домінуванням",
                min = 0, max = 10, value = 0, step = 1),
            numericInput("k","з кодомінуванням",
                min = 0, max = 10, value = 0, step = 1),
            textOutput("ph")
        ),
        tabPanel("Подагра", 
            "Подагра визначається домінантним аутосомним 
            геном. Яка ймовірність захворювання дітей, якщо
            батьки гетерезиготні? Пенетрантність гену:",
            numericInput("m","у чоловіків (%)",
                min = 0, max = 100, value = 0, step = 1),
            numericInput("f", "у жінок (%)",
                min = 0, max = 100, value = 0, step = 1),
            textOutput("pod")),
        tabPanel("Шизофренія", 
            "Деякі форми шизофреніївизначаються домінантним аутосомним 
            геном. Яка ймовірність захворіти у дітини, якщо
            батьки гетерезиготні? Пенетрантність гену:",
            numericInput("go","у гомозигот (%)",
                min = 0, max = 100, value = 100, step = 1),
            numericInput("ge", "у гетерозигот (%)",
                min = 0, max = 100, value = 0, step = 1),
            textOutput("sci")),
        tabPanel("Дигомозигота", 
                 "Яка ймовірність отримати в F2 дигомозиготу за рецисивом, 
                 якщо:",
            numericInput("v","відстань між генами (сМ)",
                min = 0, max = 50, value = 0, step = 1),
            radioButtons("cr", "проводилось схрещування:", 
                choices = c("аавв х ААВВ", "ааВВ х ААвв")),
            textOutput("dif"))
  )
)

# Define server logic
server <- function(input, output){
    output$zc <- renderText({
        paste("Частка особин забарвлених в червоний колір — ", 
            (0.5-input$c/200)^2+
            (0.5-input$c/200)^2*2*(input$p/100)+
            (input$c/200)*(0.5-input$c/200)*2+
            (input$c/200)*(0.5-input$c/200)*2*(input$p/100)+
            (input$c/200)^2*(input$p/100)*2)
    })
    output$ph <- renderText({
        paste("Кількість фенотипових класів — ", 
            2^(input$d)*3^(input$n)*3^(input$k))
    })
    output$pod <- renderText({
        paste("Ймовірність захворювання дітей — ", 
            0.375*(input$m)+0.375*(input$f), "%")
    })
    output$sci <- renderText({
        paste("Ймовірність дитини захворіти — ", 
            0.25*(input$go)+0.5*(input$ge), "%")
    })
    output$dif <- renderText({
      if (input$cr == "аавв х ААВВ"){
        paste("Ймовірність отримати дигомозиготу — ", 
              ((100-input$v)/200)^2, "%")
      } else if (input$cr == "ааВВ х ААвв"){
        paste("Ймовірність отримати дигомозиготу — ", 
              (input$v/200)^2, "%")
      } 
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# shinyShortcut::shinyShortcut(shinyDirectory = getwd(), 
#      OS = .Platform$OS.type, gitIgnore = FALSE)
