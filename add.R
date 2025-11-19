library(shiny)
library(wordcloud2)

ui <- fluidPage(
  wordcloud2Output("wc")
)

server <- function(input, output, session) {
  output$wc <- renderWordcloud2({
    wordcloud2(
      data = data.frame(
        word = c("A", "B", "C"),
        freq = c(10, 20, 30)
      )
    )
  })
}

shinyApp(ui, server)
