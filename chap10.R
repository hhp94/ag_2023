library(shiny)
library(dplyr, warn.conflicts = FALSE)

ui <- fluidPage(
  textInput("label", "label"),
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)

server <- function(input, output, session) {
  output$numeric <- renderUI({
    value <- isolate(input$dynamic) %||% 0
    if (input$type == "slider") {
      sliderInput("dynamic", input$label, value = value, min = 0, max = 10)
    } else {
      numericInput("dynamic", input$label, value = value, min = 0, max = 10)
    }
  })
}


shinyApp(ui, server)
library(purrr)

ui <- fluidPage(
  numericInput("n", "Number of colours", value = 5, min = 1),
  uiOutput("col"),
  textOutput("palette")
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))

  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL))
  })

  output$palette <- renderText({
    map_chr(col_names(), ~ input[[.x]] %||% "")
  })
}

shinyApp(ui, server)

#
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of colours", value = 5, min = 1),
      uiOutput("col"),
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  col_names <- reactive(paste0("col", seq_len(input$n)))

  output$col <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = isolate(input[[.x]])))
  })

  output$plot <- renderPlot(
    {
      cols <- map_chr(col_names(), ~ input[[.x]] %||% "")
      # convert empty inputs to transparent
      cols[cols == ""] <- NA

      barplot(
        rep(1, length(cols)),
        col = cols,
        space = 0,
        axes = FALSE
      )
    },
    res = 96
  )
}

shinyApp(ui, server)

stringr::str_to_upper

map(names(iris), ~ make_ui(iris[[.x]], .x, stringr::str_to_upper))

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_ui(iris[[.x]], .x))
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, ~ .x & .y)
  })

  output$data <- renderTable(head(iris[selected(), ], 12))
}
