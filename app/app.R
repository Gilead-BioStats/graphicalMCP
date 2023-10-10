library(shiny)
library(igraph)
library(graphicalMCP)

ui <- fluidPage(

  fluidRow(

    titlePanel("Initial graph"),

    column(5, plotOutput("plot_initial_graph")),

    column(
      7,
      numericInput(
        "num_hyps",
        "Number of hypotheses",
        value = 4,
        min = 1,
        max = 50,
        step = 1
      ),
      shinyMatrix::matrixInput(
        "hypotheses",
        "Hypothesis weights",
        value = structure(
          matrix(0, 1, 1),
          dimnames = list(NULL, "H1")
        ),
        class = "numeric",
        rows = list(
          n = 1,
          names = FALSE
        ),
        cols = list(
          n = 1,
          names = TRUE
        )
      ),
      shinyMatrix::matrixInput(
        "transitions",
        "Transition weights",
        value = structure(
          matrix(0, 1, 1),
          dimnames = list("H1", "H1")
        ),
        class = "numeric",
        rows = list(
          n = 1,
          names = TRUE
        ),
        cols = list(
          n = 1,
          names = TRUE
        )
      )
    )
  ),

  hr(),

  fluidRow(

    titlePanel("Test the graph"),

    column(5, plotOutput("plot_updated_graph")),

    column(
      7,

      shinyMatrix::matrixInput(
        "p",
        "P-values",
        value = structure(
          matrix(1, 1, 1),
          dimnames = list(NULL, "H1")
        ),
        class = "numeric",
        rows = list(
          n = 1,
          names = FALSE
        ),
        cols = list(
          n = 1,
          names = TRUE
        )
      ),

      selectInput(
        "test_type",
        "Test type",
        c("Bonferroni", "Parametric", "Simes"),
        selected = "Bonferroni"
      )

    )

  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  global <- reactiveValues(
      num_hyp_diff = 0,
      seq_hyps = 1
  )

  observe({
    global$num_hyp_diff <- input$num_hyps - length(input$hypotheses)
    global$seq_hyps <- seq_len(input$num_hyps)
  })

  # Update hypothesis weights
  observe({
    if (global$num_hyp_diff >= 0) {
      new_hypotheses <- c(input$hypotheses, rep(0, global$num_hyp_diff))
    } else {
      new_hypotheses <- input$hypotheses[global$seq_hyps]
    }

    new_hypotheses <- matrix(
      new_hypotheses,
      nrow = 1,
      ncol = input$num_hyps
    )

    dimnames(new_hypotheses) <-
      list(NULL, paste0("H", global$seq_hyps))

    shinyMatrix::updateMatrixInput(session, "hypotheses", new_hypotheses)
  })

  # Update transition weights
  observe({
    if (global$num_hyp_diff >= 0) {
      new_transitions <- rbind(
        input$transitions,
        matrix(0, global$num_hyp_diff, ncol(input$transitions))
      )

      new_transitions <- cbind(
        new_transitions,
        matrix(0, nrow(new_transitions), global$num_hyp_diff)
      )
    } else {
      new_transitions <-
        input$transitions[global$seq_hyps, global$seq_hyps, drop = FALSE]
    }

    dimnames(new_transitions) <- list(
      paste0("H", global$seq_hyps),
      paste0("H", global$seq_hyps)
    )

    shinyMatrix::updateMatrixInput(session, "transitions", new_transitions)
  })

  # Update p-values
  observe({
    if (global$num_hyp_diff >= 0) {
      new_p <- c(input$p, rep(1, global$num_hyp_diff))
    } else {
      new_p <- input$p[global$seq_hyps]
    }

    new_p <- matrix(
      new_p,
      nrow = 1,
      ncol = input$num_hyps
    )

    dimnames(new_p) <-
      list(NULL, paste0("H", global$seq_hyps))

    shinyMatrix::updateMatrixInput(session, "p", new_p)
  })

  initial_graph <- reactive({
    clean_hypotheses <- input$hypotheses
    clean_hypotheses[is.na(clean_hypotheses)] <- 0

    clean_transitions <- input$transitions
    clean_transitions[is.na(clean_transitions)] <- 0

    graph <- graph_create(clean_hypotheses, clean_transitions)
  })

  output$plot_initial_graph <- renderPlot({
    plot(initial_graph())
  })

  test_results <- reactive({
    graph_test_closure(initial_graph(), input$p, test_types = input$test_type)
  })

  output$plot_updated_graph <- renderPlot({
    plot(graph_update(initial_graph(), test_results()$outputs$rejected))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
