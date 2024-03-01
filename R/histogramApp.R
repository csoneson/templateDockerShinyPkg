#' Generate a small example shiny app
#'
#' @export
#' @returns A shiny app object
#'
#' @param x A numeric vector to create a histogram from.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel
#'     sliderInput mainPanel plotOutput renderPlot observeEvent
#'     actionButton stopApp reactiveValues reactive isolate
#' @importFrom ggplot2 ggplot aes geom_histogram labs ggsave
#'
histogramApp <- function(x) {
    stopifnot(is.numeric(x))

    ui <- fluidPage(
        titlePanel("Histogram plotting app"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(inputId = "nbins",
                            label = "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30),
                actionButton("stop", "Stop app")
            ),
            mainPanel(
                plotOutput(outputId = "distPlot")
            )
        )
    )

    server <- function(input, output, session) {
        vals <- reactiveValues(
            log = c("Start of log")
        )

        observeEvent(input$nbins, {
            vals$log <- c(vals$log,
                          paste0("Setting nbins to ", input$nbins))
        })

        histPlot <- reactive({
            bins <- seq(min(x), max(x), length.out = input$nbins + 1)
            isolate({vals$log <- c(vals$log, "Plotting")})
            ggplot(data.frame(x = x), aes(x = x)) +
                geom_histogram(breaks = bins, fill = "#75AADB",
                               color = "white") +
                labs(x = "Value",
                     title = "Histogram")
        })
        output$distPlot <- renderPlot({
            histPlot()
        })

        observeEvent(input$stop, {
            vals$log <- c(vals$log, "Stopping the app")
            stopApp(returnValue = list(nbins = input$nbins,
                                       log = vals$log,
                                       plot = histPlot()))
        })
    }

    shiny::shinyApp(ui = ui, server = server)
}
