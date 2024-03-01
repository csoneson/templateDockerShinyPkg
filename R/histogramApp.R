#' Generate a small example shiny app
#'
#' @export
#' @returns A shiny app object
#'
#' @param x A numeric vector to create a histogram from.
#' @param outputDir A character scalar pointing to a folder where output
#'     files will be saved.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel
#'     sliderInput mainPanel plotOutput renderPlot observeEvent
#'     actionButton stopApp
#'
histogramApp <- function(x, outputDir) {
    ui <- fluidPage(
        titlePanel("Histogram plotting app"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(inputId = "nbins",
                            label = "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30),
                actionButton("stop", "Stop app"),
                actionButton("export_plot", "Export plot")
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

        observeEvent(input$export_plot, {
            vals$log <- c(vals$log, "Exporting a plot")
            ggsave(plot = histPlot(), device = "png",
                   path = outputDir, filename = paste0("histogram_nbins",
                                                       input$nbins, ".png"))
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
