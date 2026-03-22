#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Call our internal package function
  data <- reactive({
    simulate_mlm_data(
      N = 20,
      C = 30,
      g00 = 2,
      g01 = 5,
      tau2_u0 = input$tau2_00,
      tau2_u1 = input$tau2_11,
      cov_uij.u0j = input$tau2_01,
      sigma2 = input$sigma2
    )
  })

  output$dynamic_formula <- renderUI({
    # Calculate ICC reactively
    icc_res <- reactive({
      calculate_theoretical_icc(input$tau2_00, input$sigma2)
    })

    # Render the text
    output$icc_value <- renderText({
      paste0(icc_res() * 100, "%")
    })

    output$dynamic_formula <- renderUI({
      formula_str <- paste0(
        "$$y_{ij} = 10 + u_{0j} + \\epsilon_{ij}$$",
        "$$\\text{Total Variance} = ",
        input$tau2_00 + input$sigma,
        "$$",
        "$$\\text{ICC} = \\frac{",
        input$tau2_00,
        "}{",
        input$tau2_00,
        " + ",
        input$sigma2,
        "} = ",
        icc_res(),
        "$$"
      )
      withMathJax(helpText(formula_str))
    })

    output$mlm_plot <- renderPlot({
      df <- data()

      base_plot <-
        ggplot2::ggplot(df, ggplot2::aes(x = Cluster, y = y, color = Cluster)) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(y = "Outcome (y)", x = "Groups (j)") +
        ggplot2::scale_color_viridis_d()

      if (input$show_boxplot) {
        base_plot <- base_plot +
          ggplot2::geom_boxplot(
            alpha = 0.3,
            outlier.shape = NA,
            color = "black"
          )
      }

      base_plot
    })
  })
}
