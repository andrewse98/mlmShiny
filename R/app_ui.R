#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    withMathJax(),

    fluidPage(
      titlePanel("MLM Error Simulator"),
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            "tau2_00",
            "Level-2 random intercept variance (\u03c4\u00b2\u2080\u2080):",
            min = 0,
            max = 10,
            value = 5
          ),
          sliderInput(
            "tau2_11",
            "Level-2 random slope variance (\u03c4\u00b2\u2081\u2081):",
            min = 0,
            max = 10,
            value = 3
          ),
          sliderInput(
            "tau2_01",
            "Level-2 covariance between random intercept and random slope (\u03c4\u00b2\u2080\u2081):",
            min = 0,
            max = 10,
            value = 1
          ),
          sliderInput(
            "sigma2",
            "Level-1 variance (\u03c3\u00b2):",
            min = 0,
            max = 10,
            value = 3
          ),
          checkboxInput(
            inputId = "show_boxplot",
            label = "Show Boxplots (Group Distributions)",
            value = TRUE
          )
        ),

        mainPanel(
          uiOutput("dynamic_formula"),
          plotOutput("mlm_plot"),
          wellPanel(
            h4("Intraclass Correlation (ICC)"),
            tags$p("Proportion of variance explained by groups:"),
            textOutput("icc_value")
          )
        ),
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mlmShiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
