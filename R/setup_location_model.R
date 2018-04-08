#' Interactive cell modelling tool
#'
#' Tool to model the perceived signal strength
#'
#' @param param list of parameters
#' @param plot.height heigth of the plots in the tool (in pixels)
#' @import shiny
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @export
setup_location_model <- function(param, plot.height=800) {

    co <- as.data.frame(expand.grid(x=seq(-1000, 2000, by = 20), y=seq(-1000, 1000, by = 20), z=0))

    app <- shinyApp(
        ui = fluidPage(
            #shinyjs::useShinyjs(),
            shiny::div(style = "font-size:75%;line-height:20px",
                       titlePanel("Cell Modelling Tool"),
                       fluidRow(
                           column(3,
                                  wellPanel(
                                      sliderInput("height", "Height", 10, 300, value = 55, step = 5),
                                      sliderInput("tilt", "Tilt", -20, 0, value = -4, step = 1),
                                      checkboxInput("small", "Small cell", value = FALSE)),
                                  conditionalPanel("!input.small",
                                                   wellPanel(
                                                       sliderInput("db0_tower", "dB at source (tower)", -70, -30, value = param$db0_tower, step = 5))),
                                  conditionalPanel("input.small",
                                                   wellPanel(
                                                       sliderInput("db0_small", "dB at source (small)", -70, -30, value = param$db0_small, step = 5))),
                                  conditionalPanel("!input.small",
                                                   wellPanel(
                                                       sliderInput("h3dB", "Horizontal -3dB angle", 20, 70, value = param$delta_min3dB, step = 1),
                                                       #sliderInput("hf", "Dens. in hor. -3dB range", .05, .99, value = param$delta_pmin3dB, step = .01))
                                                       sliderInput("hback", "Hozizontal dB back", -40, -10, value = -30, step = 5))
                                  )
                           ),
                           column(3,
                                  conditionalPanel("!input.small",
                                                   wellPanel(
                                                       sliderInput("v3dB", "Vertical -3dB angle", 4, 20, value = param$epsilon_min3dB, step = 1),
                                                       sliderInput("vback", "Vertical dB back", -40, -10, value = -30, step = 5))),

                                  #sliderInput("vf", "Dens. in vert. -dB range", .05, .99, value = param$epsilon_pmin3dB, step = .01))),
                                  wellPanel(
                                      radioButtons("type", "Output type", choices = c("dB", "likelihood"), selected = "dB"),
                                      checkboxGroupInput("enable", "Enable", choices = c("distance" =  "d", "horizontal beam" = "h", "vertical beam" = "v"), selected = c("d", "h", "v"))),
                                  conditionalPanel("input.type == 'likelihood'",
                                                   wellPanel(
                                                       sliderInput("dbmid", "dB Mid", -100, -80, value = -92.5, step = 2.5),
                                                       sliderInput("dbwidth", "db Width", 1, 20, value = 5, step = 1))),
                                  wellPanel(
                                      checkboxInput("mask", "Enable mask", value = FALSE),
                                      conditionalPanel("input.mask && input.type == 'dB'",
                                                       sliderInput("maskrangedb", "Mask range", min = -130, max = -50, value = c(-110, -50))),
                                      conditionalPanel("input.mask && input.type == 'likelihood'",
                                                       sliderInput("maskrangelh", "Mask range", min = 0, max = 1, value = .8, step = .05)))
                           ),
                           column(6,
                                  plotOutput("heatmap", height=plot.height / 2),
                                  plotOutput("lines", height=plot.height / 2)
                           ))
            )),
        server = function(input, output) {


            output$heatmap <- renderPlot({
                param <- list(db0_tower = input$db0_tower,
                              db0_small = input$db0_small,
                              delta_min3dB = input$h3dB,
                              delta_dB_back = input$hback,
                              epsilon_min3dB = input$v3dB,
                              epsilon_dB_back = input$vback,
                              db_mid = input$dbmid,
                              db_width = input$dbwidth)

                heatmap_ground(co, param, input)
            })


            output$lines <- renderPlot({
                g1 <- distance_plot(db0 = ifelse(input$small, input$db0_small, input$db0_tower))

                g2 <- likelihood_plot(db_mid = input$dbmid, db_width = input$dbwidth)

                g3 <- radiation_plot(type = "a", db_back = input$hback, beam_width = input$h3dB)
                g4 <- radiation_plot(type = "e", db_back = input$vback, beam_width = input$v3dB)

                grid.arrange(g1, g2, g3, g4)
            })

        }
    )

    runApp(app)
}
