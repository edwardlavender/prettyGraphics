#' @title Visualise one dimensional time series via R Shiny
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#'
#' @description Interactive visualisation and exploration of one dimensional time series, based on \code{\link[prettyGraphics]{pretty_ts}}.
#'
#' @param data A dataframe containing a response variable, timestamps (and, optionally) explanatory variables. Timestamps can be integer/numeric, \code{\link[base]{DateTimeClasses}} or a \code{\link[base]{Date}} objects.
#'
#' @return An interactive R Shiny application for visualising time series.
#'
#' @examples
#' \dontrun{
#' #### Example (1) Visualising POSIXct time series
#' # Here we have a simple dataframe... other columns (i.e. covariates) can be included
#' # ... alongside factor columns.
#' x <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"),
#'                 as.POSIXct("2016-01-10", tz = "UTC"), by = "2 hours")
#'                 y1 <- rnorm(length(x), 200, 25) *-1
#'                 y2 <- rnorm(length(x), lubridate::yday(x) * 0.5 +20, 0.5)
#' dat <- data.frame(x = x, y1 = y1, y2 = y2)
#' #### Launch shiny
#' vis_ts(data = dat)
#'}
#'
#'\dontrun{
#'#### Example (2) Visualising numeric time series
#' x <- 1:1000
#' y <- rnorm(length(x), x*0.1- 500, 1000)
#' y2 <- rnorm(length(x), x*0.5, 500)
#' vis_ts(data = data.frame(x = x, y = y, y2 = y2))
#'}
#'
#' @author Edward Lavender
#' @export
#'

################################################
################################################
#### vis_ts()

vis_ts <- function(data){


################################################
################################################
#### Preprocessing

# Checks
check_input_class(input = data,
                  to_class = "data.frame",
                  type = "stop")

# Define colnames
colnames_data <- colnames(data)

# Timestamp column
# Try to get the timestamp using POSIXct
col_ts <- colnames(data)[
  which(sapply(data, class) %>% unlist() %>% as.vector()
        %in% c("POSIXct", "Date"))[1]]
# But the timestamp could be a numeric value, so we'll allow for this possibility.
if(is.null(col_ts)){
  col_ts <- colnames(data)[1]
}

# Numeric columns:
coln_numeric <- which(sapply(data, class) %in% c("numeric", "integer"))
colnames_numeric <- colnames_data[coln_numeric]

# Factor columns:
coln_fct <- which(sapply(data, class) %in% c("factor", "character"))
colnames_factor <- colnames_data[coln_fct]


################################################
################################################
#### User Inferace

# Define the user interface
ui <-

  # Open up a dashboardPage
  dashboardPage(

    # Define dashboard Header:
    dashboardHeader(
      title = "Visualise Time Series",
      titleWidth = 250
      ),



    ################################################
    ################################################
    #### Define sidebar

    # Define dashboardSidebar
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        menuItem("Plot", tabName = "plot_timeseries", icon = icon("angle-right")),
        menuItem("Background", tabName = "introduction", icon = icon("angle-right"))
      ) # close sidebarMenu(
    ), # close dashboardSidebar(



    ################################################
    ################################################
    #### Define dashbar body for each table in the sidebar

    # Open dashboardBody
    dashboardBody(
      # This will consist of a number of tab items...
      tabItems(


        ################################################
        #### Plot Options ("plot_options")

        tabItem(
          tabName = "plot_timeseries",
          # Define a title for the page:
          h1("visTS: An Interactive Application to Visualise One-Dimensional Time Series"),

          #### Open a fluidPage...
          # In the first part ("row") of the page,
          # ... we'll have three boxes along the top (i.e. different columns),
          # ... defining plot options; then, immediately below that, we'll have the plot.
          fluidPage(
            title = "Visualise Time Series",

            #### Open a fluid for defining some boxes with input options for the plot...
            fluidRow(
              width = 12,



              ################################################
              #### Define the variables for plotting

              # In the first box, define the variables for plotting...
              # Define column:
              column(
                width = 3,

                # Define box with width = NULL so that this is inherited from the column...
                box(
                  width = NULL,
                  # Define box title:
                  h2("Define the variables to be plotted"),

                  #### Define whether or not to plot data for a factor level
                  checkboxInput(inputId = "fct_yn",
                                label = strong("Visualise the time series for a specific factor level."),
                                value = FALSE),
                  conditionalPanel(condition = "input.fct_yn == true",
                                   #Define column containing individuals
                                   selectInput(inputId = "id_column",
                                               label = strong("Define the column containing factor levels of interest."),
                                               choices = colnames_factor),
                                   # Define the individual for which data will be plotted using the appropriate column:
                                   uiOutput("id_options")
                                   # close condition = "input.variable2_yn == true"
                  ),

                  #### Define timestamp column
                  selectInput(inputId = "timestamp_column",
                              label = strong("Define the column containing timestamps."),
                              choices = colnames_data,
                              selected = col_ts),

                  #### Define repsonse variable
                  # Define the 1st response variable
                  uiOutput("variable_choices"),

                  #### Define line type
                  radioButtons(inputId = "line_type",
                               label = strong("Define the line type."),
                               choices = c("l", "b", "p"),
                               selected = "l"),

                   #### Define second response variable
                   # Choose whether or not to add a second variable
                   # Define second variable
                   # Define how you would like to add the second variable (as a new axis or by colour):
                   # Select whether you would like to add a second variable to the time series
                   checkboxInput(inputId = "variable2_yn",
                                 label = strong("Add a second variable to the time series."),
                                 value = FALSE),
                  conditionalPanel(condition = "input.variable2_yn == true",
                                   uiOutput("variable2_choices"),
                                   radioButtons(inputId = "variable2_method",
                                                label = strong("How would you like to input the second variable?"),
                                                choiceNames = c("Colour the time series line by the inputted variable.",
                                                                "Add a second y axis and overlay the time series of the second variable."),
                                                choiceValues = c("by_colour", "by_new_axis"),
                                                inline = T)
                                   # close condition = "input.variable2_yn == true"
                                   )
                  ) # close box( for "Define the variables to be plotted
                ), # close column


              ################################################
              #### Plot customisation

              # In the next plot, define options for customisation...
              column(
                width = 3,
                box(
                  width = NULL,
                  h2("Define axes properties"),

                  #### Define a background gird
                  checkboxInput(inputId = "add_grid",
                                label = strong("Include a background grid."),
                                value = TRUE),
                  conditionalPanel(condition = "input.add_grid",
                                   textInput(inputId = "grid_col",
                                             label = strong("Define the colour of the grid lines."),
                                             value = "lightgrey"
                                             ),
                                   radioButtons(inputId = "grid_lty",
                                                label = strong("Define the grid line type."),
                                                choices = 1:6,
                                                selected = 3,
                                                inline = TRUE
                                                ),
                                   numericInput(inputId = "grid_lwd",
                                                label = strong("Define the width of the grid lines."),
                                                value = 1)
                                   ),

                  #### Define axis sides
                  radioButtons(inputId = "define_x_side",
                               label = strong("Define a side for the x axis."),
                               choices = c(1, 3),
                               selected = 1,
                               inline = TRUE),

                  #### Define the time range
                  checkboxInput(inputId = "adjust_time_range",
                                label = strong("Adjust the time range."),
                                value = FALSE),

                  # Define date range
                  # ( If NULL, then they are chosen automatically using the range of the data )
                  conditionalPanel(condition = "input.adjust_time_range == true",
                                   uiOutput("date_range"),
                                   uiOutput("time_range"),
                                   ),
                  #### Finer scale time adjustments with slider input

                  #### Define range of y1
                  checkboxInput(inputId = "adjust_var_range",
                                label = strong("Adjust the range of the 1st repsonse variable."),
                                value = FALSE),
                  conditionalPanel(condition = "input.adjust_var_range == true",
                                   uiOutput("var_range")
                  ),

                  #### Define range of second y variable
                  conditionalPanel(condition = "input.variable2_yn == true && input.variable2_method == 'by_new_axis'",
                                   checkboxInput(inputId = "adjust_var2_range",
                                                 label = strong("Adjust the range of the 2nd repsonse variable."),
                                                 value = FALSE),
                                   conditionalPanel(condition = "input.adjust_var2_range == true",
                                                    uiOutput("var2_range")
                                   )
                  ), # close conditionalPanel(condition = "input.variable2_yn == true",

                  #### Adjust the position of the colour bar (if applicable)
                  conditionalPanel(condition = "input.variable2_yn == true && input.variable2_method == 'by_colour'",
                                   uiOutput("subplot_x"),
                                   uiOutput("subplot_y"),
                                   uiOutput("subplot_size")
                                   )

                  ) # close box
                ), # close column for plot customisation options


              ################################################
              #### Add summaries

              # Define a new column/box with options for adding summaries to the data...
              column(
                width = 3,
                box(
                  width = NULL,
                  h2("Define inferential aids"),

                  #### Shading e.g. diel summaries
                  # Add a check box to say whether you want to add shading for
                  # ... different day/night
                  checkboxInput(inputId = "add_shading",
                                label = strong("Add shading."),
                                value = FALSE),
                  conditionalPanel(condition = "input.add_shading == true",
                                   radioButtons(inputId = "shading_type",
                                                label = "Define the shading type",
                                                choices = c("diel", "season"),
                                                selected = "diel"),
                                   numericInput(inputId = "shading_long",
                                                label = strong("Define the longitude (DD) of the location."),
                                                value = 5.469723),
                                   numericInput(inputId = "shading_lat",
                                                label = strong("Define the latitude (DD) of the location."),
                                                value = 56.41041)
                                   ),

                  #### Add statistical summaries
                  # Choose whether or not to add statistical summaries to the plot:
                  checkboxInput(inputId = "add_summary",
                                label = strong("Add statistical summaries to the time series."),
                                value = FALSE),
                  # If statistical summaries are to be added...
                  conditionalPanel(condition = "input.add_summary == true",
                                  # Define all the functions that can be used to summarise the time series:
                                  pickerInput(inputId = "funs",
                                              label = strong("Define the statistical summaries you would like to add to the plot."),
                                              choices = c("mean", "median", "min", "max", "custom"),
                                              selected = list("mean"),
                                              options = list(`actions-box` = TRUE,
                                                             size = 10,
                                                            `selected-text-format` = "count > 3"),
                                              multiple = TRUE),
                                  # Add custom input
                                  conditionalPanel(condition = "input.funs .includes('custom')",
                                                   textInput(inputId = "funs_custom",
                                                             label = "Write custom function(s) in a list",
                                                             value = "list(function(x) mean(x) - sd(x), function(x) mean(x) + sd(x))"
                                                             )
                                                   ),
                                  # Define the time window over which to apply the selected functions
                                  # Intelligently guess the selected = ...
                                  # ... could be implemented here via difftime on dataframe in question.
                                  uiOutput( "interval")
                                 ) # close conditionalPanel(condition = "input.add_summary == 'true'"

                  ) # close box for adding statistical summaries
                ), # close column for adding statistical summaries

              ################################################
              #### Add plot customisation options

              column(
                width = 3,
                box(
                  width = NULL,
                  h2("Plot customisation options"),

                  #### Define plotting window
                  textInput(inputId = "par_oma",
                                 label = strong("Enter a comma delimited vector of numbers (a, b, c, d) for graphics::par(oma = c(a, b, c, d)) to set the plotting window."),
                            value = c("2, 2, 2, 4")
                            ),

                  #### Define axes labels
                  # x axis:
                  textInput(inputId = "xlab",
                            label = strong("Define the x axis label."),
                            value = "Time"),

                  # y axis
                  uiOutput("ylab1_default"),
                  # second y axis:
                  conditionalPanel(condition = "input.variable2_yn == true",
                                   uiOutput("ylab2_default")
                                  ),

                  #### Define axes label positions
                  # x axis
                  numericInput(inputId = "xlab_line",
                              label = strong("Define the number of lines of the x axis variable label from the x axis."),
                              value = 3),
                  # y axis
                  numericInput(inputId = "ylab1_line",
                               label = strong("Define the number of lines of the response variable label from the y axis."),
                               value = 3),
                  # y2 axis
                  conditionalPanel(condition = "input.variable2_yn == true",
                                   numericInput(inputId = "ylab2_line",
                                                label = strong("Define the number of lines of second response variable label from the second yaxis."),
                                                value = 2)
                                   ),

                  # download plot button
                  checkboxInput(inputId = "download_plot_checkbox",
                                label = strong("Download a high resolution .png of the plot.")
                                ),
                  conditionalPanel(condition = "input.download_plot_checkbox == true",
                                   numericInput(inputId = "height",
                                                label = strong("Define the height of the image in inches."),
                                                value = 6
                                                ),
                                   numericInput(inputId = "width",
                                                label = strong("Define the width of the image in inches."),
                                                value = 10
                                                ),
                                   numericInput(inputId = "res",
                                                label = strong("Define the nominal resolution (ppi)."),
                                                value = 600
                                                ),
                                   downloadButton(outputId = "download_plot",
                                                  label = strong("Download the .png.")
                                                  )
                                  ) #, # close conditionalPanel(condition = "download_plot_checkbox == true"

                  # textOutput(outputId= "test")

                  ) # close box (plot customisation options)
                ) # close column (box customisation options )

              ), # close first fluidRow (i.e. for boxes defining all plot options)


            ################################################
            ################################################
            #### The plot

            # Add a new fluidRow that will contain the plot constructued:
            fluidRow(width = 9,
              plotOutput(outputId = "plot_timeseries",
                         width = "90%",
                         height = "550px")
                ) # close fluidRow

            ) # close fluidPage
        ), # close TabItem for tabName = "plot_timeseries",


        ################################################
        #### Introduction ("introduction")

        tabItem(
          tabName = "introduction",
          # Define a title for the page
          h1("visTS: An Interactive Application to Visualise One-Dimensional Time Series"),
          box(width = 12,
              h2("Background"),

              p("Observations of events occurring through time (i.e. time series) are ubiquitous across academic disciplines. For instance, in the field of animal ecology, high resolution time series of movement attributes (e.g. depth) are now commonly collected by electronic tags, providing insights into the drivers of animal movement across a hierarchy of temporal scales. The open source R statistical programming environment is an increasingly used platform to visualise and analyse these kinds of data. Yet the rapid and thorough visualisation and exploration of time series remains cumbersome in R, especially for time series collected over extended periods of time and/or at high resolution. This constrains the ease with which trends in time series occurring at particular scales can be identified and qualitatively compared across individuals, as well as the generation and/or exploration of hypotheses for putative trends and their drivers. To this end, visTS is an interactive application for the rapid visualisation and exploration of data collected through time based on R Shiny. The only requirement is a dataframe with observations and associated timestamps. visTS provides functionality to visualise rapidly time series data (for multiple sample units, such as individuals); to summarise time series statistically over different scales; and to explore potential drivers in variables of interest by adding shading that delineates different time periods (e.g. day, night), by overlaying time series of potential covariates or by colouring the original time series by the values of a covariate. The user can zoom in and out of the time series rapidly to explore these patterns at different scales. To ensure publication quality figures are produced, there are several customisation options and plots can be saved as high resolution .png files. While users will always find that the greatest flexibility comes with programming custom routines, visTS provides a useful tool for the rapid and thorough visualisation and exploration of time series, to facilitate the identification of trends, putative drivers and their consistency across multiple sample units.",

                style = "font-size: 25px; font-family: Times New Roman; text-align: justify;"
              ) # close paragraph
          ) # close box

        ) # close tabItem(tabName = "introduction"


      ) # close tabItems
    ) # close dashboardBody
  ) # close dashboardPage




################################################
################################################
#### Server

server <- function(input, output) {


  ################################################
  ################################################
  #### Define reactive UI outputs

  ################################################
  #### global parameters

  lwd = 2
  cex.axis = 2
  cex.lab = 2.5

  ################################################
  #### variables to be plotted

  #### Define UI output for selecting an individual based on the ID column specified
  output$id_options <-
    renderUI(
      selectInput(inputId = "id",
                  label = strong("Select a factor level."),
                  choices = as.character(unique(data[, input$id_column])),
                  selected = as.character(unique(data[, input$id_column]))[1]
      )
    )

  #### Define fct() and fct_level()
  fct <- reactive({
    if(input$fct_yn){
      return(data[, input$id_column])
    } else{
      return(NULL)
    }
  })
  fct_level <- reactive({
    if(input$fct_yn){
      return(input$id)
    } else{
      return(NULL)
    }
  })

  #### Define column choices for timestamp column
  # Not currently implemented.
  # output$timestamp_column_choices <-
  #  renderUI(
  #    selectInput(inputId = "timestamp_column",
  #                label = strong("Select which variable you would like to add to the plot"),
  #                choices = colnames(data)[
  #                  !(colnames(data) %in% c(input$variable, input$id_column, input$timestamp_column))]
  #    )
  #  )

  #### Define variable choices for variable
  output$variable_choices <-
    renderUI(
      selectInput(inputId = "variable",
                  label = strong("Define the response variable."),
                  choices = colnames_numeric[
                    !(colnames_numeric %in% input$timestamp_column)]
      )
    )

  #### Define variable choices for variable2
  output$variable2_choices <-
    renderUI(
      selectInput(inputId = "variable2",
                  label = strong("Define the second response variable."),
                  choices = colnames_numeric[
                    !(colnames_numeric %in% c(input$variable, input$id_column, input$timestamp_column))]
                  )
    )

  #### Define variable2
  variable2 <- reactive({
    if(input$variable2_yn){
      return(input$variable2)
    } else{
      return(NULL)
    }
  })
  # Define data
  y2 <- reactive({
    if(input$variable2_yn){
      return(data[, variable2()])
    } else{
      return(NULL)
    }
  })

  #### Define subsetted dataframe for individual selected for convenience
  data_id <- reactive({
    if(input$fct_yn){
      return(data[which(data[, input$id_column] == input$id), ])
    } else{
      return(data)
    }
  })

  #### Define add_lines_args depending on inputted data
  add_lines_args <- reactive({
    if(input$variable2_yn & input$variable2_method == "by_colour"){
      ala <- list(lwd = lwd, type = input$line_type,
                  pretty_axis_args = list(pretty = list(n = 5),
                                          axis = list(las = TRUE, cex.axis = cex.axis)
                                          )
                  )
      # Adjusting the range of a colour scheme doesn't make sense in the same way that
      # ... adjusting a second axis does, so we won't implement this. (It causes errors.)
      #if(input$adjust_var2_range){
      #  ala$pretty_axis_args$lim <- list(var2_range())
      #}
    } else{
      ala <- list(lwd = lwd, type = input$line_type)
    }
    return(ala)
  })

  #### add_colour_bar_args
  add_colour_bar_args <-
    reactive({
      if(input$variable2_yn & input$variable2_method == "by_colour"){
        acba <- list(mtext_args = list(side = 4,
                                       text = input$ylab2,
                                       line = input$ylab2_line,
                                       cex = cex.lab))
      } else{
        acba <- list()
      }
      return(acba)
    })


  ################################################
  #### Axes properties

  #### Define class of timestamp column
  col_ts_class <- reactive(class(data[, input$timestamp_column])[1] %in% c("POSIXct", "POSIXlt", "Date", "date-time"))

  #### Define appropriate date range:
  # This assumes the columns are in date or as.posixt format and not numeric
  output$date_range <-
    renderUI(
      dateRangeInput(inputId = "date",
                     label = strong("Select an approximate date range."),
                     start = min(data_id()[, input$timestamp_column]),
                     end = max(data_id()[, input$timestamp_column])
      )
    )

  #### Define time_range
  output$date_range <-
    renderUI({
      if(class(data[, input$timestamp_column])[1] %in% c("POSIXct", "POSIXlt", "Date", "date-time")){
        # Adjust date range
        dateRangeInput(inputId = "date_range",
                       label = strong("(1) Select an approximate date range."),
                       start = min(data_id()[, input$timestamp_column]),
                       end = max(data_id()[, input$timestamp_column])
        )
      }
    })

  output$time_range <-
    renderUI({
      if(col_ts_class()){
        tui <- sliderInput(inputId = "time_range",
                           label = strong("(2) Use this option for further control."),
                           min = as.POSIXct.Date(input$date_range[1]),
                           max = as.POSIXct.Date(input$date_range[2]),
                           value = c(as.POSIXct.Date(input$date_range[1]),
                                     as.POSIXct.Date(input$date_range[2])))
      } else{
        tui <- numericRangeInput(inputId = "time_range",
                                 label = strong("Define the time range."),
                                 value = c(min(data_id()[, input$timestamp_column]),
                                           max(data_id()[, input$timestamp_column])),
                                 )
      }
      return(tui)
    })

  #### Define time range
  time_range <- reactive({
    if(input$adjust_time_range){
      return(input$time_range)
    } else{
      return(NULL)
    }
  })

  #### Define approximate range for the main response variable ("variable")
  output$var_range <-
    renderUI(
      numericRangeInput(inputId = "var_range",
                        label = strong("Fix y limits."),
                        value = c(min(data[, input$variable], na.rm = TRUE),
                                  max(data[, input$variable], na.rm = TRUE)
                        )
      )
    )

  #### Define var_range in practice
  var_range <- reactive({
    if(input$adjust_var_range){
      return(input$var_range)
    } else {
      return (NULL)
      }
  })

  #### Define pretty_axis_args
  pretty_axis_args <-
    reactive({
      paa <- list(side = c(as.numeric(input$define_x_side), 2),
                  lim = list(x = c(time_range()), y = c(var_range())),
                  pretty = list(n = 10),
                  axis = list(las = TRUE, cex.axis = cex.axis))
      return(paa)
    })


  #### Define add_grid_args
  add_grid_args <- reactive({
    if(input$add_grid) {
      add_grid_args <- list(col = input$grid_col, lwd = input$grid_lwd, lty = as.integer(input$grid_lty))
      } else {
        add_grid_args <- list()
      }
    return(add_grid_args)
  })

  #### Define the approximate range for the 2nd response variable ("variable2")
  output$var2_range <-
    renderUI(
      numericRangeInput(inputId = "var2_range",
                        label = strong("Fix y2 limits."),
                        value = c(min(data[, input$variable2], na.rm = TRUE),
                                  max(data[, input$variable2], na.rm = TRUE)
                        )
      )
    )

  #### Define var2range in practice
  var2_range <- reactive({
    if(input$adjust_var2_range){
      return(input$var2_range)
    } else {
      return (NULL)
    }
  })

  #### var2 pretty axis
  pretty_axis_args_y2 <-
    reactive({
      if(input$variable2_yn & input$variable2_method == "by_new_axis"){
        paay2 <- list(side = 4,
                      lim = list(y2 = c(var2_range())),
                      pretty = list(n = 10),
                      axis = list(las = TRUE, cex.axis = cex.axis))
      } else{
        paay2 <- list()
      }
      return(paay2)
    })
  #output$test <- renderText(
  #  str(mtext_args())
  #  )

  #### Define the position of the colour bar
  output$subplot_x <-
    renderUI({
      numericInput(inputId = "subplot_x",
                   label = strong("Define the x coordinate of the colour bar's bottom-left corner."),
                   value = as.numeric(round(max(data_id()[, input$timestamp_column], na.rm = TRUE)))
                   )
    })
  # output$test <- renderPrint(round(max(data_id()[, input$timestamp_column], na.rm = TRUE)))
  output$subplot_y <-
    renderUI({
      numericInput(inputId = "subplot_y",
                   label = strong("Define the y coordinate of the colour bar bottom-left corner."),
                   value = round(min(data_id()[, input$variable], na.rm = TRUE))
                   )
    })
  output$subplot_size <-
    renderUI({
      numericRangeInput(inputId = "subplot_size",
                        label = strong("Define the width and height of the colour bar (inches)."),
                        value = c(0.25, 4.5),
                        separator = " and ")
    })



  ################################################
  #### Inferential aids

  #### Reactive UI depending on whether numeric or timestamps
  output$interval <-
    renderUI({
      if(col_ts_class()){
        oi <- radioButtons(inputId = "interval",
                     label = strong("Define the time window over which you would like to calculate statistical summaries."),
                     choices = c("secs", "mins", "hours", "days", "weeks", "months", "years"),
                     selected = "days"
                     )
      } else{
        oi <- numericInput(inputId = "interval",
                     label = strong("Define the time window over which you would like to calculate statistical summaries."),
                     value = 10
                     )
      }
      return(oi)
    })

  #### Define summary functions
  summary_funs <- reactive({
    if("custom" %in% input$funs){
      pos_custom <- which(input$funs %in% "custom")
      summary_funs <- input$funs[-c(pos_custom)]
      summary_funs <- as.list(summary_funs)
      summary_funs <- append(summary_funs, eval(parse(text = input$funs_custom)))
    } else{
      summary_funs <- as.list(input$funs)
    }
    # Make sure functions are named and return
    names(summary_funs) <- 1:length(summary_funs)
    return(summary_funs)
  })

  #### summarise_in_bins_args
  summarise_in_bins_args <- reactive({
    if(input$add_summary){
      siba <- list(bin = input$interval,
                   funs = summary_funs())

    } else{
      siba <- list()
    }
    return(siba)
  })

  #### shading
  # type
  add_shading_type <- reactive({
    if(input$add_shading){
      return(input$shading_type)
    } else{ return(NULL)}
  })
  # arguments
  add_shading_dtb_args <- reactive({
    if(input$add_shading){
      return(list(type_args = list(lat = input$shading_lat, lon = input$shading_long)))
    } else{
      return(NULL)
    }
  })
  # args
  add_shading_args <- reactive({
    if(input$add_shading){
      return(list(border = FALSE))
    } else{
      return(list())
    }
  })


  ################################################
  #### Plot customisation options

  #### Define default labels
  output$ylab1_default <-
    renderUI(
      textInput(inputId = "ylab1",
                label = strong("Define the y axis label."),
                value = input$variable)
    )
  output$ylab2_default <-
    renderUI(
      textInput(inputId = "ylab2",
                label = strong("Define the second y axis label."),
                value = input$variable2)
    )

  #### mtext args
  mtext_args <-
    reactive({
      mta <- list(
        list(side = as.numeric(input$define_x_side),
             text = input$xlab,
             line = input$xlab_line,
             cex = cex.lab
             ),
        list(side = 2,
            text = input$ylab1,
            line = input$ylab1_line,
            cex = cex.lab
            )
      )
      # Add a list of arguments if the second variable is inputted via a new axis
      # (Otherwise, the variable title is dealt with in the add_colour_bar_args option)
      if(input$variable2_yn & input$variable2_method == "by_new_axis"){
        # Note the use of list() twice to ensure correct nested list structure.
        mta <- append(mta, list(list(side = 4, text = input$ylab2, line = input$ylab2_line, cex = cex.lab)))
      }
      return(mta)
    })

  ################################################
  #### Plot time series

  plot_input <-
    reactive({

      #### Validation
      # Check that the start date is before the end date
      # if(input$date[1] < input$date[2]){

      #### Create plot
      p <-
        pretty_ts(x = data[, input$timestamp_column],
                y1 = data[, input$variable],
                y2 = y2(),
                fct = fct(),
                fct_level = fct_level(),
                pretty_axis_args = pretty_axis_args(),
                mtext_args = mtext_args(),
                add_lines_args = add_lines_args(),
                y2_method = input$variable2_method,
                insert_colour_bar = TRUE,
                add_colour_bar_args = add_colour_bar_args(),
                subplot_args = list(x = input$subplot_x, y = input$subplot_y, size = input$subplot_size, vadj = 0, hadj = 0),
                pretty_axis_args_y2 = pretty_axis_args_y2(),
                add_lines_args_y2 = list(col = "royalblue", lwd = lwd),
                list_CIs_args = list(),
                add_error_envelope_args = list(),
                summarise_in_bins_args = summarise_in_bins_args(),
                add_lines_args_summaries = list(lwd = lwd, col = "red"),
                add_shading_type = add_shading_type(),
                add_shading_dtb_args = add_shading_dtb_args(),
                add_shading_args = add_shading_args(),
                add_grid_args = add_grid_args()
                )


     # } else { p <- plot(0, 0, type = "n", axes = F, xlab = "", ylab = "",
     #               main = "Please select a start date that is before the end date.",
     #              cex = 2)
     # }

      return(p)
    }) # close reactive for plot_input


  #### Print plot
  output$plot_timeseries <-
    renderPlot({

      #### Plotting margin
      graphics::par(oma = as.numeric(unlist(strsplit(input$par_oma, ","))))

      #### Make plot
      plot_input()

      })



  ################################################
  #### Download plot

  output$download_plot <- downloadHandler(
    filename = function(){paste("plot_", as.numeric(Sys.time()), ".png", sep = "")},
    contentType = "image/png",
    content = function(file) {
      grDevices::png(file, height = input$height, width = input$width, units = "in", res = input$res)

      #### Plotting margin
      graphics::par(oma = as.numeric(unlist(strsplit(input$par_oma, ","))))

      #### Create plot
      # Note that it is not possible to add plot_input() here; instead, the whole
      # ... plot command need to be repeated; as such, this is copied directly from above:
        pretty_ts(x = data[, input$timestamp_column],
                y1 = data[, input$variable],
                y2 = y2(),
                fct = data[, fct()],
                fct_level = fct_level(),
                pretty_axis_args = pretty_axis_args(),
                mtext_args = mtext_args(),
                add_lines_args = add_lines_args(),
                y2_method = input$variable2_method,
                insert_colour_bar = TRUE,
                add_colour_bar_args = add_colour_bar_args(),
                subplot_args = list(size = c(0.4, 5), vadj = 0, hadj = 0),
                pretty_axis_args_y2 = pretty_axis_args_y2(),
                add_lines_args_y2 = list(col = "royalblue", lwd = lwd),
                list_CIs_args = list(),
                add_error_envelope_args = list(),
                summarise_in_bins_args = summarise_in_bins_args(),
                add_lines_args_summaries = list(lwd = lwd, col = "red"),
                add_shading_type = add_shading_type(),
                add_shading_dtb_args = add_shading_dtb_args(),
                add_shading_args = add_shading_args()
        )


      #### Close graphical device
      grDevices::dev.off()

    })




} # close server function






################################################
################################################
#### Launch App:

shinyApp(ui, server = server)


} # close visTS(){} function

#### End of code.
################################################
################################################
