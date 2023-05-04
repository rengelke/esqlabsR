#---------- Initialization of app ----------#
# Load required packages
library(shiny)

#---------- User interface ----------#
ui <- fluidPage(
  tags$style(type = "text/css", ".modal-dialog {overflow: auto; width: auto; background:white;}"),
  shinyjs::useShinyjs(),
  headerPanel("PlotsFile Editor"),
  br(),
  actionButton("changeProjConfig", "Change project configuration file"),
  span(textOutput("currentProjConfig"), style = "color:red"),
  br(),
  actionButton("save", "Save plotsFile"),
  actionButton("saveNew", "Save as new file"),
  hr(),
  tabsetPanel(
    tabPanel(
      "DataCombined",
      br(),
      fluidRow(
        column(2, actionButton("addData", "Add Data")),
        column(4, span(textOutput("infoAddData"), style = "color:red")),
      ),
      br(),
      fluidRow(
        column(3, textInput("DataCombinedName", label = "DataCombinedName")),
        column(3, selectInput("dataType", label = "dataType", choices = c("observed", "simulated"))),
        column(3, textInput("label", label = "label"))
      ),
      fluidRow(
        column(3, selectizeInput("scenario", label = "scenario", choices = "", options = list(create = TRUE))),
        column(6, selectizeInput("path", label = "path", choices = "", options = list(create = TRUE)))
      ),
      fluidRow(
        column(9, selectizeInput("dataSet", label = "dataSet", choices = "", options = list(create = TRUE)))
      ),
      fluidRow(
        column(2, textInput("group", label = "group")),
        column(2, textInput("xOffsets", label = "xOffsets", placeholder = "e.g. (1, 2, 3)")),
        column(2, textInput("yOffsets", label = "yOffsets", placeholder = "e.g. (1, 2, 3)")),
        column(2, textInput("xScaleFactors", label = "xScaleFactors", placeholder = "e.g. (1, 2, 3)")),
        column(2, textInput("yScaleFactors", label = "yScaleFactors", placeholder = "e.g. (1, 2, 3)")),
      ),
      br(),
      tableOutput("dataCombinedSheet")
    ),
    tabPanel(
      "plotConfiguration",
      br(),
      fluidRow(
        column(2, actionButton("addPlot", "Add Plot")),
        column(3, span(textOutput("infoAddPlot"), style = "color:red")),
      ),
      br(),
      fluidRow(
        column(2, textInput("plotID", label = "plotID")),
        column(2, selectizeInput("PlotDataCombined", label = "DataCombinedName", choices = "")),
        column(2, selectInput("plotType",
          label = "plotType",
          choices = c("individual", "population", "observedVsSimulated", "residualsVsSimulated", "residualsVsTime")
        )),
        column(2, textInput("plotTitle", label = "title")),
        column(2, selectizeInput("xUnit", label = "xUnit", choices = c("", unname(purrr::list_flatten(ospUnits))))),
        column(2, selectizeInput("yUnit", label = "yUnit", choices = c("", unname(purrr::list_flatten(ospUnits)))))
      ),
      fluidRow(
        column(2, selectizeInput("xAxisScale", label = "xAxisScale", choices = c("", tlf::Scaling))),
        column(2, selectizeInput("yAxisScale", label = "yAxisScale", choices = c("", tlf::Scaling))),
        column(2, textInput("xAxisLimits", label = "xAxisLimits", placeholder = "e.g. (0, 10)")),
        column(2, textInput("yAxisLimits", label = "yAxisLimits", placeholder = "e.g. (0, 10)")),
        column(2, textInput("quantiles", label = "quantiles", placeholder = "e.g. (-2, 0, 2)")),
        column(2, textInput("foldDistance", label = "foldDistance")),
      ),
      br(),
      tableOutput("plotConfigurationSheet")
    ),
    tabPanel(
      "plotGrids",
      br(),
      fluidRow(
        column(2, actionButton("addPlotGrid", "Add Plot Grid")),
        column(3, span(textOutput("infoAddPlotGrid"), style = "color:red")),
      ),
      br(),
      fluidRow(
        column(3, textInput("plotGridName", label = "name")),
        column(3, selectizeInput("plotIDs", label = "plotIDs", choices = "", multiple = TRUE)),
        column(3, textInput("plotGridTitle", label = "title"))
      ),
      br(),
      tableOutput("plotGridSheet")
    )
  )
)

#---------- Backend logic ----------#
server <- function(input, output, session) {

  r <- reactiveValues()


  ######## FILE IMPORT ###########

  # Choose a file on computer
  observeEvent(input$changeProjConfig, {
    tryCatch(
      {
        r$newConfigFile <- file.choose()
        r$displayFileMessage <- paste(" Current project configuration file:", r$newConfigFile)
      },
      error = function(cond) {
        r$displayFileMessage <- "No project configuration selected"
        return(NULL)
      }
    )
  })

  # Load excel files based on selected project configuration
  observeEvent(r$newConfigFile, {
    loadProjectConfiguration(r)
  })


  #' loadProjectConfiguration
  #' @description Read project configuration files and import plot, scenario and data files.
  #'
  #' @param r reactiveValues object
  loadProjectConfiguration <- function(r) {
    # browser()
    r$newProjectConfiguration <- createDefaultProjectConfiguration(r$newConfigFile)

    codeFolder <- dirname(r$newConfigFile)

    r$plotFile <- file.path(codeFolder, r$newProjectConfiguration$paramsFolder, r$newProjectConfiguration$plotsFile)
    r$scenarioFile <- file.path(codeFolder, r$newProjectConfiguration$paramsFolder, r$newProjectConfiguration$scenarioDefinitionFile)

    # Load plotFile
    if (file.exists(r$plotFile)) {
      r$dfDataCombined <- readExcel(r$plotFile, sheet = "DataCombined")
      r$dfPlots <- readExcel(r$plotFile, sheet = "plotConfiguration")
      r$dfPlotGrids <- readExcel(r$plotFile, sheet = "plotGrids")
    } else {
      r$dfDataCombined <- NULL
      r$dfPlots <- NULL
      r$dfPlotGrids <- NULL
    }

    # Load ScenarioFile
    if (file.exists(r$scenarioFile)) {
      r$dfScenarios <- readExcel(r$scenarioFile, sheet = "Scenarios")
      r$dfOutputPaths <- readExcel(r$scenarioFile, sheet = "OutputPaths")
    }

    # Load dataFile

    dataFolder <- file.path(codeFolder, r$newProjectConfiguration$dataFolder)
    dataFile <- file.path(dataFolder, r$newProjectConfiguration$dataFile)
    dataImporterConfigurationFile <- file.path(dataFolder, r$newProjectConfiguration$dataImporterConfigurationFile)
    if (file.exists(dataFile) & file.exists(dataImporterConfigurationFile)) {
      tryCatch(
        {
          r$datasets <- ospsuite::loadDataSetsFromExcel(
            xlsFilePath = dataFile,
            importerConfigurationOrPath = dataImporterConfigurationFile,
            # currently all sheets will be loaded
            importAllSheets = TRUE
          )
        },
        error = function(cond) {
          return()
        }
      )
    }
  }


  ######## Add new rows in tables ###########

  observeEvent(input$addData, {
    req(input$DataCombinedName)
    req(input$label)

    if (input$dataType == "simulated") {
      req(input$scenario)
      req(input$path)

      newRow <- data.frame(
        DataCombinedName = input$DataCombinedName,
        dataType = input$dataType,
        label = input$label,
        scenario = input$scenario,
        path = input$path, group = input$group
      )
    } else {
      req(input$dataSet)
      newRow <- data.frame(
        DataCombinedName = input$DataCombinedName,
        dataType = input$dataType,
        label = input$label,
        dataSet = input$dataSet,
        group = input$group
      )
    }

    r$dfDataCombined <- bind_rows(r$dfDataCombined, newRow)
  })

  # TODO: apply same strategy below

  observeEvent(input$addPlot, {
    if (gsub(" ", "", input$plotID) == "") {
      output$infoAddPlot <- renderText("Please fill in field 'plotID'")
      return()
    }
    if (gsub(" ", "", input$plotID) %in% r$dfPlots$plotID) {
      output$infoAddPlot <- renderText("Plot ID already exists, please choose another ID")
      return()
    }
    output$infoAddPlot <- NULL
    newRow <- data.frame(
      plotID = input$plotID,
      DataCombinedName = input$PlotDataCombined,
      plotType = input$plotType,
      title = input$plotTitle,
      xUnit = input$xUnit,
      yUnit = input$yUnit,
      xAxisScale = input$xAxisScale,
      yAxisScale = input$yAxisScale,
      xAxisLimits = input$xAxisLimits,
      yAxisLimits = input$yAxisLimits,
      quantiles = input$quantiles,
      foldDistance = input$foldDistance
    )
    r$dfPlots <- bind_rows(r$dfPlots, newRow)
  })

  observeEvent(input$addPlotGrid, {
    if (gsub(" ", "", input$plotGridName) == "" || is.null(input$plotIDs)) {
      output$infoAddPlotGrid <- renderText("Please fill in fields 'name' and 'plotIDs'")
      return()
    }
    if (gsub(" ", "", input$plotGridName) %in% r$dfPlotGrids$name) {
      output$infoAddPlotGrid <- renderText("Plot grid already exists, please choose another name")
      return()
    }
    output$infoAddPlot <- NULL
    newRow <- data.frame(
      name = input$plotGridName,
      plotIDs = paste(input$plotIDs, collapse = ", "),
      title = input$plotGridTitle
    )
    r$dfPlotGrids <- bind_rows(r$dfPlotGrids, newRow)
  })


  ####### SAVE OUTPUT ########

  # save changes to projectConfiguration$plotsFile
  observeEvent(input$save, {
    writeExcel(list(
      "DataCombined" = r$dfDataCombined,
      "plotConfiguration" = r$dfPlots,
      "plotGrids" = r$dfPlotGrids
    ), path = r$plotFile)
  })

  # save data frames of plotsFile in new file
  observeEvent(input$saveNew, {
    try(writeExcel(list(
      "DataCombined" = r$dfDataCombined %||% data.frame(),
      "plotConfiguration" = r$dfPlots %||% data.frame(),
      "plotGrids" = r$dfPlotGrids %||% data.frame()
    ), path = file.choose(new = TRUE)))
  })

  ####### GENERAL OBSERVERS ##########

  # Update Inputs based on data contents
  observeEvent(r$dfDataCombined, {
    choices <- if (is.null(r$dfDataCombined)) {
      ""
    } else {
      r$dfDataCombined$DataCombinedName
    }
    updateSelectizeInput(session, "PlotDataCombined", choices = choices)
  })

  observeEvent(r$dfPlots, {
    choices <- if (is.null(r$dfPlots)) {
      ""
    } else {
      r$dfPlots$plotID
    }
    updateSelectizeInput(session, "plotIDs", choices = choices)
  })


  observeEvent(r$dfScenarios, {
    choices <- if (is.null(r$dfScenarios)) {
      ""
    } else {
      r$dfScenarios$Scenario_name
    }
    updateSelectizeInput(session, "scenario", choices = choices, options = list(create = TRUE))
  })

  observeEvent(r$dfOutputPaths, {
    choices <- if (is.null(r$dfOutputPaths)) {
      ""
    } else {
      r$dfOutputPaths$OutputPath
    }
    updateSelectizeInput(session, "path", choices = choices, options = list(create = TRUE))
  })


  observeEvent(r$dataSets, {
    choices <- if (is.null(r$dataSets)) {
      ""
    } else {
      names(r$dataSets)
    }
    updateSelectizeInput(session, "dataSet", choices = choices, options = list(create = TRUE))
  })


  # Able/Disable some fields depending on dataType input
  # TODO: explain why here
  observeEvent(input$dataType, {
    output$infoAddData <- NULL
    if (input$dataType == "simulated") {
      shinyjs::enable("scenario")
      shinyjs::enable("path")
      updateTextInput(session, "dataSet", value = "")
      shinyjs::disable("dataSet")
    } else {
      shinyjs::disable("scenario")
      shinyjs::disable("path")
      shinyjs::enable("dataSet")
    }
  })


  # Display notification if one input is missing for adding data
  observeEvent(input$addData, {
    necessary_inputs <- c("DataCombinedName", "label")

    if (input$dataType == "simulated") {
      necessary_inputs <- c(necessary_inputs, "scenario", "path")
    } else {
      necessary_inputs <- c(necessary_inputs, "dataSet")
    }

    for (ni in necessary_inputs) {
      if (is.na(input[[ni]]) || input[[ni]] == "" || input[[ni]] == " ") {
        showNotification(ui = paste(ni, "is missing."))
      }
    }
  })


  # OUTPUTS

  # display dataCombined table
  output$dataCombinedSheet <- renderTable(r$dfDataCombined)

  # display plots table
  output$plotConfigurationSheet <- renderTable(r$dfPlots)

  # display plotgrids table
  output$plotGridSheet <- renderTable(r$dfPlotGrids)

  # Display a message describing imported file status
  output$currentProjConfig <- renderText({
    req(r$displayFileMessage)
    r$displayFileMessage
  })
}

shinyApp(ui = ui, server = server)
