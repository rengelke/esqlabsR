createPITaskFromExcel <- function(scenarios, observedData, projectConfiguration) {
  validateIsOfType(scenarios, "Scenario")
  validateIsOfType(observedData, "DataSet")
  validateIsOfType(projectConfiguration, "ProjectConfiguration")

  PISettings <- readExcel(projectConfiguration$PISettingsFile, sheet = "PISettings")

  # create PIParameter objects, one per a distinct entry in `Parameter name` column
  parameterNames <- unique(PISettings[["Parameter name"]])
  taskParameters <- vector(mode = "list", length = length(parameterNames))
  for (parameterIndex in seq_along(parameterNames)) {
    rows <- PISettings %>% filter(`Parameter name` == parameterNames[parameterIndex])
    for (rowIndex in 1:nrow(rows)) {
      modelParameters <- list()
      # load all scenarios specified in the `Scenario` column
      scenarios <- createScenarios(readScenarioConfigurationFromExcel(scenarioNames = rows[["Scenario"]], projectConfiguration = projectConfiguration))
      modelParameters <- c(modelParameters, ospsuite::getParameter(path = rows[["Parameter path"]][[rowIndex]], simulation = scenarios[[rowIndex]][["simulation"]]))
    }
    taskParameters[[idx]] <- PIParameters$new(parameters = modelParameters)
  }

  # create PIConfiguration object and populate it with values from the first row of the Excel file
  taskConfiguration <- PIConfiguration$new()
  taskConfiguration$printEvaluationFeedback <- PISettings[["Print evaluation feedback"]][[1]]
  taskConfiguration$algorithm <- PISettings[["Algorithm"]][[1]]
  taskConfiguration$algorithmOptions <- PISettings %>%
    filter(!is.na(`Algorithm option name`)) %>%
    select(`Algorithm option name`, `Algorithm option value`) %>%
    split(x = .$`Algorithm option value`, f = .$`Algorithm option name`)

  # create simulation object from the first specified scenario
  taskSimulation <- createScenarios(readScenarioConfigurationFromExcel(scenarioNames = rows[["Scenario"]][[1]], projectConfiguration = projectConfiguration))[["simulation"]]

  outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                                              container = simulations$Aciclovir
  ))
  outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
  outputMapping$scaling <- "lin"
  outputMappings <- c(outputMapping)

  # create PIOutputMapping objects, one per row in the Excel file
  scenarios <- createScenarios(readScenarioConfigurationFromExcel(scenarioNames = rows[["Scenario"]], projectConfiguration = projectConfiguration))
  simulations <- lapply(scenarios, function(scenario) scenario[["simulation"]])
  taskOutputMappings <- lapply(1:nrow(PISettings), function(rowIndex) {
    outputMapping <- PIOutputMapping$new(quantity = getQuantity(PISettings[["Output path"]][[rowIndex]],
                                                                  container = simulations[[rowIndex]]
    ))
    outputMapping$addObservedDataSets(observedData[[PISettings[["Observed data"]][[rowIndex]]]])
    outputMapping$scaling <- PISettings[["Scaling"]][[rowIndex]]
    outputMapping
  })

  return(ParameterIdentification$new(
    parameters = taskParameters,
    configuration = taskConfiguration,
    simulations = taskSimulation,
    outputMappings = taskOutputMappings
  ))
}
