createPITaskFromExcel <- function(scenarios, observedData, projectConfiguration) {
  validateIsOfType(scenarios, "Scenario")
  validateIsOfType(observedData, "DataSet")
  validateIsOfType(projectConfiguration, "ProjectConfiguration")

  PISettings <- readExcel(projectConfiguration$PISettingsFile, sheet = "PISettings")

}
