projectConfiguration <- esqlabsR::createDefaultProjectConfiguration("C:/Users/SergeiVavilov/Documents/esqlabs/test_project/ProjectConfiguration.xlsx")
testScenario <- esqlabsR::createScenarios(esqlabsR::readScenarioConfigurationFromExcel(scenarioNames = "TestScenario", projectConfiguration = projectConfiguration))
observedData <- esqlabsR::loadObservedData(projectConfiguration = projectConfiguration, sheets = "Laskin 1982.Group A")
esqlabsR::runScenarios(testScenario)

createPITaskFromExcel(scenarios = testScenario, observedData = observedData, projectConfiguration = projectConfiguration, sheets = "Laskin 1982.Group A")
