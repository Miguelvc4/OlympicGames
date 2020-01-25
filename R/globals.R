utils::data("DatosOlimpicos")
utils::data("DatosResumen")
utils::data("DatosUnificados")
utils::data("DatosEdad")

utils::globalVariables(c("ID", "Year", "Season", "DatosOlimpicos", "Region",
                         "SportsCompetition","Event","spread","Medal",
                         "DatosEdad","showOutput","renderChart2","Age","Gender","cor",
                         "Total","long","lat","group","region","Gold","Silver","Bronze",
                         "DatosUnificados","Count","DatosResumen","hPlot","write.csv",
                         "read.csv"))
