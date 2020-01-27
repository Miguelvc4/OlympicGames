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

download("https://github.com/ramnathv/rCharts/archive/master.tar.gz", "rCharts.tar.gz")
install.packages("rCharts.tar.gz", repos = NULL, type = "source")
library(rCharts)
