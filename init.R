# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("RSQLite", "dplyr", "wordcloud2", "readr",
                "leaflet", "maps", "shiny", "RColorBrewer",
                "mapdata", "rnaturalearth", "tigris", "ggplot2",
                "leaflegend", "shinydashboard", "tidyr", "sf",
                "lubridate", "dygraphs", "xts", "DT", "tidytext",
                "scales", "quanteda", "newsmap", "tools", "shinycssloaders",
                "zoo", "future", "BMS", "plotly")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))

