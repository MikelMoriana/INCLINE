library(tidyverse)
community23 <- read_csv2("C:/Users/mikemori/Downloads/INCLINE_community_2018_2019_2021_2022_2023.csv")
community22_korrekt <- read_csv2("C:/Users/mikemori/Downloads/INCLINE_community_2018_2019_2021_2022_korrekturlest.csv")

community22 <- community23 |> filter(year != 2023)
community22 |> filter(subPlot == 10)
community22_korrekt |> filter(subPlot == 10)
view(community22 |> filter(Site == "Lavisdalen" & Block == 5 & plot == 3 & year == 2021))
view(community22_korrekt |> filter(Site == "Lavisdalen" & Block == 5 & plot == 3 & year == 2021))
