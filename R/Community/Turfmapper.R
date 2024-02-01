# Libraries and functions. We get the file from the main cleaning script----

library(turfmapper)
library(pipebind)

grid <- make_grid(ncol = 7, nrow = 5) # We create the 5x7 grid for the turfmapper

# Find the subplots with wrong number
find_subplot_species <- function(x, y, z) {
  x |>
    filter(plotID == y) |>
    filter(year == z) |> 
    filter(subPlot %in% c(9, 11, 13, 23, 25, 27)) |> 
    select(subPlot, species, value)
}

# Find in what specific plot and year a species was recorded
find_plot_year <- function(x, y) {
  x |>
    filter(species == y) |>
    select(plotID, year) |>
    unique()
}

# We adjust the make_turf_function from turfmapper to not include cover
make_turf_plot_fertile <- function(data, grid_long, year, subturf, species, fertile, title) {
  data <- rename(data,
                 subturf = {{ subturf }},
                 species = {{ species }},
                 year = {{ year }}
  )
  stopifnot(all(data$subturf %in% grid_long$subturf))
  data <- left_join(data, grid_long, by = "subturf")
  ggplot(data, aes(x = .data$.x, y = .data$.y, fill = {{ fertile }})) +
    geom_tile(colour = "grey60") +
    facet_grid(species ~ year) +
    ggtitle(title) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Paired") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.spacing.y = unit(0.04, "lines"),
      panel.grid.minor = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

turfplot <- function(x, y) {
  x |>
    filter(plotID == y) |>
    pipebind::bind(
      x,
      make_turf_plot_fertile(
        data = x,
        grid_long = grid,
        year = year, species = species, subturf = subPlot, fertile = fertile,
        title = glue::glue("Site {x$site}: plot {x$plotID}")
      )
    )
}

turfplot_year <- function(x, y, z) {
  x |>
    filter(plotID == y) |>
    filter(year == z) |> 
    pipebind::bind(
      x,
      make_turf_plot_fertile(
        data = x,
        grid_long = grid,
        species = species, subturf = subPlot, fertile = fertile,
        title = glue::glue("Site {x$site}: plot {x$plotID}")
      )
    )
}

# In a few cases the subplot number is wrong (there shouldn't be anything in subplots 9, 11, 13, 15, 17, 19)----

# We check the turfplot and the scans to decide what we do with them
community_clean_subplot |> 
  filter(subPlot %in% c(9, 11, 13, 23, 25, 27)) |> 
  select(plotID, year) |> 
  unique() |> 
  print(n = Inf)

# Skj_7_1, 2018
find_subplot_species(community_clean_subplot, "Skj_7_1", 2018) # Alc_alp 23
turfplot_year(community_clean_subplot, "Skj_7_1", 2018) # We remove it

# Skj_3_1  2019
find_subplot_species(community_clean_subplot, "Skj_3_1", 2019) # Vio_pal 13
turfplot_year(community_clean_subplot, "Skj_3_1", 2019) # It's subplot 14, not 13

# Skj_6_6  2019
find_subplot_species(community_clean_subplot, "Skj_6_6", 2019) # Sal_her 9, 11, 13. Oma_sup 13
turfplot_year(community_clean_subplot, "Skj_6_6", 2019) # Sal_her has been shifted one to the left (9 for 10, 11 for 12, 13 for 14), and is missing in 7. We remove Oma_sup

# Skj_7_1  2019
find_subplot_species(community_clean_subplot, "Skj_7_1", 2019) # Sib_pro 9, 11, 13, 23, 25, 27
turfplot_year(community_clean_subplot, "Skj_7_1", 2019) # Sib_pro also counted in transplant subplots. This was for another study. For this one we remove it

# Skj_7_5  2019
find_subplot_species(community_clean_subplot, "Skj_7_5", 2019) # Sib_pro 11, 13, 23, 25, 27. Ver_alp 13
turfplot_year(community_clean_subplot, "Skj_7_5", 2019) # Sib_pro also counted in transplant subplots. This was for another study. For this one we remove it. Ver_alp was in fact registered in subplot 13. We check other years
turfplot(community_clean_subplot, "Skj_7_5") # No Ver_alp near subplot 13, we remove it

# Gud_1_2  2019
find_subplot_species(community_clean_subplot, "Gud_1_2", 2019) # Des_ces 23
turfplot_year(community_clean_subplot, "Gud_1_2", 2019) # We remove it

# Gud_1_3  2019
find_subplot_species(community_clean_subplot, "Gud_1_3", 2019) # Pot_ere 27
turfplot_year(community_clean_subplot, "Gud_1_3", 2019) # It's subplot 26 (crossed-out something in 26, had to write in 27)

# Gud_1_4  2019
find_subplot_species(community_clean_subplot, "Gud_1_4", 2019) # Ast_alp 9
turfplot_year(community_clean_subplot, "Gud_1_4", 2019) # It's subplot 8, not 9

# Gud_2_2  2019
find_subplot_species(community_clean_subplot, "Gud_2_2", 2019) # Nid_seedling 9. Nid_seedling is not used in the current analyses, so I don't do anything

# Gud_7_1  2019
find_subplot_species(community_clean_subplot, "Gud_7_1", 2019) # Tha_alp 9, 11, 13
turfplot_year(community_clean_subplot, "Gud_7_1", 2019) # They're shifted to the right. And 7 is actually 14

# Lav_2_2  2021
find_subplot_species(community_clean_subplot, "Lav_2_2", 2021) # Vio_bif 9, 11
turfplot_year(community_clean_subplot, "Lav_2_2", 2021) # They're shifted to the left. And 14 is actually 16

# Lav_2_5  2021
find_subplot_species(community_clean_subplot, "Lav_2_5", 2021) # Agr_cap 9, 11, 13
turfplot_year(community_clean_subplot, "Lav_2_5", 2021) # They're shifted to the left, and I delete 13
# We found out that moss, lichen and litter are shifted one column for the whole plot

# Lav_2_6  2021
find_subplot_species(community_clean_subplot, "Lav_2_6", 2021) # Cer_cer 9
turfplot_year(community_clean_subplot, "Lav_2_6", 2021) # 9 is actually 10. And 3 and 4 are also shifted (they're 4 and 5)
# We found out that Vac_myr was not typed in. Luckily, Agr_cap is found in all the plots were Vac_myr exists, so we use it

# Lav_5_3  2021
find_subplot_species(community_clean_subplot, "Lav_5_3", 2021) # Agr_cap 13
turfplot_year(community_clean_subplot, "Lav_5_3", 2021) # I delete 13. We're missing 16 and 17. And we are missing moss, lichen... for all

# Lav_7_1  2021
find_subplot_species(community_clean_subplot, "Lav_7_1", 2021) # Sib_pro 9
turfplot_year(community_clean_subplot, "Lav_7_1", 2021) # 9 is actually 8

# Lav_7_3  2021
find_subplot_species(community_clean_subplot, "Lav_7_3", 2021) # Ant_odo 13
turfplot_year(community_clean_subplot, "Lav_7_3", 2021) # Some columns have shifted left: I change 13 to 15 and 16 to 18

# Skj_1_3  2021
find_subplot_species(community_clean_subplot, "Skj_1_3", 2021) # Agr_cap 11
turfplot_year(community_clean_subplot, "Skj_1_3", 2021) # 11 is 12, and 14 is 15. 17 should be empty (only logger)

# Skj_1_5  2021
find_subplot_species(community_clean_subplot, "Skj_1_5", 2021) # Tha_alp 23, 25, 27
turfplot_year(community_clean_subplot, "Skj_1_5", 2021) # Have been shifter to the right. 23 is 22, 25 is 24 and 27 is 26

# Skj_4_3  2021
find_subplot_species(community_clean_subplot, "Skj_4_3", 2021) # Cam_rot 23, 25
turfplot_year(community_clean_subplot, "Skj_4_3", 2021) # We are missing 19 and 24, we change them

# Gud_2_2  2021
find_subplot_species(community_clean_subplot, "Gud_2_2", 2021) # Alc_alp and Ave_fle 23
turfplot_year(community_clean_subplot, "Gud_2_2", 2021) # Alc_alp: 20 for 23. Ave_fle: 24 for 23, 19 for 22

# Gud_3_5  2021
find_subplot_species(community_clean_subplot, "Gud_3_5", 2021) # Vio_bif 9
turfplot_year(community_clean_subplot, "Gud_3_5", 2021) # We remove it

# Gud_4_4  2021
find_subplot_species(community_clean_subplot, "Gud_4_4", 2021) # Nar_str 13
turfplot_year(community_clean_subplot, "Gud_4_4", 2021) # Some columns have shifted right: 13 for 12, 17 for 14. We create 18

# Gud_5_2  2021
find_subplot_species(community_clean_subplot, "Gud_5_2", 2021) # Many species. Looking at the scan, they were also recorded in these subplots
turfplot_year(community_clean_subplot, "Gud_5_2", 2021) # We remove them

# Gud_5_4  2021
find_subplot_species(community_clean_subplot, "Gud_5_4", 2021) # Many species. Looking at the scan, they were also recorded in these subplots
turfplot_year(community_clean_subplot, "Gud_5_4", 2021) # We remove them

# Ulv_1_1  2021
find_subplot_species(community_clean_subplot, "Ulv_1_1", 2021) # Agr_cap 13
turfplot_year(community_clean_subplot, "Ulv_1_1", 2021) # We remove it

# Ulv_3_4  2021
find_subplot_species(community_clean_subplot, "Ulv_3_4", 2021) # Many species. Looking at the scan, they were also recorded in subplot 9
turfplot_year(community_clean_subplot, "Ulv_3_4", 2021) # We remove it

# Ulv_6_4  2022
find_subplot_species(community_clean_subplot, "Ulv_6_4", 2022) # Eup_wet 11
turfplot_year(community_clean_subplot, "Ulv_6_4", 2022) # It's 10, not 11

# Ulv_7_2  2022
find_subplot_species(community_clean_subplot, "Ulv_7_2", 2022) # Bis_viv and Tar_sp 25
turfplot_year(community_clean_subplot, "Ulv_7_2", 2022) # Bis_viv: it's not 25, but 24. Tar_sp is wrong, I write it again

# Skj_6_6  2022
find_subplot_species(community_clean_subplot, "Skj_6_6", 2022) # Alc_alp 25
turfplot_year(community_clean_subplot, "Skj_6_6", 2022) # It's 24, not 25

# Gud_4_6  2023
find_subplot_species(community_clean_subplot, "Gud_4_6", 2023) # Tha_alp 9, 11
turfplot_year(community_clean_subplot, "Gud_4_6", 2023) # They've been shifted left. 9 is 10 and 11 is 12

# Ulv_2_1  2023
find_subplot_species(community_clean_subplot, "Ulv_2_1", 2023) # Nar_str 27
turfplot_year(community_clean_subplot, "Ulv_2_1", 2023) # 27 is 28

# Some individuals were a bit uncertain (suffix _cf in the file)----

levels(as.factor(grep("_cf$", community_clean_subplot_nr$species, value = TRUE)))
# Agr_cap_cf, Car_cap_cf, Car_nig_cf, Car_nor_cf, Epi_ana_cf, Ran_acr_cf and Vio_can_cf

find_plot_year(community_clean_subplot_nr, "Agr_cap_cf") # Lav_4_1 and Lav_5_2 in 2021, Gud_7_3 in 2023
turfplot(community_clean_subplot_nr, "Gud_7_3") # Seems it is indeed Agr_cap
turfplot(community_clean_subplot_nr, "Lav_4_1") # Seems it is indeed Agr_cap
turfplot(community_clean_subplot_nr, "Lav_5_2") # Seems it is indeed Agr_cap

find_plot_year(community_clean_subplot_nr, "Car_cap_cf") # Gud_4_3 and Gud_4_4 in 2021, Gud_6_3 in 2023
turfplot(community_clean_subplot_nr, "Gud_4_3") # Seems it is actually Car_fla
turfplot(community_clean_subplot_nr, "Gud_4_4") # Seems it is indeed Car_cap
turfplot(community_clean_subplot_nr, "Gud_6_3") # Seems it is indeed Car_cap

find_plot_year(community_clean_subplot_nr, "Car_nig_cf") # Skj_3_1 in 2023
turfplot(community_clean_subplot_nr, "Skj_3_1") # Seems it is actually Car_big

find_plot_year(community_clean_subplot_nr, "Car_nor_cf") # Lav_3_3 and Skj_1_1 in 2023
turfplot(community_clean_subplot_nr, "Lav_3_3") # Seems it is indeed Car_nor
turfplot(community_clean_subplot_nr, "Skj_1_1") # Seems it is actually Car_cap

find_plot_year(community_clean_subplot_nr, "Epi_ana_cf") # Lav_2_4
turfplot(community_clean_subplot_nr, "Lav_2_4") # Seems it is indeed Epi_ana

find_plot_year(community_clean_subplot_nr, "Ran_acr_cf") # Gud_2_2
turfplot(community_clean_subplot_nr, "Gud_2_2") # The scans says Rum_ace. But neither of the species grow in this block. I remove it

find_plot_year(community_clean_subplot_nr, "Vio_can_cf") # Ulv_7_4
turfplot(community_clean_subplot_nr, "Ulv_7_4") # Seems it is actually a juvenile Vio_bif

# For some individuals we know the genus but not the species (_sp)----

levels(as.factor(grep("_sp", community_clean_subplot_nr_cf$species, value = TRUE)))
# Alc_sp and Tar_sp are species by themselves, we keep them. We check the rest

# Ant_sp
find_plot_year(community_clean_subplot_nr_cf, "Ant_sp") # Skj_6_3 in 2019
turfplot(community_clean_subplot_nr_cf, "Skj_6_3") # Not really possible to tell, I drop it

# Car_sp
find_plot_year(community_clean_subplot_nr_cf, "Car_sp") |> print(n = 41) # Many cases. I create a file only with Carex
community_carex <- community_clean_subplot_nr_cf |> filter(grepl("Car_", species))
turfplot(community_carex, "Gud_1_2") # Seems it is Car_vag in 2018. In the other cases either the other species were already present in the subplot, or none appear any year, and then better to drop them
turfplot(community_carex, "Gud_1_3") # Seems it is Car_vag in all cases. No duplicates
turfplot(community_carex, "Gud_1_4") # Seems it is Car_vag in all cases. Duplicates with unique()
turfplot(community_carex, "Gud_1_5") # Seems it is Car_vag in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_2_1") # Seems it is Car_big in all cases. Subplot1 must be removed
turfplot(community_carex, "Gud_2_2") # Seems it is Car_big in all cases. Car_sp in subplot17 2022 is cf, we remove the Car_big. Car_big in subplot34 2023 is D, we remove the Car_sp
turfplot(community_carex, "Gud_2_3") # Seems it is Car_big in all cases. Duplicates with unique()
turfplot(community_carex, "Gud_2_4") # Seems it is Car_big in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_3_2") # Seems it is Car_big in 2018 and Car_vag in 2019. Duplicate with unique()
turfplot(community_carex, "Gud_3_3") # Seems it is Car_big in all cases. No duplicates
turfplot(community_carex, "Gud_3_5") # Seems it is Car_vag in all cases. No duplicates
turfplot(community_carex, "Gud_4_1") # All other Carex already present in that subplot. I drop it
turfplot(community_carex, "Gud_4_3") # All other Carex already present in those subplots. I drop them
turfplot(community_carex, "Gud_4_4") # All other Carex already present in those subplots. I drop them
turfplot(community_carex, "Gud_5_1") # Seems it is Car_cap (looking at the scan). No duplicates
turfplot(community_carex, "Gud_5_2") # All other Carex already present in that subplot. I drop it
turfplot(community_carex, "Gud_5_4") # Seems it is Car_big. No duplicate
turfplot(community_carex, "Gud_5_5") # Seems it is Car_vag in all cases. No duplicates
turfplot(community_carex, "Gud_7_2") # Car_big: 3, 12 (2018), 14, 16. Car_vag: 12 (2021), 21. Both: 8. Remove: 15, 35
turfplot(community_carex, "Gud_7_3") # One Car_big (subplot 30). All other Carex in the other subplot
turfplot(community_carex, "Gud_7_4") # Seems it is Car_big in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_7_6") # Car_vag: 1, 24, 31. Car_big: 5. Car_fla: 8. Remove: 2
turfplot(community_carex, "Lav_2_2") # Seems it is Car_pil. No duplicates
turfplot(community_carex, "Lav_4_3") # Seems it is Car_vag. Duplicate with unique()
turfplot(community_carex, "Skj_5_3") # Seems it is Car_cap. No duplicates
turfplot(community_carex, "Ulv_3_3") # It is probably Car_pal. No duplicates
turfplot(community_carex, "Ulv_3_5") # It is probably Car_big. No duplicates
turfplot(community_carex, "Ulv_6_3") # Not enough information to decide. I drop it

# Epi_sp
find_plot_year(community_clean_subplot_nr_cf, "Epi_sp") # Skj_2_5 and Gud_1_5 in 2018, Lav_5_3 and Skj_3_4 in 2021
turfplot(community_clean_subplot_nr_cf, "Gud_1_5")
filter(community_clean_subplot_nr_cf, grepl("Epi_", species) & grepl("Gud_1", plotID)) |> select(plotID, year, species)
turfplot(community_clean_subplot_nr_cf, "Lav_5_3") # It is Epi_ana
turfplot(community_clean_subplot_nr_cf, "Skj_2_5") # Seems it is Epi_ana
turfplot(community_clean_subplot_nr_cf, "Skj_3_4")
filter(community_clean_subplot_nr_cf, grepl("Epi_", species) & grepl("Skj_3", plotID)) |>
  select(plotID, year, species) |>
  print(n = 32)
# Gud_1_5 and Skj_3_4 are difficult to tell. But small Epilobium can be confounded with Veronica alpina. In both cases Ver_alp is found within the same subplot, so we remove them and edit the values of Ver_alp (if needed)

# Equ_sp
find_plot_year(community_clean_subplot_nr_cf, "Equ_sp") # Skj_5_2 in 2021
# I have checked the scan, this is actually Eup_sp, there was a typo
turfplot(community_clean_subplot_nr_cf, "Skj_5_2") # It is probably Eup_wet

# Eri_sp
find_plot_year(community_clean_subplot_nr_cf, "Eri_sp") # Skj_1_4 in 2018 and 2021
turfplot(community_clean_subplot_nr_cf, "Skj_1_4")
filter(community_clean_subplot_nr_cf, grepl("Eri_", species)) |> select(plotID, year, species) # We do not have enough information, we keep it as Eri_sp

# Fes_sp
find_plot_year(community_clean_subplot_nr_cf, "Fes_sp") # Ulv_1_4 in 2018, Gud_3_5 and Skj_2_6 in 2023
turfplot(community_clean_subplot_nr_cf, "Gud_3_5") # Seems it is Fes_rub
turfplot(community_clean_subplot_nr_cf, "Skj_2_6") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Fes_", species) & grepl("Skj_2", plotID)) |> select(plotID, year, species) # It is probably Fes_rub
turfplot(community_clean_subplot_nr_cf, "Ulv_1_4") # It is not Fes_rub
filter(community_clean_subplot_nr_cf, species == "Fes_ovi" & grepl("Ulv_", plotID)) # It is probably Fes_ovi
filter(community_clean_subplot_nr_cf, species == "Fes_viv" & grepl("Ulv_", plotID)) # And not Fes_viv

# Gal_sp
find_plot_year(community_clean_subplot_nr_cf, "Gal_sp") # Ulv_5_5 and Gud_3_6 in 2022, Gud_3_6 and Gud_6_4 in 2023
turfplot(community_clean_subplot_nr_cf, "Gud_3_6") # Difficult to tell
turfplot(community_clean_subplot_nr_cf, "Gud_6_4") # Difficult to tell
filter(community_clean_subplot_nr_cf, site == "Gudmedalen" & grepl("Gal_", species)) |> select(plotID, year, species) # We do not have enough information, we keep all in Gudmedalen as Gal_sp
turfplot(community_clean_subplot_nr_cf, "Ulv_5_5") # Seems it is Gal_bor

# Gen_sp
find_plot_year(community_clean_subplot_nr_cf, "Gen_sp") # Gud_7_3 in 2019
turfplot(community_clean_subplot_nr_cf, "Gud_7_3") # Seems it is Gen_niv

# Hie_sp
find_plot_year(community_clean_subplot_nr_cf, "Hie_sp") # Gud_4_3 in 2018; Gud_4_3, Gud_7_6, Ulv_1_4, Ulv_1_3 and Ulv_6_3 in 2019; Gud_4_1, Gud_4_3 and Ulv_6_4 in 2021; Skj_2_5 in 2022; Gud_7_3 in 2023
turfplot(community_clean_subplot_nr_cf, "Gud_4_1") # Difficult to tell
turfplot(community_clean_subplot_nr_cf, "Gud_4_3") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Hie", species) & grepl("Gud_4", plotID)) |> select(plotID, year, species) # Seems they might be Hie_pil, but only found in 2018
turfplot(community_clean_subplot_nr_cf, "Gud_7_3") # Difficult to tell
turfplot(community_clean_subplot_nr_cf, "Gud_7_6") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Hie", species) & species != "Hie_sp" & grepl("Gud_", plotID)) |>
  select(plotID, year, species) |>
  print(n = 44) # Not possible to say
turfplot(community_clean_subplot_nr_cf, "Skj_2_5") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Hie", species) & grepl("Skj_", plotID)) |>
  select(plotID, year, species) |>
  print(n = 33) # Not possible to say
turfplot(community_clean_subplot_nr_cf, "Ulv_1_3") # Difficult to tell
turfplot(community_clean_subplot_nr_cf, "Ulv_1_4") # Difficult to tell
turfplot(community_clean_subplot_nr_cf, "Ulv_6_3") # Difficult to tell
turfplot(community_clean_subplot_nr_cf, "Ulv_6_4") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Hie", species) & grepl("Ulv_", plotID)) |>
  select(plotID, year, species) |>
  print(n = 27) # Not possible to say
# It is best to keep it as _sp rather than remove it

# Hyp_sp
find_plot_year(community_clean_subplot_nr_cf, "Hyp_sp") # Skj_3_3 in 2021
turfplot(community_clean_subplot_nr_cf, "Skj_3_3") # Seems it is Hyp_mac

# Leo_sp
find_plot_year(community_clean_subplot_nr_cf, "Leo_sp") # Ulv_6_1 in 2021
turfplot(community_clean_subplot_nr_cf, "Ulv_6_1") # Seems it is Leo_aut

# Oma_sp
find_plot_year(community_clean_subplot_nr_cf, "Oma_sp") # Lav_7_1 in 2021 and Skj_7_1 in 2023
turfplot(community_clean_subplot_nr_cf, "Lav_7_1") # Seems it is Oma_sup
turfplot(community_clean_subplot_nr_cf, "Skj_7_1") # Might be Oma_sup
filter(community_clean_subplot_nr_cf, grepl("Oma_", species) & grepl("Skj_7", plotID)) |> select(plotID, year, species) # It is probably Oma_sup

# Pyr_sp
find_plot_year(community_clean_subplot_nr_cf, "Pyr_rot") # Many cases. Since it is difficult to distinguish Pyr_min and Pyr_rot, we group all of them under the name Pyr_sp

# Ran_sp
find_plot_year(community_clean_subplot_nr_cf, "Ran_sp") # Lav_2_3 in 2018
turfplot(community_clean_subplot_nr_cf, "Lav_2_3") # Seems it is Ran_pyg

# Rhi_sp
find_plot_year(community_clean_subplot_nr_cf, "Rhi_sp") # Skj_1_4 in 2023
turfplot(community_clean_subplot_nr_cf, "Skj_1_4") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Rhi_", species) & grepl("Skj_1", plotID)) |> select(plotID, year, species) # It is probably Rhi_min

# Sag_sp
find_plot_year(community_clean_subplot_nr_cf, "Sag_sp") # Skj_4_1 in 2021
turfplot(community_clean_subplot_nr_cf, "Skj_4_1") # Seems it is Sag_sag

# Sal_sp
find_plot_year(community_clean_subplot_nr_cf, "Sal_sp") # Lav_2_2, Lav_3_3 and Gud_5_1 in 2021
turfplot(community_clean_subplot_nr_cf, "Gud_5_1") # Seems it is Sal_lan
turfplot(community_clean_subplot_nr_cf, "Lav_2_2") # Sal_sp does not exist in the scan. The values are not from another species. I remove it
turfplot(community_clean_subplot_nr_cf, "Lav_3_3") # Sal_sp does not exist in the scan. The values are not from another species. I remove it

# Sel_sp
find_plot_year(community_clean_subplot_nr_cf, "Sel_sp") # Skj_6_4 in 2019
turfplot(community_clean_subplot_nr_cf, "Skj_6_4") # Difficult to tell
filter(community_clean_subplot_nr_cf, grepl("Sel_", species) & grepl("Skj_6", plotID)) |>
  select(plotID, year, species) |>
  print(n = 22) # It is probably Sel_sel

# Tri_sp
find_plot_year(community_clean_subplot_nr_cf, "Tri_sp") # Lav_2_5 in 2021
turfplot(community_clean_subplot_nr_cf, "Lav_2_5") # Difficult to tell. The scan says Trifolium
filter(community_clean_subplot_nr_cf, grepl("Tri", species) & species != "Tri_ces" & site == "Lavisdalen") |> select(plotID, year, species) # It could be Tri_pra

# Vio_sp
find_plot_year(community_clean_subplot_nr_cf, "Vio_sp") # Ulv_1_5 in 2021
turfplot(community_clean_subplot_nr_cf, "Ulv_1_5") # It is probably Vio_bif
