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
