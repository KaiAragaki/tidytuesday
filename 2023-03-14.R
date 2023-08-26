d <- tt_load("2023-03-14")
data <- d$drugs

data |> 
  filter(!is.na(species)) |> 
  mutate(species = str_remove_all(species, " \\([^;]*"),
         species = str_replace(species, "^Calves.*", "Calves"),
         species = str_replace(species, "Male pigs; Female pigs", "Pigs"),
         species = str_replace(species, "Salmonidae", "Atlantic salmon"),
         species = str_replace_all(species, "Sows", "Pigs")) |> 
  rowwise() |> 
  mutate(species = str_split(species, "; ")[[1]] |> sort() |> paste0(collapse = ", "),
         species = ifelse(str_detect(species, "eggs"), "Chicken", species)) |> 
  ungroup() |> 
  mutate(species = ifelse(species == "Honey bees", "Bees", species),
         species = ifelse(species == "Cattle, Cattle", "Cattle", species)) |> 
  separate_longer_delim(species, ", ") |> 
  filter(species != "Raccoon dogs") |>  # Raccoon dogs == Red foxes for all drugs (same animal?)
  mutate(species = ifelse(species %in% c("Pheasants", "Chicken", "Turkeys", "Ducks"), "Avian", species)) |> 
  mutate(company = marketing_authorisation_holder_company_name |> 
           str_remove_all("\\.") |> 
           str_squish() |> 
           tolower() |> 
           str_replace_all("limited", "ltd") |> 
           iconv(to = "ASCII//TRANSLIT") |> 
           str_remove_all("[',~]") |> 
           str_remove(" (sa|bv|ltd|nv|ab|oy|co)$") |> 
           str_squish()) |>
  mutate(company = fct_infreq(company)) |> 
  ggplot(aes(company, fill = species)) + 
  geom_bar() +
  facet_grid(rows = vars(species)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none")

# Remember to avoid double counting - select for unique animal/drug combos
