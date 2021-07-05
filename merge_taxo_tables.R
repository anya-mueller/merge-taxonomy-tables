## Goal: merge two taxonomic tables
## Author: Anya Mueller
## Date: July 5, 2021


# Set up ------------------------------------------------------------------
## install packages
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("here")

## read in packages
library(tidyverse)
library(janitor)
library(here)

## read in files
co1_taxo_custom <- read.delim(here("taxonomy_table.CO1.RDP_customDB.txt"), 
                              h=TRUE,
                              fill = TRUE)

co1_taxo_96 <- read.delim(here("taxonomy_table.CO1.NCBI_NT+customDB.96sim.rm_unknown_hits.txt"), 
                                h=TRUE, 
                                fill = TRUE)

## clean up tables
co1_taxo_custom <- co1_taxo_custom %>%
  #match names to the other taxonomic file
  rename("query" = "row_names",
         "superkingdom" = "Rank1", # eukaryota = superkingdom
         "kingdom" = "Rank2", # metazoa  = kingdom
         "phylum" = "Rank3", # Arthropoda = phylum
         "class" = "Rank4", # Hexanauplia = class
         "order" = "Rank5", # Calanoida = order
         "family"  = "Rank6", # Acartiidae = family
         "genus" = "Rank7", # Acartia = genus
         "spe" = "Species") %>% # half of species names 
  #clean names up
  clean_names() %>%
  #make genus species column
  #replace non-taxonomically significant values with NAs
  mutate(spe = str_replace(string = spe, 
                               pattern = "sp\\.", # \\. = escape character for a period 
                               replacement = NA_character_)) %>% 
  mutate(spe = str_replace(string = spe, 
                               pattern = "aff\\.", # \\. = escape character for a period 
                               replacement = NA_character_)) %>%
  mutate(species = if_else(condition = is.na(spe)|is.na(genus), 
                           # if there is an NA in the species/ genus column then put an NA 
                           true = NA_character_,
                           # if not, then combine genus and species 
                           false = paste(genus,
                                         spe, 
                                         sep = " ")))

colnames(co1_taxo_96) <- colnames(co1_taxo_96) %>%
  #remove X. from column names
  str_remove_all(., "X.") 
co1_taxo_96 <- co1_taxo_96 %>% 
  #clean up column names
  clean_names() %>%
  #remove rows with no identification
  filter(method != "filtered out") %>%
  #clean up species row - replace incomplete names with NAs
  mutate(species = str_replace_all(species, 
                                   pattern = "sp\\.", # \\. = escape character for a period 
                                   replacement = NA_character_)) %>%
  mutate(species = str_replace_all(species, 
                                   pattern = "cf\\.", # \\. = escape character for a period 
                                   replacement = NA_character_)) %>%
  mutate(species = str_replace_all(species, 
                                   pattern = "aff\\.", # \\. = escape character for a period 
                                   replacement = NA_character_)) %>%
  #fix assignments
  rename("superkingdom" = "kingdom") %>%
  mutate(superkingdom = str_replace(superkingdom,
                      pattern = "Viridiplantae", # Viridiplantae is a clade of eukaryotes
                      replacement = "Eukaryota")) %>%
  #replace NA with "NA" for if_else statement
  mutate(phylum = str_replace_na(phylum)) %>%
  #populate kingdom column with Metazoa
  mutate(kingdom = if_else(condition = phylum == "Arthropoda",
                           true = "Metazoa",
                           false = if_else(condition = phylum == "Echinodermata",
                                           true = "Metazoa",
                                           false = if_else(condition = phylum == "Annelida",
                                                           true = "Metazoa",
                                                           false = if_else(condition = phylum == "Cnidaria",
                                                                           true = "Metazoa",
                                                                           false = if_else(condition = phylum == "Platyhelminthes",
                                                                                           true = "Metazoa",
                                                                                           false = if_else(condition = phylum == "Mollusca",
                                                                                                           true = "Metazoa",
                                                                                                           false = if_else(condition = phylum == "Porifera",
                                                                                                                           true = "Metazoa",
                                                                                                                           false = if_else(condition = phylum == "Nemertea",
                                                                                                                                           true = "Metazoa",
                                                                                                                                           false = if_else(condition = phylum == "Chordata",
                                                                                                                                                           true = "Metazoa",
                                                                                                                                                           false = if_else(condition = phylum == "Bryozoa",
                                                                                                                                                                           true = "Metazoa",
                                                                                                                                                                           false = if_else(condition = phylum == "Ctenophora",
                                                                                                                                                                                           true = "Metazoa",
                                                                                                                                                                                           false = "NA")))))))))))) %>%
  #replace "NA" with NA_character_
  mutate(kingdom = str_replace(string = kingdom,
                               pattern = "^NA$",
                               replacement = NA_character_))
  
# add column to specify database
co1_taxo_96 <- co1_taxo_96%>%
  mutate(database = "NCBIBOLD") 
co1_taxo_custom <- co1_taxo_custom %>%
  mutate(database = "custom")                                                                                                                                                          


# Merge tables ------------------------------------------------------------
# Let's merge the two taxonomy tables, with conflicts being resolved...
# ...by taking the assignment from the table we are more confident in

#merge the two taxonomy tables
co1_taxo_all <- full_join(x = co1_taxo_custom,
                          y = co1_taxo_96,
                          by = "query") %>% 
  mutate(across(cols = c(ends_with(".x"),
                         ends_with(".y")),
                .fns = ~ str_replace_na(.))) %>% # make NA_characters into "NA" because the if_else() statements need the same structure of item in order to work (otherwise just get an NA_character returned)
  mutate(superkingdom = if_else(condition = 
                                  #check if they have the same value
                                  superkingdom.x == superkingdom.y, #check if they have the same value
                                #if yes, then we will take superkingdom.x as the superkingdom value (it doesn't matter which one we take)
                                true = superkingdom.x, 
                                false = if_else(condition = 
                                                  #if not check if superkingdom.x is an NA
                                                  superkingdom.x == "NA", 
                                                #if yes then we will take the superkingdom value from superkingdom.y
                                                true = superkingdom.y, 
                                                #if not, then both superkingdom.x and .y have superkingdom values that are not equivalent...
                                                #...we will take the value from the custom database
                                                false = superkingdom.x))) %>% 
  mutate(kingdom = if_else(condition = 
                             #check if they have the same value
                             kingdom.x == kingdom.y,
                           #if yes, then we will take kingdom.x as the kingdom value (it doesn't matter which one we take)
                           true = kingdom.x, 
                           false = if_else(condition = 
                                             #if not, check if kingdom.x is an NA
                                             kingdom.x == "NA", 
                                           #if yes then we will take the kingdom value from kingdom.y
                                           true = kingdom.y, 
                                           #if not, then both kingdom.x and .y have kingdom values that are not equivalent...
                                           #...we will take the value from the custom database
                                           false = kingdom.x))) %>% 
  mutate(phylum = if_else(condition = 
                            #check if they have the same value
                            phylum.x == phylum.y, 
                          #if yes, then we will take phylum.x as the phylum value (it doesn't matter which one we take)
                          true = phylum.x, 
                          false = if_else(condition = 
                                            #if not, check if phylum.x is an NA
                                            phylum.x == "NA", 
                                          #if yes then we will take the phylum value from phylum.y
                                          true = phylum.y, 
                                          #if not, then both phylum.x and .y have phylum values that are not equivalent...
                                          #...we will take the value from the custom database
                                          false = phylum.x))) %>% 
  mutate(class = if_else(condition = 
                           #check if they have the same value
                           class.x == class.y, 
                         #if yes, then we will take class.x as the class value (it doesn't matter which one we take)
                         true = class.x,
                         false = if_else(condition = 
                                           #if not, check if class.x is an NA
                                           class.x == "NA", 
                                         #if yes then we will take the class value from class.y
                                         true = class.y, 
                                         #if not, then both class.x and .y have class values that are not equivalent...
                                         #...we will take the value from the custom database
                                         false = class.x))) %>% 
  mutate(order = if_else(condition = 
                           #check if they have the same value
                           order.x == order.y, 
                         #if yes, then we will take order.x as the order value (it doesn't matter which one we take)
                         true = order.x, 
                         false = if_else(condition = 
                                           # if not check if order.x is an NA
                                           order.x == "NA", 
                                         #if yes then we will take the order value from order.y
                                         true = order.y, 
                                         #if not, then both order.x and .y have order values that are not equivalent...
                                         #...so we will take the value from the custom database
                                         false = order.x))) %>% 
  mutate(family = if_else(condition = 
                            #check if they have the same value
                            family.x == family.y, 
                          #if yes, then we will take family.x as the family value (it doesn't matter which one we take)
                          true = family.x, 
                          false = if_else(condition = 
                                            #if not, check if family.x is an NA
                                            family.x == "NA", 
                                          #if yes then we will take the family value from family.y
                                          true = family.y, 
                                          #if not, then both family.x and .y have family values that are not equivalent...
                                          #...we will take the value from the custom database
                                          false = family.x))) %>% 
  mutate(genus = if_else(condition = 
                           #check if they have the same value
                           genus.x == genus.y, 
                         #if yes, then we will take genus.x as the genus value (it doesn't matter which one we take)
                         true = genus.x, 
                         false = if_else(condition = 
                                           #if not, check if genus.x is an NA
                                           genus.x == "NA", 
                                         #if yes then we will take the genus value from genus.y
                                         true = genus.y, 
                                         #if not, then both genus.x and .y have genus values that are not equivalent...
                                         #...so we will take the value from the custom database
                                         false = genus.x))) %>% 
  mutate(species = if_else(condition = 
                             #check if they have the same value
                             species.x == species.y, 
                           #if yes, then we will take species.x as the species value (it doesn't matter which one we take)
                           true = species.x, 
                           false = if_else(condition = 
                                             #if not check if species.x is an NA
                                             species.x == "NA", 
                                           #if yes then we will take the species value from species.y
                                           true = species.y, 
                                           #if not, then both species.x and .y have species values that are not equivalent...
                                           #...so we will take the value from the custom database
                                           false = species.x))) %>% 
  #replace "NA" with NA_character_
  mutate(across(.fns = ~ str_replace(.,
                                     "^NA$", # exact match to NA; ^ = starts with,  $ = ends with
                                     NA_character_))) %>%
  unite(col = database,
        starts_with("database"),
        sep = ",",
        na.rm = TRUE) %>% #merge the "database" columns, but not if the value is NA
  select(-ends_with(".x"),
         -ends_with(".y")) 


# Identify merge conflicts ------------------------------------------------
# Let's identify the taxonomic merge conflicts between the two taxonomic tables

co1_taxo_all_w_dis <- full_join(x = co1_taxo_custom,
                          y = co1_taxo_96,
                          by = "query") %>% #merge the two taxonomy tables
  mutate(across(cols = c(ends_with(".x"),
                         ends_with(".y")),
                .fns = ~ str_replace_na(.))) %>% # make NA_characters into "NA" because the if_else() statements need the same structure of item in order to work (otherwise jsut get an NA_character returned)
  mutate(superkingdom = if_else(condition = superkingdom.x == superkingdom.y, #check if they have the same value
                                true = superkingdom.x, #if yes, then we will take superkingdom.x as the superkingdom value (it doesn't matter which one we take)
                                false = if_else(condition = superkingdom.x == "NA", #check if superkingdom.x is an NA
                                                true = superkingdom.y, #if yes then we will take the superkingdom value from superkingdom.y
                                                false = if_else(condition = superkingdom.y == "NA", #check if superkingdom.y is an NA
                                                                true = superkingdom.x, #if yes we will take the superkingdom from superkingdom.x
                                                                false = paste(superkingdom.x,
                                                                              superkingdom.y,
                                                                              sep = ","))))) %>% #if not, then both superkingdom.x and .y have superkingdom values that are not equivalent, so we will take both
  mutate(kingdom = if_else(condition = kingdom.x == kingdom.y, #check if they have the same value
                           true = kingdom.x, #if yes, then we will take kingdom.x as the kingdom value (it doesn't matter which one we take)
                           false = if_else(condition = kingdom.x == "NA", #check if kingdom.x is an NA
                                           true = kingdom.y, #if yes then we will take the kingdom value from kingdom.y
                                           false = if_else(condition = kingdom.y == "NA", #check if kingdom.y is an NA
                                                           true = kingdom.x, #if yes we will take the kingdom from kingdom.x
                                                           false = paste(kingdom.x,
                                                                         kingdom.y,
                                                                         sep = ","))))) %>% #if not, then both kingdom.x and .y have kingdom values that are not equivalent, so we will take both
  mutate(phylum = if_else(condition = phylum.x == phylum.y, #check if they have the same value
                          true = phylum.x, #if yes, then we will take phylum.x as the phylum value (it doesn't matter which one we take)
                          false = if_else(condition = phylum.x == "NA", #check if phylum.x is an NA
                                          true = phylum.y, #if yes then we will take the phylum value from phylum.y
                                          false = if_else(condition = phylum.y == "NA", #check if phylum.y is an NA
                                                          true = phylum.x, #if yes we will take the phylum from phylum.x
                                                          false = paste(phylum.x,
                                                                        phylum.y,
                                                                        sep = ","))))) %>% #if not, then both phylum.x and .y have phylum values that are not equivalent, so we will take both
  mutate(class = if_else(condition = class.x == class.y, #check if they have the same value
                         true = class.x, #if yes, then we will take class.x as the class value (it doesn't matter which one we take)
                         false = if_else(condition = class.x == "NA", #check if class.x is an NA
                                         true = class.y, #if yes then we will take the class value from class.y
                                         false = if_else(condition = class.y == "NA", #check if class.y is an NA
                                                         true = class.x, #if yes we will take the class from class.x
                                                         false = paste(class.x,
                                                                       class.y,
                                                                       sep = ","))))) %>% #if not, then both class.x and .y have class values that are not equivalent, so we will take both
  mutate(order = if_else(condition = order.x == order.y, #check if they have the same value
                         true = order.x, #if yes, then we will take order.x as the order value (it doesn't matter which one we take)
                         false = if_else(condition = order.x == "NA", #check if order.x is an NA
                                         true = order.y, #if yes then we will take the order value from order.y
                                         false = if_else(condition = order.y == "NA", #check if order.y is an NA
                                                         true = order.x, #if yes we will take the order from order.x
                                                         false = paste(order.x,
                                                                       order.y,
                                                                       sep = ","))))) %>% #if not, then both order.x and .y have order values that are not equivalent, so we will take both
  mutate(family = if_else(condition = family.x == family.y, #check if they have the same value
                          true = family.x, #if yes, then we will take family.x as the family value (it doesn't matter which one we take)
                          false = if_else(condition = family.x == "NA", #check if family.x is an NA
                                          true = family.y, #if yes then we will take the family value from family.y
                                          false = if_else(condition = family.y == "NA", #check if family.y is an NA
                                                          true = family.x, #if yes we will take the family from family.x
                                                          false = paste(family.x,
                                                                        family.y,
                                                                        sep = ","))))) %>% #if not, then both family.x and .y have family values that are not equivalent, so we will take both
  mutate(genus = if_else(condition = genus.x == genus.y, #check if they have the same value
                         true = genus.x, #if yes, then we will take genus.x as the genus value (it doesn't matter which one we take)
                         false = if_else(condition = genus.x == "NA", #check if genus.x is an NA
                                         true = genus.y, #if yes then we will take the genus value from genus.y
                                         false = if_else(condition = genus.y == "NA", #check if genus.y is an NA
                                                         true = genus.x, #if yes we will take the genus from genus.x
                                                         false = paste(genus.x,
                                                                       genus.y,
                                                                       sep = ","))))) %>% #if not, then both genus.x and .y have genus values that are not equivalent, so we will take both
  mutate(species = if_else(condition = species.x == species.y, #check if they have the same value
                           true = species.x, #if yes, then we will take species.x as the species value (it doesn't matter which one we take)
                           false = if_else(condition = species.x == "NA", #check if species.x is an NA
                                           true = species.y, #if yes then we will take the species value from species.y
                                           false = if_else(condition = species.y == "NA", #check if species.y is an NA
                                                           true = species.x, #if yes we will take the species from species.x
                                                           false = paste(species.x,
                                                                         species.y,
                                                                         sep = ","))))) %>% #if not, then both species.x and .y have species values that are not equivalent, so we will take both
  mutate(across(.fns = ~ str_replace(.,
                                     "^NA$", # exact match to NA; ^ = starts with,  $ = ends with
                                     NA_character_))) %>%
  unite(col = database,
        starts_with("database"),
        sep = ",",
        na.rm = TRUE) %>% #merge the "database" columns, but not if the value is NA
  select(-ends_with(".x"),
         -ends_with(".y")) #remove duplicated columns

#make column to indicate disagreements in taxonomies - detect if there exists a comma in any level of taxonomy, if yes put a 1, if no, put a 0 
co1_taxo_all_w_dis <- co1_taxo_all_w_dis %>% 
  mutate(across(.fns = ~ str_replace_na(.))) %>% # turn NA_character into NA so that if_else can function properly
  mutate(taxo_disagreement = if_else(condition = str_detect(string = superkingdom,
                                                            pattern = ","),
                                     true = 1,
                                     false = if_else(condition = str_detect(string = kingdom, 
                                                                            pattern = ","),
                                                     true = 1,
                                                     false = if_else(condition = str_detect(string = phylum, 
                                                                                            pattern = ","),
                                                                     true = 1,
                                                                     false = if_else(condition = str_detect(string = class, 
                                                                                                            pattern = ","),
                                                                                     true = 1,
                                                                                     false = if_else(condition = str_detect(string = order, 
                                                                                                                            pattern = ","),
                                                                                                     true = 1,
                                                                                                     false = if_else(condition = str_detect(string = family, 
                                                                                                                                            pattern = ","),
                                                                                                                     true = 1,
                                                                                                                     false = if_else(condition = str_detect(string = genus, 
                                                                                                                                                            pattern = ","),
                                                                                                                                     true = 1,
                                                                                                                                     false = if_else(condition = str_detect(string = species, 
                                                                                                                                                                            pattern = ","),
                                                                                                                                                     true = 1,
                                                                                                                                                     false = 0 ))))))))) %>%
  mutate(across(.fns = ~ str_replace(.,
                                     "^NA$", # exact match to NA; ^ = starts with,  $ = ends with
                                     NA_character_)))
