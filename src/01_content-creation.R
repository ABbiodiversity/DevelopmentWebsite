#
# Title: Creation of species pages
# Created: April 25th, 2022
# Last Updated: April 26th, 2022
# Author: Brandon Allen
# Objectives: Create pages for species
# Keywords: Notes, Pages
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All figures are created through the Custom Reporting repository to avoid duplication of scripts and files.
#
#########
# Pages #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(googledrive)
library(tools)

# Figuring out drive paths
# googledrive::drive_find(n_max = 50)
# drive_find(shared_drive = as_id("1yNYw_DPHqlbAfB5chErGKqhPU1TKpcSl"))
# drive_ls(path = as_id("0AB7wwTMFE0L1Uk9PVA"))
# drive_ls(path = as_id("1uNgDKd-l5E9PMpXBG0iNW2_gI_06AKKo"))
# drive_ls(path = as_id("1By3V6hOn3nP2lo_GqPNDNplIsltb0a_s"))
# drive_ls(path = as_id("1GBhDa6rG6LpCuI2if78HReHOjzbUZKsg"))
# drive_ls(path = as_id("1afVqNnnkyDeNCzHOayETQpHT6CBAmWHk"))

# Load species lookup
species.lookup <- read.csv("lookup/species-lookup.csv")

# Identify species with information
drive.paths <- drive_ls(path = as_id("1GBhDa6rG6LpCuI2if78HReHOjzbUZKsg"))

# Align lookup table with available species list
species.lookup <- species.lookup[species.lookup$sppid %in% drive.paths$name, ]

# Loop through page creation

for (taxon in unique(species.lookup$taxon)) {
  
  # Append information to the navigation yml
  
  new.text <- paste0("# ", taxon, " sidebar \n",
                     taxon, ":\n  - title: Species \n    children:")
  write(new.text,
        file="_data/navigation.yml",
        append=TRUE)
  
  for(spp in species.lookup$sppid[1:3]) {
    
    # Identify display name
    display.name <- species.lookup[species.lookup$sppid == spp, "display"]
    
    # Check if species has north/south results
    img.list <- drive_ls(path = as_id(as.character(drive.paths[drive.paths$name == spp, "id"])))
    
    if("veghf.jpg" %in% img.list$name) {models <- "veg"}
    if("soilhf.jpg" %in% img.list$name) {models <- "soil"}
    if("veghf.jpg" %in% img.list$name & "soilhf.jpg" %in% img.list$name) {models <- "combo"}
    if(length(img.list$name) == 1) {models = "detect"}
    
    # Load appropriate template
    template  <- readLines(paste0("lookup/species-template-", models, ".md"))
    
    # Update names
    template <- gsub("TaxonLow", taxon, template)
    template <- gsub("TaxonCap", toTitleCase(taxon), template)
    template <- gsub("SpeciesID", spp, template)
    template <- gsub("SpeciesName", display.name, template)
    
    # Update IDS
    template <- gsub("DETECTION", as.character(img.list[img.list$name == "det.jpg", "id"]), template)
    template <- gsub("VEGHF", as.character(img.list[img.list$name == "veghf.jpg", "id"]), template)
    template <- gsub("LINNORTH", as.character(img.list[img.list$name == "lin-north.jpg", "id"]), template)
    template <- gsub("SENORTH", as.character(img.list[img.list$name == "sector-north.jpg", "id"]), template)
    template <- gsub("SOILHF", as.character(img.list[img.list$name == "soilhf.jpg", "id"]), template)
    template <- gsub("LINSOUTH", as.character(img.list[img.list$name == "lin-south.jpg", "id"]), template)
    template <- gsub("SESOUTH", as.character(img.list[img.list$name == "sector-south.jpg", "id"]), template)
    template <- gsub("MAPID", as.character(img.list[img.list$name == "map.jpg", "id"]), template)
    
    writeLines(template, con = paste0("_pages/", taxon, "/", "species/", spp, ".md"))
    
    # Update navigation yml
    new.text <- paste0('      - title: "', display.name, '"\n',
                       "        url: /", taxon, "/species/", spp)
    write(new.text,
          file="_data/navigation.yml",
          append=TRUE)
    
    rm(template, display.name, img.list)
    
  }
  
  
}


