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
library("tools")

# Load species lookup
species.lookup <- read.csv("lookup/species-lookup.csv")

# Align lookup table with available species list
species.lookup <- species.lookup[species.lookup$sppid %in% list.files("assets/images/birds/"), ]

# Loop through page creation

for (taxon in unique(species.lookup$taxon)) {
  
  # Append information to the navigation yml
  
  new.text <- paste0("# ", taxon, " sidebar \n",
                     taxon, ":\n  - title: Species \n    children:")
  write(new.text,
        file="_data/navigation.yml",
        append=TRUE)
  
  for(spp in species.lookup$sppid) {
    
    # Identify display name
    display.name <- species.lookup[species.lookup$sppid == spp, "display"]
    
    # Check if species has north/south results
    img.list <- list.files(path = paste0("assets/images/", taxon, "/", spp, "/"))
    
    if("veghf.jpg" %in% img.list) {models <- "veg"}
    if("soilhf.jpg" %in% img.list) {models <- "soil"}
    if("veghf.jpg" %in% img.list & "soilhf.jpg" %in% img.list) {models <- "combo"}
    if(length(img.list) == 1) {models = "detect"}
    
    # Load appropriate template
    template  <- readLines(paste0("lookup/species-template-", models, ".md"))
    template <- gsub("TaxonLow", taxon, template)
    template <- gsub("TaxonCap", toTitleCase(taxon), template)
    template <- gsub("SpeciesID", spp, template)
    template <- gsub("SpeciesName", display.name, template)
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


