#
# Title: Creation of species pages
# Created: April 25th, 2022
# Last Updated: January 12th, 2023
# Author: Brandon Allen
# Objectives: For each species we have data for, we need to create a custom page. This needs to include the following:
# 1) All necessary figures
# 2) Correct links between pages
# 3) Updated navigation bar
# 4) Comment section
# Keywords: Notes, Pages
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All figures are created using the BiodiversityBrowser repository.
# 2) The links between pages can't use periods (e.g., genus.species). This results in pages being downloaded instead of viewed.
#########
# Pages #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(googledrive)
library(tools)

# Google drive needs to be authenticated. This can be done through the drive_auth function.
# Make sure to authorize viewing/editing of your Google Drive.

# Determine the appropriate drive path. As the folder is nested within folders, you will need to search a few levels in
# before finding the folder containing the taxonomic groups.
# Only needs to be performed once on intialization
# googledrive::drive_find(n_max = 50)

# Load species lookup
load("lookup/species-lookup.Rdata")

# Temporary fix species names that end in periods
species.lookup$SpeciesID[species.lookup$SpeciesID == "Limnozetes.canadensis.s.l."] <- "Limnozetes.canadensis.s.l"
species.lookup$SpeciesID[species.lookup$SpeciesID == "Hemerocallis.Stella.de.Oro."] <- "Hemerocallis.Stella.de.Oro"
species.lookup$SpeciesID[species.lookup$SpeciesID == "Eleocharis.palustris.s.l."] <- "Eleocharis.palustris.s.l"
species.lookup$SpeciesID[species.lookup$SpeciesID == "Chenopodium.other."] <- "Chenopodium.other"

# Identify the folders for the taxonomic groups
taxon.paths <- drive_ls(path = as_id("1By3V6hOn3nP2lo_GqPNDNplIsltb0a_s"))

# Update the lookup table so we have the appropriate id for each species
species.lookup$PathID <- NA

for (taxon in unique(species.lookup$Taxon)) {
  
  species.temp <- drive_ls(path = as_id(as.character(taxon.paths[taxon.paths$name == taxon, "id"])))
  
  # Merge the ids
  species.lookup$PathID[match(species.temp$name, species.lookup$SpeciesID, nomatch = 0)] <- species.temp$id
  
  rm(species.temp)
  
}

# Loop through page creation
# Note, make sure to clear the navigation.yml upon each creation
for (taxon in unique(species.lookup$Taxon)) {
  
  # Append information to the navigation yml
  new.text <- paste0("\n", "# ", taxon, " sidebar \n",
                     taxon, ":\n  - title: Species \n    children:")
  write(new.text,
        file="_data/navigation.yml",
        append=TRUE)
  
  taxon.subset <- species.lookup[species.lookup$Taxon == taxon, ]
  
  for(species in 1:length(taxon.subset$SpeciesID)) {
    
    # Identify species
    spp <- taxon.subset$SpeciesID[species]
    
    # Identify name
    name <- taxon.subset[taxon.subset$SpeciesID == spp, "ScientificName"]
    
    # Identify display name (Use common name unless unavailable)
    display.name <- taxon.subset[taxon.subset$SpeciesID == spp, "CommonName"]
    
    if(is.na(display.name)) {
      
      display.name <- taxon.subset[taxon.subset$SpeciesID == spp, "ScientificName"]
      
    }
    
    # Create the camal case name as the page identifier
    page.identifier <- paste0(toTitleCase(strsplit(x = spp, split = "\\.")[[1]]), collapse = "")

    # Load the template 
    template  <- readLines("lookup/species-template.md")
    
    # Update names
    template <- gsub("TaxonCap", taxon, template)
    template <- gsub("SpeciesID", page.identifier, template)
    template <- gsub("SpeciesName", display.name, template)
    
    # Save as unique species
    writeLines(template, con = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"))
    
    # Check what images the species has associated wth it
    img.list <- drive_ls(path = as_id(taxon.subset[taxon.subset$SpeciesID == spp, "PathID"]))
    
    # If figures are present, append the appropriate information
    
    # Detection
    if("det.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Detection</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "1200" width = "800">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "det.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
    }
    
    # North use-availability
    if("useavail-north.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Vegetation Use Availability</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "useavail-north.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
    }
    
    # North model
    if("veghf.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Vegetation Coefficients</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "veghf.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
    }
    
    # North sector
    if("sector-north.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Forested Sector Effects</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "sector-north.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
      
    }
    
    # North linear (In the works)
    
    # South use-availability
    if("useavail-south.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Soil Use Availability</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "useavail-south.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
    }
    
    # South model
    if("soilhf.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Soil Coefficients</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "soilhf.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
    }
    
    # South sector
    if("sector-south.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Prairie Sector Effects</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "sector-south.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
      
    }
    
    # South linear (In the works)
    
    # Prediction Map
    if("map.jpeg" %in% img.list$name) {
      
      new.text <- paste0('<h2>Prediction Maps</h2>', '\n', '\n',
                         '<a href="https://drive.google.com/uc?export=view&id=UNIQUEID">', '\n',
                         '<img src="https://drive.google.com/uc?export=view&id=UNIQUEID" height = "500" width = "1000">', '\n',
                         '</a>', '\n', '\n')
      
      new.text <- gsub("UNIQUEID", as.character(img.list[img.list$name == "map.jpeg", "id"]), new.text)
      
      write(new.text,
            file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
            append = TRUE)
      
    }
    
  
    # Now that we have the completed page, update links for the navigation buttons
    
    # Identify page index
    fix.index <- 0
    if(species == 1) {
      
      previous.index <- length(taxon.subset$SpeciesID[])
      next.index <- species + 1
      fix.index <- 1
      
    }
    
    if(species == length(taxon.subset$SpeciesID)) {
      
      previous.index <- length(taxon.subset$SpeciesID) - 1
      next.index <- 1
      fix.index <- 1
      
    }
    
    if(fix.index != 1) {
      
      previous.index <- species - 1
      next.index <- species + 1
      
    }
    
    # Identify link names
    name.next <- taxon.subset$SpeciesID[next.index]
    display.next <- taxon.subset[taxon.subset$SpeciesID == name.next, "CommonName"]
    if(is.na(display.next)) {
      
      display.next <- taxon.subset[taxon.subset$SpeciesID == name.next, "ScientificName"]
      
    }
    
    name.previous <- taxon.subset$SpeciesID[previous.index]
    display.previous <- taxon.subset[taxon.subset$SpeciesID == name.previous, "CommonName"]
    if(is.na(display.previous)) {
      
      display.previous <- taxon.subset[taxon.subset$SpeciesID == name.previous, "ScientificName"]
      
    }
    
    # Convert the name.previous and name.next to camal case
    name.previous <- paste0(toTitleCase(strsplit(x = name.previous, split = "\\.")[[1]]), collapse = "")
    name.next <- paste0(toTitleCase(strsplit(x = name.next, split = "\\.")[[1]]), collapse = "")
    
    # Create the navigation link text pagenation
    new.text <- paste0('<a href="PreviousURL" class="pagination--pager" title="PreviousName">Previous</a> <a href="NextURL" class="pagination--pager" title="NextName">Next</a>',
                       '\n', '\n', '<p>&nbsp;</p>', '\n', '\n',
                       '{% include comment-section.html %}')
    
    # Update button links
    new.text <- gsub("PreviousURL", paste0("/DevelopmentWebsite/", taxon, "/species/", name.previous), new.text)
    new.text <- gsub("PreviousName", display.previous, new.text)
    new.text <- gsub("NextURL", paste0("/DevelopmentWebsite/", taxon, "/species/", name.next), new.text)
    new.text <- gsub("NextName", display.next, new.text)
    
    # Write the lines
    write(new.text,
          file = paste0("_pages/", taxon, "/", "species/", page.identifier, ".md"),
          append = TRUE)

    # Update navigation yml
    new.text <- paste0('      - title: "', display.name, '"\n',
                       "        url: /", taxon, "/species/", page.identifier)
    write(new.text,
          file="_data/navigation.yml",
          append=TRUE)
    
    rm(template, display.name, img.list)
    
    print(species)
    
  }
  
  
}


