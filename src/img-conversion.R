library(png)
library(jpeg)

setwd("C:/Users/ballen/Desktop/ScienceCentre/")

taxon.list <- list.dirs("C:/Users/ballen/Desktop/2020/images", recursive = FALSE)
taxon.name <- list.dirs("C:/Users/ballen/Desktop/2020/images", recursive = FALSE, full.names = FALSE)

for(taxon in taxon.name) {
  
  spp.list <- list.dirs(taxon.list[grep(taxon, taxon.list)])
  spp.name <- list.files(taxon.list[grep(taxon, taxon.list)], full.names = FALSE)
  
  for(spp in spp.name) {
    
    spp.path <- spp.list[grep(spp, spp.list)][1]
    fig.list <- list.files(spp.path, full.names = FALSE)
    fig.list <- fig.list[fig.list %in% c("det.png", "lin-north.png", "lin-south.png", 
                                             "map.png", "sector-north.png", "sector-south.png",
                                             "soilhf.png", "veghf.png")]
    fig.list <- paste0(spp.path, "/", fig.list)
    
    dir.create(path = gsub("C:/Users/ballen/Desktop/2020/images/", 
                           "C:/Users/ballen/Desktop/ScienceCentre/", 
                           spp.path))
    
    for (fig in fig.list) {
      
      img <- readPNG(fig)
      
      # Name change
      fig <- gsub("C:/Users/ballen/Desktop/2020/images", "C:/Users/ballen/Desktop/ScienceCentre", fig)
      
      writeJPEG(img, target = gsub(".png", ".jpg", fig), quality = 0.25)
      
    }
    
    print(spp)
    
  }
  
  print(taxon)
  
}

