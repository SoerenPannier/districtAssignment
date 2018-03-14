library("maptools")

shpFile <- readShapePoly("Data/Bez250/Bezirksgrenzen_1250.000.shp")
plot(shpFile)
sort(shpFile@data$PB)

load(file = "Data/probabilities_districtsNew.RData")

shpFile@data$PB <- gsub(pattern = "Ã¼", 
                        replacement = "ü", 
                        x = shpFile@data$PB)
shpFile@data$PB <- gsub(pattern = "Ã¶", 
                        replacement = "ö", 
                        x = shpFile@data$PB)
shpFile@data$PB <- gsub(pattern = "Ã¤", 
                        replacement = "ä", 
                        x = shpFile@data$PB)
shpFile@data$PB[!shpFile@data$PB %in% regionsNew$District]
shpFile@data$PB <- gsub(pattern = "\\(Stadt\\)", 
                        replacement = " \\(Stadt\\)", 
                        x = shpFile@data$PB)
shpFile@data$PB <- gsub(pattern = "\\(Land\\)", 
                        replacement = " \\(Land\\)", 
                        x = shpFile@data$PB)
View(cbind.data.frame(sort(shpFile@data$PB), sort(regionsNew$District)))
shpFile@data$PB[!shpFile@data$PB %in% regionsNew$District] <-
  c("Klagenfurt (Land)", "Innsbruck (Stadt)", "Innsbruck (Land)", "Wien")

all(sort(shpFile@data$PB) == sort(as.character(regionsNew$District)))
# shapefile:
# http://data-synergis.opendata.arcgis.com/datasets/bb4acc011100469185d2e59fa4cae5fc_0
# Lizenzinformationen
# Creative Commons Namensnennung - Weitergabe unter gleichen Bedingungen 2.0
shpFile@data$BL <- gsub(pattern = "Ã¤", 
                        replacement = "ä", 
                        x = shpFile@data$BL)
shpFile@data$BL <- gsub(pattern = "Ã¶", 
                        replacement = "ö", 
                        x = shpFile@data$BL)
shpFile@data$ST <- gsub(pattern = "Ã–", 
                        replacement = "Ö", 
                        x = shpFile@data$ST)


shape_austria_dis <- shpFile
save(shape_austria_dis, file = "Data/shape_austria_dis.rds", compress = "xz")

