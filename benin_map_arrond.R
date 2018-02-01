
############## 
#Arrondissements of Benin: map, surface area, pop density 
#Started 10/31/2016 
#Location: Benin dissertation folder, maps 
###############

#setwd("C:\\Users\\Sanata\\Dropbox\\01 Dissertation\\AAB_Benin\\Benin Maps")
#path <- "C:\\Users\\Sanata\\Dropbox\\tmp_BEN\\"

require(foreign)
require(maptools, quietly = T)
require(raster,quietly = T)
require(RColorBrewer, quietly = T)
require(rgeos, quietly = T)
library(gstat, quietly = T)


#Import arrondissement shapefile
shp <- shapefile("BEN_adm3.shp",stringsAsFactors = F)
projection(shp)
shp.utm <- spTransform(shp, CRS("+proj=utm +zone=31 +datum=WGS84"))

shp.utm@data$AREA <- round(gArea(shp.utm, TRUE) / 1e+06, 0)
shp.utm@data$CODEARRO <- as.numeric(shp.utm@data$CODEARRO)
dat <- shp.utm@data

#Import cities 
coords <- read.csv("benin_cities.csv", 
                   sep=",", stringsAsFactors = F)
cities <- coords[, c(3, 2)]
rownames(cities) <- coords[, 1]
cities.points <- SpatialPoints(cities, CRS("+proj=longlat +datum=WGS84"))
cities.utm <- spTransform(cities.points, CRS("+proj=utm +zone=31 +datum=WGS84"))
#Create buffer cities 
sample.cities <- c("Cotonou", "Porto-Novo", "Aplahoue", "Dogbo-Tota", "Bohicon")
buffer.cities <- cities[rownames(cities) %in% sample.cities, ]
buffers.points <- SpatialPoints(buffer.cities , CRS("+proj=longlat +datum=WGS84"))
buffers.points.utm <- spTransform(buffers.points , CRS("+proj=utm +zone=31 +datum=WGS84"))

#Import census 2013 data 
load("census_by_arrond2013_edited.RData")

#Import voting data 
load("arrond_voteshares2015_edited.RData")


#get number of households by arrondissement (2013)
cen2013$area_arr <- shp.utm@data$AREA[match(cen2013$CODEARRO , shp.utm@data$CODEARRO)]
#fill in for major cities missing 
cen2013$area[cen2013$com=="PORTO-NOVO"] <- c(22, 22, 22, 22, 22)
cen2013$area[cen2013$com=="PARAKOU"] <- c(147, 147, 147)

cen2013$hhpopdens <- ifelse(!is.na(cen2013$area_arr), round(cen2013$nombre.men / cen2013$area_arr, 0), NA)

#get pop density 2013 
cen2013$popdens <- ifelse(!is.na(cen2013$area_arr), round(cen2013$total / cen2013$area_arr, 0), NA)

#save census file 
save(cen2013, file="C:\\Users\\Sanata\\Dropbox\\01 Dissertation\\AAB_Benin\\Census\\census_by_arrond2013_edited.RData")

#add popdens info to shapefile 
shp.utm@data$popdens <- cen2013$popdens[match(shp.utm@data$CODEARRO, cen2013$CODEARRO)]
shp.utm@data$hhpopdens <- cen2013$hhpopdens[match(shp.utm@data$CODEARRO, cen2013$CODEARRO)]


#Plot densities for Southern Benin 
south.dep <- c("ATLANTIQUE", "LITTORAL", "MONO", "COUFFO", "PLATEAU", "ZOU", "OUEME")
south.com <- unique(cen2013$com[cen2013$dep %in% south.dep])
south.com.codearro <- cen2013$CODEARRO[cen2013$dep %in% south.dep]

south.shp.utm <- shp.utm[shp.utm@data$CODEARRO %in% south.com.codearro, ]

ncat <- 8
cats <- findInterval(south.shp.utm@data$hhpopdens, quantile(south.shp.utm@data$hhpopdens, seq(0, 1, 1/ncat), na.rm=T)) # find in which of our ncat categories a value falls
cats[is.na(cats)] <- 0 
cols = brewer.pal(ncat,"YlOrBr") 
cols = c("grey",cols)

#par(margin(t=4, r=4, b=6, l=4), mfrow = c(1,1))
png(filename="southern_arrond.png", 
    width = 800, height = 800, units = "px", pointsize = 12,bg = "white", res = NA)
plot(south.shp.utm, col = cols[cats+1], border=NA, main="Household Density by Arrondissement", 
     cex.main=2)
plot(south.shp.utm, add=T)
#add cities
plot(cities.utm, add=T, pch=16, col = "red", cex=1.5)
dev.off()
#add buffers
buffers <- gBuffer(buffers.points.utm, width = 25000)
plot(buffers, add=T)


#add vote margin 
cen2013$vote.diff <- lead.arr$vote.diff[match(cen2013$CODEARRO, lead.arr$CODEARRO)]

#test correlation 
cor.test(cen2013$vote.diff, cen2013$hhpopdens) #not correlated with household pop dens (not counting PN, PARAKOU, AND BOHICON II!)
cor.test(cen2013$vote.diff, cen2013$popdens) #not correlated with pop dens

#Drop districts with less than 100 HH per sq km 
south.shp.mindens100 <- south.shp.utm[south.shp.utm@data$hhpopdens >=100, ]

cen.south.over100 <- cen2013[cen2013$CODEARRO %in% south.shp.mindens100@data$CODEARRO, ]
#drop single districts
dups <- unique( south.shp.mindens100@data$NAMECOMM[duplicated( south.shp.mindens100@data$NAMECOMM)] )
cen.south.over100 <- cen.south.over100[cen.south.over100$com %in% dups, ]

summary(cen.south.over100$vote.diff)
barplot( sort(cen.south.over100$vote.diff) )

#add commune ID (22 communes)
cen.south.over100 <- with(cen.south.over100, transform(cen.south.over100,id=as.numeric(factor(com))))
colnames(cen.south.over100)[which(colnames(cen.south.over100)=="id")] <- "comid"

#pick 8 communes 
require(sampling)
test <- strata(cen.south.over100, stratanames=c("comid", "CODEARRO"), size=c(4, 2), 
       method="srswor")


#export dataset
write.csv(cen.south.over100, file="C:\\Users\\Sanata\\Dropbox\\01B_Working Docs\\pool_sample_communes.csv")


##### Distance Btwn Arrond ##### 

survey.coms <- c("OUIDAH" , "ADJARRA" , "BOHICON" , "COTONOU")
survey.arrs <- list(c(3405, 3451, 3452,3454), 
                 c(10151, 10152, 10102, 10103), 
                 c(12351, 12304, 12305), 
                 c(8152, 8153, 8160, 8161))
names(survey.arrs) <- survey.coms
survey.arrs.hq <- c(3451, 10151, 12351, 8157)
names(survey.arrs.hq) <- survey.coms

#porto 5eme to mairie: 4.67km 
#porto 3eme to mairie: 4.09 km 

final.list <- as.list(rep(NA, length(survey.coms)))

for(i in 1:length(survey.coms)){
  
  shp <- shp.utm[shp.utm@data$NAMECOMM==survey.coms[i], ] 
  arrs <- survey.arrs[[i]] 
  hq <- survey.arrs.hq[i] 
  dist <- pointDistance(coordinates(shp)[shp@data$CODEARRO==hq, ], 
                        coordinates(shp)[shp@data$CODEARRO %in% arrs, ], lonlat=F) / 1000
  final.list[[i]] <- dist
}

final.list <- unlist(final.list)
final.list <- c(final.list, NA, NA)
names(final.list)[16:17] <- c(998, 999)
arr.names <- shp.utm@data$NAMEARRO[match(as.numeric(names(final.list)), row.names(shp.utm@data))]
arr.codes <- shp.utm@data$CODEARRO[match(as.numeric(names(final.list)), row.names(shp.utm@data))]
tab <- as.data.frame(cbind(final.list, arr.names, arr.codes))
colnames(tab)[1] <- "dist.km.hq"
tab$dist.km.hq <- as.numeric(tab$dist.km.hq)
tab$arr.codes <- as.numeric(tab$arr.codes)
tab$arr.names <- as.character(tab$arr.names)
tab$dist.km.hq[16:17] <- c(4.09, 4.67)
tab$arr.names[16:17] <- c("3EME PORTO", "5EME PORTO")
tab$com <- c(rep("OUIDAH", 4), rep("ADJARRA", 4), rep("BOHICON", 3), rep("COTONOU", 4), rep("PORTO-NOVO", 2))
tab$lesser <- c(1,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,1)

my.ttest.bal(tab$dist.km.hq[tab$lesser==1], tab$dist.km.hq[tab$lesser==0], "All Communes")
sapply(1:length(unique(tab$com)), function(x) mean(tab$dist.km.hq[tab$lesser==1 & tab$com==unique(tab$com)[x]]) )
sapply(1:length(unique(tab$com)), function(x) mean(tab$dist.km.hq[tab$lesser==0 & tab$com==unique(tab$com)[x]]) )


ouidah.shp.utm <- shp.utm[shp.utm@data$NAMECOMM==survey.coms[1], ]
ouidah.survey.arrs <- c("OUIDAH I" , "OUIDAH II", "OUIDAH IV", "PAHOU")
ouidah.arr.hq <- "OUIDAH I"
ouidah.distances <- pointDistance(coordinates(ouidah.shp.utm)[ouidah.shp.utm@data$NAMEARRO==ouidah.arr.hq, ], 
              coordinates(ouidah.shp.utm)[ouidah.shp.utm@data$NAMEARRO %in% ouidah.survey.arrs, ], lonlat=F)

plot(ouidah.shp.utm)
text(coordinates(ouidah.shp.utm), ouidah.shp.utm@data$NAMEARRO)
pointDistance(coordinates(ouidah.shp.utm)[1, ], coordinates(ouidah.shp.utm)[2, ], lonlat = F)

#export tab 
tab$arr.name.survey <- c("PAHOU" , "OUIDAHI", "OUIDAHII", "OUIDAHIV", 
                         "ADJARRAI", "HONVIE" , "MALANHOUI", "ADJARRAII", 
                         "BOHICONII", "OUASSAHO", "LISSEZOUN","10emeArrondissement COTONOU" , 
                         "11emeArrondissement COTONOU", "3emeArrondissement COTONOU" , 
                         "2emeArrondissement COTONOU", "3emeArrondissement PORTO-NOVO", 
                         "5emeArrondissement PORTO-NOVO")
write.csv(tab, file="C:\\Users\\Sanata\\Dropbox\\01B_Working Docs\\Survey R Code\\survey_district_distances.csv")

###Extra code #####

### In Buffers Only ## 
intersect <- gIntersects(south.shp.utm, buffers, byid=T)
south.shp.utm@data$intersect <- intersect[1, ]

south.shp.buffers <- south.shp.utm[south.shp.utm@data$intersect==T, ]
dat3 <- south.shp.buffers@data

south.shp.buffer.over100 <- south.shp.buffers[south.shp.buffers@data$hhpopdens >= 100, ]
dat4 <- south.shp.mindens100@data

cen.south.over100 <- cen2013[cen2013$CODEARRO %in% south.shp.buffer.over100@data$CODEARRO, ]
summary(cen.south.over100$vote.diff)


#Drop single districts 
dups <- unique(south.shp.mindens100@data$NAMECOMM[duplicated(south.shp.mindens100@data$NAMECOMM)])
south.shp.final <- south.shp.mindens100[south.shp.mindens100@data$NAMECOMM %in% dups, ]
plot(south.shp.final, col="blue")
plot(buffers.points.utm , add=T, pch=16, col="red")

south.urban.coms <- unique(south.shp.final@data$NAMECOMM)
write.csv(south.urban.coms, file="C:\\Users\\Sanata\\Dropbox\\01 Dissertation\\AAB_Benin\\Benin Maps\\south_urban_coms.csv")


#scalebar(2000, xy=c(365656.8, 676900.6), type='bar', divs=4, label = t(c("0","200","400 Km")))
#add legend
allLabels <- format(quantile(south.shp.utm@data$hhpopdens,seq(0,1,1/ncat),na.rm = T), digits = 1)
labels <- paste0(allLabels[1:(length(allLabels)-1)]," - ",(allLabels[2:(length(allLabels))]))
labels <- c(paste0("No Data"),labels) # add category 0 for no data 
# display the actual legend 
legend(496693.4, 855895.7, legend=c(labels), fill=c(cols,"black"), cex=.3, 
       bty="n", title = "HH per Sq. Km")

coordinates(south.shp.utm)
text(coordinates(south.shp.utm[south.shp.utm@data$NAMECOMM=="ABOMEY-CALAVI", ]), south.shp.utm@data$NAMEARRO[south.shp.utm@data$NAMECOMM=="ABOMEY-CALAVI"])
