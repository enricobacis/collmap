#!/usr/bin/env Rscript

library(maps)
library(ggmap)
library(geosphere)

outfile <- 'output.pdf'
geo_infile <- 'geo.csv'  # if it does not exist will be created
atenei_infile <- 'atenei.csv'

# add custom fixes
f_ateneo <- c("LIUC - CASTELLANZA", "Scuola Superiore Sant'Anna", "Cattolica del Sacro Cuore", "TUSCIA", "Univ. Telematica GIUSTINO FORTUNATO", "Politecnica delle MARCHE", "ROMA TRE", "SALENTO", "Univ. Telematica GUGLIELMO MARCONI", "Univ. Telematica Internazionale UNINETTUNO", "Univ. Telematica E-CAMPUS", "Univ. Telematica UNITELMA SAPIENZA", "Univ. Telematica PEGASO")
f_lon    <- c(8.8997403, 10.4005972, 9.1749898,  12.0904487, 14.7974033, 13.5105872, 12.4773732, 18.1650782, 12.4624389, 12.4755283, 9.1181806, 12.5149003, 14.2461277)
f_lat    <- c(45.610585, 43.7209661, 45.4621325, 42.4274713,  41.120348, 43.6171442, 41.8620534, 40.3495599, 41.9073661, 41.8955925, 45.6982225, 41.904946, 40.8371617)

# Color
pal <- colorRampPalette(c("#33cc3320", "#cccc3360", "#ff9933a0", "#ff0000ff"), alpha=TRUE)
colors <- pal(200)


print('loading data')
collabs <- read.csv(atenei_infile, header=TRUE)
collabs <- collabs[order(collabs$numcollaborazioni),]     # sort by numcollaborazioni
maxcnt <- max(collabs$numcollaborazioni)

print('loading geo data')
if (file.exists(geo_infile)) {
    print('geocoding skipped (db already present)')
    geo <- read.csv(geo_infile)
} else {
    print('geocoding data')
    ateneo <- unique(c(as.character(collabs$ateneo), as.character(collabs$ateneo.1)))
    geo <- data.frame(ateneo, geocode(as.character(ateneo)))

    print('integrating fixes')
    fixes <- data.frame(f_ateneo, f_lon, f_lat)
    matches <- match(fixes$f_ateneo, geo$ateneo)
    geo[matches, 2:3] = fixes[2:3]

    print('saving geocoded db')
    write.csv(geo, file=geo_infile, row.names=FALSE)
}

print('opening file for writing')
pdf(outfile)

print('drawing base map')
map(regions="Italy", col="#191919", fill=TRUE, bg="#000000", lwd=0.25)

print('drawing connections')
for (i in 1:nrow(collabs)) {
    row <- collabs[i,]
    atn1 <- geo[geo$ateneo == as.character(row$ateneo),]
    atn2 <- geo[geo$ateneo == as.character(row$ateneo.1),]
    num <- row$numcollaborazioni
    line <- gcIntermediate(atn1[2:3], atn2[2:3], n=100, addStartEnd=TRUE)
    colindex <- round((num / maxcnt) * length(colors))
    width <- 0.5 + log(num, maxcnt/10)
    lines(line, col=colors[colindex], lwd=width)
}

print('closing file')
invisible(dev.off())
