library(ggmap, quietly = T)
library(tidyverse, quietly = T)

load('../Data/dataArr.RData')
load('../Data/ind_prec_df.rda')
load('../Data/indexList_MAIN.RData')
load('../Data/nycSub.RData')
load('../Data/totalStreetBuffInfo_NEW.RData')

# Year choice
dataArrF = dataArrF[dataArrF$year == 2014, ]

# Figure 1 ---------------------------------------------------------------------
png(filename = "../Plots/prec77arrLoc1.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)

prec_num = 49

plot(nycSub[prec_num, ], lty = 2)
points(dataArrF$x_coord_cd[dataArrF$arrest_precinct == nycSub$Precinct[prec_num]],
       dataArrF$y_coord_cd[dataArrF$arrest_precinct == nycSub$Precinct[prec_num]],
       col = 'blue', cex = 2.5)
dev.off()

pdf("../Plots/prec77arrLoc1_scale.pdf", width = 20, height = 10)
par(mfrow=c(1,2))
plot(nycSub[prec_num, ], lty = 2)
points(dataArrF$x_coord_cd[dataArrF$arrest_precinct == nycSub$Precinct[prec_num]],
       dataArrF$y_coord_cd[dataArrF$arrest_precinct == nycSub$Precinct[prec_num]],
       col = 'blue', cex = 1)

plot(nycSub[prec_num, ], lty = 2)
df_temp <- data.frame(x = dataArrF$x_coord_cd[dataArrF$arrest_precinct == nycSub$Precinct[prec_num]], 
                      y = dataArrF$y_coord_cd[dataArrF$arrest_precinct == nycSub$Precinct[prec_num]])

df_temp2 <- cbind(unique(df_temp), value = with(df_temp, tapply(x, paste(x,y), length)))
points(y ~ x, cex = value^(1/3), data = df_temp2, col = 'blue')
dev.off()

png(filename = "../Plots/prec77arrLoc2.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)
dataArrF_fig1 = dataArrF[dataArrF$arrest_precinct == nycSub$Precinct[prec_num], ]

qmplot(lon, lat, data = dataArrF_fig1, alpha = I(.9), size = I(8), colour = 'darkred', darken = .1, source = "osm") +
  theme(legend.position = "none")

dev.off()

# Supplement (Sec. 6) Full NYC w/ streets --------------------------------------
png(filename = "../Plots/NYCwStreets.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)
load('../../NegControl/Data/streetsByPrec.RData')
plot(nycSub, border = 'black', lwd = 2)
for(i in 1:length(streetsByPrec)) {
    lines(streetsByPrec[[i]], lwd = 0.5, col = 'red')
}
dev.off()
# Figure 2 ---------------------------------------------------------------------

# Borders ind1, ind2
ind1 = 98; ind2 = 104
ind_choice = ind_prec_df[c(ind1, ind2), ]
arr_choice = dataArrF[dataArrF$arrest_precinct %in% c(ind_choice[1,], ind_choice[2, ]), ]

png(filename = "../Plots/arrestsAndBuffer.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)
par(mfrow = c(1,2))

# ind1 first
nyc_small = nycSub[c(which(nycSub$Precinct == ind_choice[1,1]),
                     which(nycSub$Precinct == ind_choice[1,2])), ]

plot(totalStreetBuffInfo_NEW[[5]][[ind1]]$poly2, lwd = 3)
plot(totalStreetBuffInfo_NEW[[5]][[ind1]]$poly1, add = T, lwd = 3)
plot(nyc_small, lty = 2, add = T)
points(arr_choice$x_coord_cd[arr_choice$arrest_precinct == ind_choice[1,1]],
       arr_choice$y_coord_cd[arr_choice$arrest_precinct == ind_choice[1,1]],
       col = 'red', cex = 2.5)
points(arr_choice$x_coord_cd[arr_choice$arrest_precinct == ind_choice[1,2]],
       arr_choice$y_coord_cd[arr_choice$arrest_precinct == ind_choice[1,2]],
       col = 'blue', cex = 2.5)

# ind2 second
nyc_small = nycSub[c(which(nycSub$Precinct == ind_choice[2,1]),
                     which(nycSub$Precinct == ind_choice[2,2])), ]

plot(totalStreetBuffInfo_NEW[[5]][[ind2]]$poly1, lwd = 3)
plot(totalStreetBuffInfo_NEW[[5]][[ind2]]$poly2, add = T, lwd = 3)
plot(nyc_small, lty = 2, add = T)
points(arr_choice$x_coord_cd[arr_choice$arrest_precinct == ind_choice[2,1]],
       arr_choice$y_coord_cd[arr_choice$arrest_precinct == ind_choice[2,1]],
       col = 'red', cex = 2.5)
points(arr_choice$x_coord_cd[arr_choice$arrest_precinct == ind_choice[2,2]],
       arr_choice$y_coord_cd[arr_choice$arrest_precinct == ind_choice[2,2]],
       col = 'blue', cex = 2.5)

dev.off()

# Figure 6 (streets) -----------------------------------------------------------
load('../Data/OutputStrInfo_realData/strInfo_5_53.dat')

png(filename = "../Plots/precStreetsAndBuff.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)

areas = NULL
for(i in 1:length(streetLengthInfo_null)) {
  if (sum(is.na(streetLengthInfo_null[[i]][[1]])) == 0) {
    areas = c(areas, streetLengthInfo_null[[i]][[1]]$buffer@polygons[[1]]@area)
  } else {
    areas = c(areas, NA)
  }
}
areas_ind = order(areas, decreasing = T)
plot(nycSub[53, ], lwd = 2.5)
plot(streetLengthInfo_null[[areas_ind[1]]][[1]]$buffer, border = "blue", lwd = 2, add = T)
temp = nycSub[53, ]
plot(streetsByPrec[[53]], add = T, col = "grey", lwd = 1.5)

borderBuff = gBuffer(temp, width = -500)
newSubStreets = gIntersection(streetsByPrec[[53]], borderBuff)
plot(newSubStreets, add = T, col = 'red', lwd = 2)
plot(streetLengthInfo_null[[areas_ind[1]]][[1]]$buffer,  add = T, border = "blue", lwd = 2)
plot(streetLengthInfo_null[[areas_ind[12]]][[1]]$buffer, add = T, border = "blue", lwd = 2)
plot(streetLengthInfo_null[[areas_ind[60]]][[1]]$buffer, add = T, border = "blue", lwd = 2)

dev.off()

# Arrest rates per NYC Precinct ------------------------------------------------
load('../Data/dataOff_sub.rda')
load('../Data/dataArr_sub.rda')
plot(nycSub)
nycSub$Precinct

totals_per_precinct = matrix(ncol = 3, nrow = length(nycSub$Precinct))
colnames(totals_per_precinct) = c("prec", "arrest", "crime")

for(i in 1:length(nycSub$Precinct)) {
    totals_per_precinct[i,1] = nycSub$Precinct[i]
    totals_per_precinct[i,2] = sum(dataArr_sub$precinct == nycSub$Precinct[i])
    totals_per_precinct[i,3] = sum(dataOff_sub$precinct == nycSub$Precinct[i])
}

color.gradient <- function(x, colors=c("green", "yellow", "red"), 
                           colsteps=100) {
    return(colorRampPalette(colors)(colsteps)[
        findInterval(x, seq(min(x), max(x), length.out=colsteps))
    ])
}

png(filename = "../Plots/arrPerGroup.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)
par(oma = c(4,1,1,1), mfrow = c(1, 2), mar = c(2, 2, 1, 1))
plot(nycSub, col = color.gradient(arr_per_crime), main = 'Arrests per crime activity',cex.main=2)
plot(nycSub, col = color.gradient(arr_per_area), main = 'Arrests per precinct area',cex.main=2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = c("High arrest rate", "Low arrest rate"),
       col = c('red', 'green'),lwd = 10, xpd = TRUE, horiz = TRUE, cex = 2.5, seg.len=1, bty = 'n')
dev.off()