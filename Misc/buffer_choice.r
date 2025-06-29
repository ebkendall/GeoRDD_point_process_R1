# Code used to serve as a general guideline for choosing a buffer width, but
# in theory our approach should be robust to a given choice of buffer width.

library(sp)

load('Data/streetNYC.RData')
load('Data/nycSub.RData')

set.seed(2025)

n_buff_width = 8
adjust_val = c(0.5, 1, 1.5, 2, 3, 4)

line_mid_points = getSpatialLinesMidPoints(streetNYC)

mid_points_by_prec = vector(mode = 'list', length = length(nycSub$Precinct))

for(i in 1:length(nycSub$Precinct)) {
    
    print(i)
    
    if(length(nycSub@polygons[[i]]@Polygons) > 1) print(paste0("more polys ", i))
    
    mid_point_i = point.in.polygon(line_mid_points@coords[,1], line_mid_points@coords[,2],
                                   nycSub@polygons[[i]]@Polygons[[1]]@coords[,1],
                                   nycSub@polygons[[i]]@Polygons[[1]]@coords[,2])
    
    mid_points_by_prec[[i]] = line_mid_points@coords[mid_point_i > 0, ]
}

adj_dists = vector(mode = 'list', length = length(mid_points_by_prec))
for(i in 1:length(mid_points_by_prec)) {
    print(i)
    temp = lapply(1:nrow(mid_points_by_prec[[i]]), function(x){
        spDistsN1(pts = mid_points_by_prec[[i]][-x,], pt = mid_points_by_prec[[i]][x,])})
    temp2 = do.call('c', lapply(temp, min))
    adj_dists[[i]] = temp2
}

all_adj_dists = do.call('c', adj_dists)
print(quantile(all_adj_dists, 0.95))