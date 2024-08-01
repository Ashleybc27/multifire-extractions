### raw functions
###
inxfires <- function(data, years, window){
  reburnareas <- c()
  outpolys <- c()
  for (i in years){
    oneyr <- subset(data, data$year == i)
    print(i)
    yrwind <- subset(data, data$year < i & data$year >= (i-window))
    itx <- st_intersection(oneyr, yrwind)
    att <- itx %>% mutate(areacalc = st_area(.) %>% as.numeric())
    assign(paste0("reburn_polys_",i),att,pos=1) #area of all the individual polys uncomment if wanted
    ar <- sum(att$areacalc) ##area is in m2
    arkm <- ar/1000000 ##convert to km2
    outs <- cbind(i,arkm)
    reburnareas <- rbind(reburnareas,outs)
    outpolys <- rbind(outpolys,att)
    #print('yay')
  } 
  outpolys <<- outpolys
  colnames(reburnareas) <- c('year','area_km2')
  return(reburnareas)
}



dayfunc<-function(d){
  rv<-raster(apply(d,1,rev),crs=crs(meanprRas),
             xmn=corners[1], xmx=corners[2], ymn=corners[3], ymx=corners[4])
  zs<-exact_extract(rv,basin,'mean') ##"fast" zonal statistics
  return(zs) 
}


makedf <- function(extracted) {
  output <- c()
  for (i in extracted){
    output <- rbind(output,i)
  }
  return(output)
}


percentarea <- function(summeddf,totarea){
  output <-c()
  for (i in 1:nrow(totarea)){
    year <- totarea[[1]][i]
    print(year)
    tot <- totarea[[2]][i]
    print(tot)
    tmp <- subset(summeddf, summeddf$year == year)
    print(head(tmp))
    tmp$percent <- summeddf$area/tot
    print(head(tmp))
    output <- as.data.frame(rbind(output,tmp))
  }
  return(output)
}


highhigh <- function(grabpolys,inityear=2021,rangeyears=1:20,path='allyearsseverity/mtbs_CA_'){
  resev <- raster(paste0(path,inityear,'.tif')) ##reburn severity map
  reburn <- getsev(grabpolys,resev) ##extract reburn severity 
  print(paste0('reburn read of',inityear))
  outarea <- c()
  for (i in rangeyears){ ##loop through all previous years 
    processyear <- as.integer(inityear-i)
    print(processyear)
    initsev <- raster(paste0(path,processyear,'.tif')) ##initial burn map
    initburn <- getsev(grabpolys,initsev) ##extract initial burn map 
    #print(processyear)
    #print('initburn read of',processyear,' for',inityear)
    #if (nrow(reburn) == nrow (initburn)){
    #timestep <- cbind(reburn,initburn)
    timestep<- base::merge(reburn,initburn,by=c('x','y'))
    colnames(timestep) <- c('x','y','reburn_sev','reburn_cover', 'initburn_sev','cover_frac2')
    matches <- subset(timestep, timestep$reburn_sev == 4 & timestep$initburn_sev == 4)
    highhigharea <- sum((matches$cover_frac * 900))
    toadd <- cbind(paste0('inityear_',processyear),highhigharea)
    outarea <- rbind(outarea,toadd)
    print(paste0('inityear_',processyear,'worked'))
  }
  colnames(outarea) <- c('year','highhigharea')
  return(outarea)
}

allyearshighhigh <- function(polys,allyears,rangeyears,path='allyearsseverity/mtbs_CA_'){
  allout <- c()
  for (k in allyears){
    grabpolys <- subset(polys, polys$year == k)
    tmpyear <- highhigh(grabpolys,k,rangeyears,path)
    tmpyear$reburnyr <- paste0('reburnyear_',k)
    allout <- rbind(allout,tmpyear)
  }
  return(allout)
}

getsev <- function(grabpolys,sev){
  output <- c()
  for (i in 1:nrow(grabpolys)){
    dat <- (grabpolys[i,])
    zz <- exact_extract(sev,dat,include_xy=T)
    zz2 <- makedf(zz)
    output <- rbind(output,zz2)
  }
  return(output)
}




