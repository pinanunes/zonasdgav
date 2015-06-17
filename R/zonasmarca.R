library(RPostgreSQL)
library(plotKML)
library(sp)
library(rgeos)

zonasmarca<-function(marca,doenca){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user="postgres", password="postgres",dbname="PCEDA", host="10.50.4.60", port=5432)
  zprot<-3000
  if(doenca=="PSA"|doenca=="SVD"){
    zvig<-10000
  }else if(doenca=="PSC"){
    zvig<-14000
  }
  
  smarcas<- data.frame(marca=character(),
                       tipo_exp2=character(), 
                       codmes2=character(), 
                       tipos_tpor=integer(),
                       tipos_tsub=integer(),
                       tipos_tvar=integer(),
                       tipos_teng=integer(),
                       tipos_soma=integer(),
                       Lon=double(),
                       Lat=double(),
                       geo=character(), 
                       stringsAsFactors=FALSE)   
  
  for(m in 1:length(marca)){
    msql<-paste("Select distinct(marca),tipo_exp2, codmes2, tipos_tpor, tipos_tsub, tipos_tvar,tipos_teng,tipos_soma, ST_X(geom) as Lon, ST_Y(geom) as Lat,ST_AsText(geom) as geo from exploracoes where marca='",marca[m],"'",sep="")
    rs <- dbSendQuery(con, msql)
    marcas<-fetch(rs,n=-1)
    dbClearResult(rs)
    
    smarcas<-rbind(smarcas,marcas)
    
  }
  dbDisconnect(con)  
  con <- dbConnect(drv, user="postgres", password="postgres",dbname="PCEDA", host="10.50.4.60", port=5432)  
  smvig <- data.frame(marca=character(),
                      tipo_exp2=character(), 
                      codmes2=character(), 
                      tipos_tpor=integer(),
                      tipos_tsub=integer(),
                      tipos_tvar=integer(),
                      tipos_teng=integer(),
                      tipos_soma=integer(),
                      Lon=double(),
                      Lat=double(),
                      stringsAsFactors=FALSE)  
  smprot <- data.frame(marca=character(),
                       tipo_exp2=character(), 
                       codmes2=character(), 
                       tipos_tpor=integer(),
                       tipos_tsub=integer(),
                       tipos_tvar=integer(),
                       tipos_teng=integer(),
                       tipos_soma=integer(),
                       Lon=double(),
                       Lat=double(),
                       stringsAsFactors=FALSE)  
  for(k in 1:nrow(smarcas)){
    sql<-paste("Select distinct(marca),tipo_exp2, codmes2, tipos_tpor, tipos_tsub, tipos_tvar,tipos_teng,tipos_soma, ST_X(geom) as Lon, ST_Y(geom) as Lat from exploracoes where ST_Distance_Sphere(geom, ST_GeomFromText('",smarcas$geo[k],"')) > ",zprot," and ST_Distance_Sphere(geom,ST_GeomFromText('",smarcas$geo[k],"')) <= ",zvig,sep="")
    rs <- dbSendQuery(con, sql)
    mzvig<-fetch(rs,n=-1)
    dbClearResult(rs)
    sql<-paste("Select distinct(marca),tipo_exp2, codmes2, tipos_tpor, tipos_tsub, tipos_tvar,tipos_teng,tipos_soma, ST_X(geom) as Lon, ST_Y(geom) as Lat from exploracoes where ST_Distance_Sphere(geom, ST_GeomFromText('",smarcas$geo[k],"')) < ",zprot,sep="")    
    rs <- dbSendQuery(con, sql)
    mzprot<-fetch(rs,n=-1)
    dbClearResult(rs)
    smvig<-rbind(smvig,mzvig)
    smprot<-rbind(smprot,mzprot)
    
  }
  smvig<-unique(smvig)
  smprot<-unique(smprot)
  smprot<-subset(smprot, !(marca %in% smarcas$marca))
  smvig<-subset(smvig, !(marca %in% smprot$marca))
  smvig<-subset(smvig, !(marca %in% smarcas$marca))
  names(smarcas)<-c("Marca","Tipo","Codmes","Porcas","Substituicao","Varrascos","Engorda","Total","Lon","Lat","geo")
  smarcas<-smarcas[1:10]
  smarcas$Zona<-"Foco"
  
  names(smvig)<-c("Marca","Tipo","Codmes","Porcas","Substituicao","Varrascos","Engorda","Total","Lon","Lat")
  smvig$Zona<-"Vigilância"
  
  names(smprot)<-c("Marca","Tipo","Codmes","Porcas","Substituicao","Varrascos","Engorda","Total","Lon","Lat")
  smprot$Zona<-"Protecção"
  
  exps<-rbind(smarcas,smprot)
  exps<-rbind(exps,smvig)
  print(exps)
  print("Explorações foco:")
  print(smarcas$Marca)
  print("")
  print(paste("Zona de Protecção",zprot,"metros"))
  print(paste("Total de explorações:",nrow(smprot)))   
  print(paste("Total de suinos:",sum(smprot$Total)))
  print(xtabs(Total~Tipo+Codmes,smprot))
  print("")
  print(paste("Zona de Vigilância",zvig,"metros"))
  print(paste("Total de explorações:",nrow(smvig)))
  print(paste("Total de suinos:",sum(smvig$Total)))
  print(xtabs(Total~Tipo+Codmes,smvig))
  
  coordinates(exps)<-~Lon+Lat
  proj4string(exps) <- CRS("+init=epsg:4326")
  plotKML(exps,balloon=TRUE,colour="Zona")
  
#   coordinates(smarcas)<-~Lon+Lat
#   proj4string(smarcas) <- CRS("+init=epsg:4326")
#   marcas2 <- spTransform(smarcas, CRS( "+init=epsg:20791" )) 
#   a<-(gBuffer(marcas2, width=zprot))
#   b<-(gBuffer(marcas2, width=zvig))
#   c<-b-a
#   plotKML(c)
  
  cons<-dbListConnections(drv)
  for(con in cons)
    dbDisconnect(con)
}

zonasmarca(c("PTRY04A","PTRY03A","PTRS68U"),"PSA")