# read csv creates NA values wehere there were empty values - this needs cleaning


make.uk <- function(path.to.scotland,path.to.england,path.to.wales,path.to.output){
  read.country(list.files(path.to.scotland, pattern="*.csv"),"sco",path.to.scotland)
  read.country(list.files(path.to.england, pattern="*.csv"),"eng",path.to.england)
  read.country(list.files(path.to.wales, pattern = "*.csv"), "wales", path.to.wales)
  metadata <- make.eng_metadata(allEntries)
  
  save.uk(sco_cases,eng_cases,wales_cases,path.to.output,"cases.csv")
  
  save.uk(sco_lighting, eng_lighting, wales_lighting, path.to.output,"lighting.csv")
  save.uk(sco_occupants,eng_occupants, wales_occupants ,path.to.output,"occupants.csv") 
  save.uk(sco_roofs,eng_roofs, wales_roofs, path.to.output,"roofs.csv")
  save.uk(sco_storeys,eng_storeys, wales_storeys, path.to.output,"storeys.csv") 
  save.uk(`sco_water-heating`,`eng_water-heating`, `wales_water-heating`, path.to.output,"water-heating.csv") 
  save.uk(sco_ventilation,eng_ventilation, wales_ventilation ,path.to.output,"ventilation.csv") 
  save.uk(`sco_space-heating`,`eng_space-heating`, `wales_space-heating` ,path.to.output,"space-heating.csv") 
  save.uk(`sco_additional-properties`,`eng_additional-properties`, `wales_additional-properties`, path.to.output,"additional-properties.csv")
  save.eng_metadata(metadata, file.path(path.to.output, "metadata.csv"))
  save.uk(sco_elevations,eng_elevations, wales_elevations, path.to.output,"elevations.csv")
  
}


read.country <- function(country.files,countrycode,path.to.country){  
  
  for(file in country.files)
  {
    perpos <- which(strsplit(file, "")[[1]]==".")
    assign(
      gsub(" ","",paste(countrycode,"_",substr(file, 1, perpos-1))), 
      read.csv(file.path(path.to.country,file),stringsAsFactors=FALSE,check.names = FALSE),
      pos=sys.frame())
  }
  
}

save.uk <- function(sco,eng,wales,path.to.output,file.name){
  uk <- rbind(sco,eng,wales)
  write.csv(uk,file.path(path.to.output,file.name), row.names = FALSE, na="")
}
