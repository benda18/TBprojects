library("tidyverse")
library("lubridate") 
library("jsonlite")


#vars----

#functions----
fun_project <- function(project.name, last.updated,
                        project.type = c("citywide project", "area plans", "corridor study", "other"), 
                        project.status = c("active", "implementation", "completed"),
                        start.date = NA, end.date = NA, 
                        url = NA) {
  df.out <- data.frame(name = project.name, 
                       type = project.type, 
                       status = project.status,
                       start.date = start.date, 
                       end.date = end.date,
                       last.updated = last.updated,
                       url = url, 
                       stringsAsFactors = FALSE)
  return(df.out)
}




# #import city calendar from api----
# #http://data-ral.opendata.arcgis.com/datasets/public-meetings-calendar
# CORcalendar.api <- "https://opendata.arcgis.com/datasets/37d0fd6d45074ebcaf585e186cbab472_0.geojson"
# CORcal.json <- fromJSON(CORcalendar.api)
# 
# #convert to data.frame----
# corcaldf <- CORcal.json[["features"]][["properties"]]
# 
# #tidy datetime events----
# corcaldf$start_date <- ymd_hms(corcaldf$start_date, tz = "America/New_York") 
# corcaldf$end_date   <- ymd_hms(corcaldf$end_date, tz = "America/New_York") 


