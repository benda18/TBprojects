library("tidyverse")
library("lubridate") 
library("jsonlite")
library("ggrepel")


#vars----

#functions----
fun_project <- function(project.name, last.updated,
                        project.type = c("citywide", "area plans", "corridor study", "other"), 
                        project.status = c("active", "implementation", "completed"),
                        start.date = NA, end.date = NA, 
                        url = NA) {
  df.out <- data.frame(project.name = project.name, 
                       project.type = project.type, 
                       project.status = project.status,
                       start.date = start.date, 
                       end.date = end.date,
                       last.updated = last.updated,
                       project.url = url, 
                       stringsAsFactors = FALSE)
  return(df.out)
}

fun_event <- function(event.name, project.name,
                      event.loc,
                      event.date, event.url = NA) {
  df.out <- data.frame(event.name = event.name, 
                       project.name = project.name, 
                       event.date = event.date, 
                       event.loc = event.loc,
                       event.url = event.url)
  return(df.out)
}


#data creation----
cityplanning.prj <- rbind(fun_project("2030 Comp Plan Update", 
                                      last.updated = ymd(20190701), 
                                      "citywide", "active", 
                                      start.date = ymd(20190312), 
                                      end.date = ymd(20190903), 
                                      url = NA), 
                          fun_project("Raleigh BRT: Equitable Development Around Transit", 
                                      last.updated = ymd(20190701), 
                                      "citywide", "active", 
                                      start.date = NA, end.date = NA, 
                                      url = NA))
cityplanning.evnt <- rbind(fun_event("Ask a Planner", "2030 Comp Plan Update", 
                                     "RMB 237", ymd(20190702)))

#plot----
ggplot() + 
  geom_segment(data = cityplanning.prj, 
               aes(x = start.date, xend = end.date, 
                   y = str_wrap(project.name,0,25),
                   yend = str_wrap(project.name,0,25), 
                   group = str_wrap(project.name,0,25))) +
  geom_point(data = cityplanning.evnt, 
             aes(x = event.date, 
                 y = str_wrap(project.name,0,25))) +
  geom_text_repel(data = cityplanning.evnt, 
                  min.segment.length = 0, point.padding = unit(0.125, "inches"),
                  arrow = arrow(angle = 20, type = "closed", unit(0.075, "inches")),
                  direction = "y", 
                  aes(x = event.date, 
                      y = str_wrap(project.name,0,25), 
                      label = event.name))

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


