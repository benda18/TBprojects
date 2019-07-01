library("tidyverse")
library("lubridate") 
library("jsonlite")
library("ggrepel")


#vars----
wrap_len <- 21
date.updated <- ymd(20190701)

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
                                      last.updated = date.updated, 
                                      "citywide", "active", 
                                      start.date = ymd(20190312), 
                                      end.date = ymd(20190903), 
                                      url = NA), 
                          fun_project("Raleigh BRT: Equitable Development Around Transit", 
                                      last.updated = date.updated, 
                                      "citywide", "active", 
                                      start.date = ymd(20190606), 
                                      end.date = ymd(20200101), 
                                      url = NA), 
                          fun_project("Bike Plan Update", date.updated, "citywide", 
                                      "implementation", 
                                      start.date = NA, 
                                      end.date = NA, 
                                      url = NA), 
                          fun_project("Southeast Special Area Study", 
                                      last.updated = date.updated, 
                                      "area plans", "active", 
                                      start.date = NA, 
                                      end.date = NA))
cityplanning.evnt <- rbind(fun_event("Ask a Planner", 
                                     "2030 Comp Plan Update", 
                                     "RMB 237", ymd(20190702)), 
                           fun_event("Public Hearing CP-2C-19", 
                                     "2030 Comp Plan Update", 
                                     "Council Chambers", ymd(20190702)), 
                           fun_event("Public Hearing CP-2B-19", 
                                     "2030 Comp Plan Update", 
                                     "Council Chambers", ymd(20190903)), 
                           fun_event("Begin: Station Area Planning", 
                                     "Raleigh BRT: Equitable Development Around Transit", 
                                     NA, ymd(20200101)), 
                           fun_event("September Workshops", 
                                     "Raleigh BRT: Equitable Development Around Transit", 
                                     NA, ymd(20190901)))

#tidy wrap long text strings
cityplanning.evnt$project.name <- str_wrap(cityplanning.evnt$project.name, width = wrap_len)
cityplanning.prj$project.name  <- str_wrap(cityplanning.prj$project.name, width = wrap_len) 

#tidy events
cityplanning.evnt <- inner_join(cityplanning.evnt, cityplanning.prj[!colnames(cityplanning.prj) %in% c("start.date", "end.date")])

cityplanning.prj <- cityplanning.prj[cityplanning.prj$project.status == "active",]
cityplanning.evnt<- cityplanning.evnt[cityplanning.evnt$project.status == "active",]
#plot----
ggplot() + 
  geom_vline(xintercept = Sys.Date(), color = "grey", 
             size = 1.25, linetype = 1)+
  geom_segment(data = cityplanning.prj, 
               size = 2.0,
               aes(x = start.date, xend = end.date, 
                   y = project.name,
                   yend = project.name, 
                   group = project.name, 
                   color = project.name)) +
  geom_point(data = cityplanning.evnt, 
             size = 4, shape = 21, fill = "white", 
             aes(x = event.date, 
                 y = project.name, 
                 color = project.name)) +
  geom_label_repel(data = cityplanning.evnt, 
                   fill = "white", 
                   size = 3.25,
                   min.segment.length = 0, point.padding = unit(0.325, "inches"),
                   arrow = arrow(angle = 20, type = "closed", unit(0.075, "inches")),
                   direction = "y", 
                   aes(x = event.date, 
                       y = project.name, 
                       label = event.name)) +
  scale_x_date(name = "Date", 
               date_breaks = "2 month", date_labels = "%b\n%Y", 
               date_minor_breaks = "1 month") +
  theme_linedraw() + 
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        #axis.text.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        legend.position = "none") +
  facet_grid(project.type~.,  
             #margins = "project.name",
             space = "free_y", scales = "free_y") +
  labs(title = "Raleigh City Planning Ongoing Projects", 
       subtitle = paste("Updated:", format(date.updated, format = "%B %d, %Y")),
       color = "Project")

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


