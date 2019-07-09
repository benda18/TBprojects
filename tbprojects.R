library("tidyverse")
library("lubridate") 
library("jsonlite")
library("ggrepel")


#vars----
wrap_len <- 15
date.updated <- ymd(20190709)

#functions----
fun_project <- function(project.name, last.updated,
                        project.type = c("citywide", "area plans", "corridor study", "other"), 
                        project.status = c("active", "completed"),
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
                                      "completed", 
                                      start.date = NA, 
                                      end.date = NA, 
                                      url = NA), 
                          fun_project("Southeast Special Area Study", 
                                      last.updated = date.updated, 
                                      "area plans", "active", 
                                      start.date = ymd(20190617), 
                                      end.date = Sys.Date()%m+% months(10)), 
                          fun_project("Midtown-St. Albans", 
                                      last.updated = date.updated, 
                                      "area plans", "active", 
                                      start.date = ymd(20180501),
                                      end.date = ymd(20191031)),
                          fun_project("Downtown Land Disposition", 
                                      last.updated = ymd(20190701), 
                                      "area plans", "completed", 
                                      start.date = ymd(20160416), 
                                      end.date = Sys.Date()%m+% months(10)), 
                          fun_project("Accessory Dwelling Units - Mordecai CAC", 
                                      last.updated = ymd(20190701), 
                                      "area plans", "completed", 
                                      start.date = NA, 
                                      end.date = NA), 
                          fun_project("Cameron Village Hillsborough Street Small Area Plan", 
                                      last.updated = date.updated, 
                                      "area plans", "completed", 
                                      start.date = NA, 
                                      end.date = NA), 
                          fun_project("Downtown Plan", 
                                      last.updated = date.updated, 
                                      "area plans", "completed", 
                                      start.date = NA, 
                                      end.date = NA), 
                          fun_project("Moore Square Design", 
                                      last.updated = date.updated, 
                                      "area plans", "completed", 
                                      start.date = NA, 
                                      end.date = NA), 
                          fun_project("Capital Blvd North", date.updated, 
                                      "corridor study", "active", 
                                      start.date = ymd(20180801), 
                                      end.date = ymd(20200401)), 
                          fun_project("Falls of Neuse", date.updated, 
                                      "corridor study", "completed", 
                                      start.date = ymd(20170501), 
                                      end.date = ymd(20190521)),
                          fun_project("Avent Ferry", date.updated, 
                                      "corridor study", "active", 
                                      start.date = ymd(20170301), 
                                      end.date = ymd(20191001)))
cityplanning.evnt <- rbind(fun_event("Public Hearing CP-2C-19", 
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
                                     NA, ymd(20190901)), 
                           fun_event("Winter Workshop", 
                                     #"Fall 2019: Big Ideas Workshop", 
                                     "Capital Blvd North", NA, 
                                     ymd(20191001)),
                           fun_event("Winter Workshop", 
                                     #"Winter 19/20: Specific Ideas Workshop", 
                                     "Capital Blvd North", NA, 
                                     ymd(20200101)),
                           fun_event("Final Recommendations", 
                                     "Capital Blvd North", NA, 
                                     ymd(20200401)), 
                           fun_event("Council Presentation", "Avent Ferry", 
                                     "Council Chambers", ymd(20190702)), 
                           fun_event("Project Completion: Fall 2019", 
                                     "Midtown-St. Albans", NA, ymd(20191031)), 
                           fun_event("Project Completion: Fall 2019", "Avent Ferry", 
                                     NA, ymd(20191001)))
#tim tasks----
next.steps <- data.frame(project.name = str_wrap(unique(cityplanning.prj$project.name), width = wrap_len), 
                         `Next.Step` = NA, stringsAsFactors = FALSE)
next.steps$Next.Step[next.steps$project.name %in% str_wrap(c("Midtown-St. Albans", 
                                                    "Raleigh BRT: Equitable Development Around Transit", 
                                                    "Capital Blvd North", 
                                                    "Avent Ferry"), width = wrap_len)] <- "Reach out to PM"
next.steps$Next.Step[next.steps$project.name %in% str_wrap(c("Southeast Special Area Study"), 
                                                           width = wrap_len)] <- "Awaiting CC Action"
next.steps$Next.Step[next.steps$project.name %in% str_wrap(c("2030 Comp Plan Update"), 
                                                           width = wrap_len)] <- "Review Changes"

#tidy wrap long text strings
cityplanning.evnt$project.name <- str_wrap(cityplanning.evnt$project.name, width = wrap_len)
cityplanning.prj$project.name  <- str_wrap(cityplanning.prj$project.name, width = wrap_len) 

#tidy events
cityplanning.evnt <- inner_join(cityplanning.evnt, cityplanning.prj[!colnames(cityplanning.prj) %in% c("start.date", "end.date")]) 

cityplanning.prj <- cityplanning.prj[cityplanning.prj$project.status == "active",]%>%
  inner_join(., next.steps)
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
                   color = Next.Step)) +
  geom_point(data = cityplanning.evnt, 
             size = 4, shape = 21, fill = "white",
             aes(x = event.date, 
                 y = project.name)) +
  geom_label_repel(data = cityplanning.evnt[cityplanning.evnt$event.date >= Sys.Date()%m-% months(1) & 
                                              cityplanning.evnt$event.date <= Sys.Date()%m+% months(9),], 
                   fill = "white", size = 3.0,
                   alpha = 0.66,
                   min.segment.length = 0, point.padding = unit(0.225, "inches"),
                   arrow = arrow(angle = 20, type = "closed", unit(0.075, "inches")),
                   direction = "both", 
                   aes(x = event.date, 
                       y = project.name, 
                       label = event.name)) +
  scale_x_date(name = "Date", 
               date_breaks = "2 month", date_labels = "%b\n%Y", 
               date_minor_breaks = "1 month") +
  theme_bw() + 
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(hjust = 0.5), 
        #axis.ticks.y = element_blank(),
        #legend.position = "none", 
        axis.title = element_blank()) +
  facet_grid(project.type~.,  
             #margins = "project.name",
             space = "free_y", scales = "free_y") +
  labs(title = "Raleigh City Planning Active Projects", 
       subtitle = paste("Updated:", format(date.updated, format = "%B %d, %Y")),
       color = "Tim/GoR Next Steps") +
  coord_cartesian(xlim = c(Sys.Date()%m-% months(3), Sys.Date()%m+% months(9)))

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


