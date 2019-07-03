library("tidyverse") 
library("lubridate")
library("editData")

#vars----
#functions----
fun_add.work <- function(level = c("area", "project", "task"),
                         area.name     = NA, 
                         area.desc     = NA, 
                         area.notes    = NA, 
                         project.name  = NA, 
                         project.desc  = NA, 
                         project.notes = NA, 
                         task.name     = NA, 
                         task.desc     = NA, 
                         task.notes    = NA) {
  
  area.df <- data.frame(level      = level, 
                        area.name  = area.name,
                        area.desc  = area.desc,
                        area.notes = area.notes,
                        stringsAsFactors = FALSE)
  
  project.df <- data.frame(level         = level, 
                           area.name     = area.name,
                           project.name  = project.name, 
                           project.desc  = project.desc, 
                           project.notes = project.notes,
                           stringsAsFactors = FALSE)
  
  task.df <- data.frame(level        = level, 
                        project.name = project.name, 
                        task.name    = task.name, 
                        task.desc    = task.desc,
                        stringsAsFactors = FALSE)
  
  #row numbers by field
  area.rn <- area.df %>%
    group_by(area.name) %>% 
    summarise(rownum = 1:length(unique(area.df$area.name)))
  #area.rn$rownum <- 1:nrow(area.rn)
  
  #join them all 
  joined.df <- inner_join(area.df, project.df) %>%
    inner_join(., task.df) %>%
    inner_join(., area.rn)
  return(joined.df)
}

# #next function
# df_timwork <- function(name, 
#                        level = c("area", "project", "task"), 
#                        goal = NA, 
#                        desc = NA, 
#                        dw.notes = NA, 
#                        lvl.num = NA) {
#   df.out <- data.frame(name = name, 
#                        level = level, 
#                        lvl.num = lvl.num,
#                        goal = goal, 
#                        desc = desc, 
#                        dw.notes)
#   return(df.out)
# }

#goals----

# track City Planning work to ensure that Transit is participating when necessary
# maintain capital planning program
# plans review
# Wake

#data----

tim.resps <- fun_add.work(level = "area", area.name = "Capital Planning", 
                          area.desc = "Manage Capital Planning Program", 
                          area.notes = c("is this something i should be doing? dw response: \"i don't know\" ", 
                                         "dw said he needs to be more educated on this side of stuff.", 
                                         "dw: in the meantime keep doing the stufff that i'm doing now.", 
                                         "dw: conitnue doing the working document and share it.", 
                                         "what does dw need from me?  dw: i actually need to give info you YOU.  need to learn more about the process.  continue what i'm doing.", 
                                         "*to do:  check into adopted CIP"))

# tim.areas <- df_timwork(name = "Capital Planning", 
#                         level = c("TIP", "STIP", "CIP", "UPWP"), 
#                         dw.notes = rep(c("is this something i should be doing?\ndw response: \"i don't know\" ",2,5,6),
#                                        each = 4)) %>% as_tibble()
tim.areas
#tidy----
#plots----

ggplot() + 
  geom_point(data = tim.resps, 
             aes(x = ))

# ggplot() + 
#   geom_text(data = tim.areas, 
#             aes(x = name, y = level, label = dw.notes))

