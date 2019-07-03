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
                        area.name    = area.name,
                        project.name = project.name, 
                        task.name    = task.name, 
                        task.desc    = task.desc,
                        stringsAsFactors = FALSE)
  
  #join them all 
  joined.df <- inner_join(area.df, project.df, by = "area.name") %>%
    inner_join(., task.df, by = c("project.name")) 
  
  # #row numbers by field
  # area.rn <- joined.df %>%
  #   group_by(area.name) %>% 
  #   summarise(area.rownum = 0)
  # for (i in unique(area.rn$area.name)) {
  #   area.rn$area.rownum[area.rn$area.name == i] <- 1:nrow(area.rn[area.rn$area.name == i,])
  # }
  # 
  # project.rn <- joined.df %>%
  #   group_by(area.name, project.name) %>%
  #   summarise(project.rownum = 0)
  # for (an in unique(project.rn$area.name)) {
  #   for (pn in unique(project.rn$project.name))
  #     project.rn$project.rownum[project.rn$area.name == an & 
  #                                 project.rn$project.name == pn] <- 1:nrow(project.rn[project.rn$area.name == an & 
  #                                                                                       project.rn$project.name == pn,])
  # }
  # 
  # #join again
  # joined.df <- inner_join(joined.df, area.rn) %>%
  #   inner_join(., project.rn)
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

tim.resps <- rbind(fun_add.work(level = "area", area.name = "Capital Planning", 
                          area.desc = "Manage Capital Planning Program", 
                          area.notes = c("is this something i should be doing? dw response: \"i don't know\" ", 
                                         "dw said he needs to be more educated on this side of stuff.", 
                                         "dw: in the meantime keep doing the stufff that i'm doing now.", 
                                         "dw: conitnue doing the working document and share it.", 
                                         "what does dw need from me?  dw: i actually need to give info you YOU.  need to learn more about the process.  continue what i'm doing.", 
                                         "*to do:  check into adopted CIP")), 
                   fun_add.work(level = "project", area.name = "Capital Planning", 
                                project.name = c("TIP", "STIP")), 
                   fun_add.work(level = "area", area.name = "Plans Review", 
                                area.desc = "plan reviews"), 
                   fun_add.work(level = "project", area.name = "Plans Review", 
                                project.name = c("site plans", "rezonings", 
                                                 "express reviews")), 
                   fun_add.work(level = "task", area.name = "Plans Review", 
                                project.name = "rezonings", task.name = c("task1", "task10")))

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

