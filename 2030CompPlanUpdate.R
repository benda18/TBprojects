library("tidyverse")
library("lubridate")
library("ggrepel")


#vars----
wrap_len <- 200

#functions----
fun_toc <- function(sub.sec, name, name.short = NA, np.desc = NA, notable.policy = NA, 
                    transit.impact = FALSE) {
  df.out <- data.frame(sub.sec = sub.sec, 
                       name = name, 
                       name.short = name.short, 
                       transit.impact = transit.impact, 
                       notable.policy = notable.policy, 
                       np.desc = np.desc, 
                       stringsAsFactors = FALSE)
  return(df.out)
}

#executive summary----

policy.changes <- "Plan Section@Revised@Removed@New
Land Use@18@2@5
Transportation@15@1@12
Environmental Protection@19@@4
Economic Development@5@@1
Housing@10@8@2
Parks, Recreation, and Open Space@10@6@16
Public Utilities@@@1
Community Facilities and Services@@@5
Urban Design@9@2@22
Historic Preservation@2@@4
Arts and Culture@6@5@4
Regional and Interjurisdictional Coordination@1@1@
Downtown Raleigh@8@6@
Area Specific Guidance@2@62@86
Implementation@2@@"
policy.changes <- read_delim(policy.changes, delim = c("@"))

pc.melt <- reshape2::melt(policy.changes, measure.vars = c("Revised", "Removed", "New"), 
                          variable.name = "policy.type", value.name = "policy.number")
pc.melt$table <- "policy"


# ggplot() + 
#   geom_bin2d(data = pc.melt, 
#              aes(x = `Plan Section`, y = policy.type, 
#                  fill = policy.number)) +
#   coord_flip() +
#   scale_fill_viridis_c(option = "D", trans = "log10")

action.changes <- "Plan Section@Revised@Removed@New
Land Use@3@@1
Transportation@@2@11
Environmental Protection@@3@13
Economic Development@2@4@4
Housing@3@10@3
Parks, Recreation, and Open Space@1@7@19
Public Utilities@@@
Community Facilities and Services@5@1@
Urban Design@1@@4
Historic Preservation@4@2@5
Arts and Culture@@12@20
Regional and Interjurisdictional Coordination@1@1@
Downtown Raleigh@1@12@1
Area Specific Guidance@2@12@64
Implementation@@1@"

action.changes <- read_delim(action.changes, delim = c("@"))
ac.melt <- reshape2::melt(action.changes, measure.vars = c("Revised", "Removed", "New"), 
                          variable.name = "action.type", value.name = "action.number")
ac.melt$table <- "action"
# ggplot() + 
#   geom_bin2d(data = ac.melt, 
#              aes(x = `Plan Section`, y = action.type, 
#                  fill = action.number)) +
#   coord_flip() +
#   scale_fill_viridis_c(option = "D", trans = "log10")

#plots----
joined.data <- inner_join(ac.melt, pc.melt, by = c("Plan Section", "action.type" = "policy.type")) %>%
  as_tibble() %>%
  melt(., measure.vars = c("action.number", "policy.number"), variable.name = "number.type", 
       value.name = "number.number")

ggplot() + 
  geom_col(data = joined.data, 
           aes(x = `Plan Section`, y = number.number, 
               fill = number.type)) +
  coord_flip() +
  scale_y_log10() +
  facet_grid(~action.type, 
             scales = "free", 
             margins = "action.type")


#section 4 Transportation----
toc.sec4 <- rbind(fun_toc("4.1", "Land Use and Transportation Coordination", 
                          "land use-trans coord", transit.impact = TRUE, 
                          notable.policy = NA, 
                          np.desc = c("Offer residents safe and attractive choices among 
                                      modes...", 
                                      "...target transit investments along corridors with 
                                      concentrations of office, retail and residential uses")), 
                  fun_toc("4.2", "Roadway System and TDM", "road system & tdm", 
                          np.desc = c("promote and develop an integrated, multimodal transportation 
                          system", "PT2.14 Employer-based trip reduction", "AT2.5 Intermodal facility 
                                      prioritization")), 
                  fun_toc("4.3", "Complete Streets: Hierarchy and Design", 
                          "complete streets", 
                          np.desc = c(NA)), 
                  fun_toc("4.4", "Public Transportation", "transit",
                          transit.impact = TRUE, 
                          np.desc = c("PT4.1 promote transit with emphasis on trasnit-dependent households", 
                                      "PT4.3 prioritize frequent transit investments in corridors with greatest
                                      potential to attract riders and shape development", 
                                      "PT4.4 reserve rights-of-way for transit", 
                                      "PT4.5 promote transit efficiency by reducing wait time and transfer time...", 
                                      "PT4.6 substitute event-based transit for on-site parking where feasible to free
                                      land for other uses around event locations", 
                                      "PT4.7 increase service between residential and employment areas as well as
                                      to regional facilities such as RDU and RTP", 
                                      "PT4.8 new developments should coordinate with goraleigh to provide stop facilities 
                                      that are lit, include a shelter, bench and waste bin and other amenities", 
                                      "PT4.9 ID bike/ped needs within reasonable distance of transit stops in need of
                                      enchancements", 
                                      "PT4.10 transit-first features such as TSP and queue jumps", 
                                      "PT4.11 Demand-response transit - support expansion of GoRaleigh Access...",
                                      "PT4.12 Special needs - provide transit for... senior citizens", 
                                      "PT4.13 crosstown travel", 
                                      "PT4.14 provide ciruclator services fo facilitate mobility within identified
                                      City Growth Centers", 
                                      "PT4.16 bus stop spacing", 
                                      "AT4.6 locate park & rides along capital/atlantic, six forks, glenwood, 
                                      creedmoor, new bern, west raleigh, arena area, south saunders/tryon", 
                                      "AT4.18 develop a public outreach campaign" )), 
                  fun_toc("4.5", "Pedestrian and Bicycle Circulation", "bike-ped"), 
                  fun_toc("4.6", "Parking Management","parking"), 
                  fun_toc("4.7", "Transportation Safety Improvements", "safety"), 
                  fun_toc("4.8", "Commercial Truck and Rail Freight", "freight"), 
                  fun_toc("4.9", "Future Street Improvements", "future street imprvmts"), 
                  fun_toc("4.10", "Emerging Technologies", "innovation")) %>% as_tibble()
toc.sec4$sub.sec_f <- factor(toc.sec4$sub.sec, levels = paste("4.", rev(seq(1,10,1)), sep = ""))

toc.sec4$np.desc <- str_wrap(toc.sec4$np.desc, width = wrap_len)
toc.sec4$rn <- 1:nrow(toc.sec4)

toc.sec4$fill <- "white"



ggplot() + 
  geom_label(data = toc.sec4, size = 3.0, nudge_x = -0.55,
             aes(x = NA, y = factor(rn), hjust = 0,
                 #color = transit.impact,
                 label = np.desc, 
                 fill = I(fill)
                 )) +
  facet_grid(sub.sec_f+name.short~., scale = "free_y", space = "free_y", 
             switch = "y", 
             as.table = FALSE) +
  theme_bw()+
  theme(strip.text.y = element_text(angle = 180), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank()) +
  scale_y_discrete(name = "Section") +
  scale_x_discrete(name = "Impact on Transit") +
  labs(title = "Summary 2030 Comp Plan Changes", 
       subtitle = "relevant to public transit", 
       caption = paste("updated", Sys.Date()), 
       color = "Impact on Transit")
