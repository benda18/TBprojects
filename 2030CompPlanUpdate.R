library("tidyverse")
library("lubridate")
library("ggrepel")

#functions----


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


