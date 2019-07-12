library("tidycensus")
library("tidyverse")

fun_getgeo <- function(feature) {
  feature.geo <- NULL
  for (i in 1:length(feature$geometry)) {
    for (j in 1:length(feature$geometry[[i]]))
    GEOID  <- feature$GEOID[i]
    lon <- feature$geometry[[i]][[j]][[1]][,1] 
    lat <- feature$geometry[[i]][[j]][[1]][,2] 
    feature.geo <- rbind(feature.geo, 
                        data.frame(GEOID = GEOID, 
                                   lon = lon, 
                                   lat = lat, 
                                   stringsAsFactors = FALSE))
  }
  return(feature.geo)
}

#https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

Sys.getenv("CENSUS_API_KEY")
data(county_laea)
data(fips_codes)
data(state_laea)



nc <- state_laea[state_laea$GEOID %in% fips_codes$state_code[fips_codes$state == "NC"],]

state.geo  <- fun_getgeo(feature = nc)
county.geo <- fun_getgeo(feature = county_laea[substr(county_laea$GEOID,1,2) %in% unique(nc$GEOID),])
county.geo$state.code = substr(county.geo$GEOID,1,2)
county.geo$county.code = substr(county.geo$GEOID,3,5)

fips <- fips_codes

joined.counties <- inner_join(county.geo, fips, by = c("state.code" = "state_code", 
                                    "county.code" = "county_code"))

vars10 <- c("P005003", "P005004", "P005006", "P004003")

vars.sf1 <- load_variables(year = 2010, dataset = "sf1", cache = TRUE)
vars.acs1 <- load_variables(year = 2015, dataset = "acs1", cache = TRUE)

pt.vars <- vars.acs1[vars.acs1$concept == "MEANS OF TRANSPORTATION TO WORK",] %>%
  .[grepl("public transportation", .$label, ignore.case = TRUE), ] 

args(get_acs)
pt.acs <- get_acs(geography = "county", variables = pt.vars$name[1:nrow(pt.vars)-1], 
                  year = c(2010),
                  state = "37") %>% 
  inner_join(., pt.vars, by = c("variable" = "name"))

pt.acs$state.code <- substr(pt.acs$GEOID, 1,2)
pt.acs$county.code <- substr(pt.acs$GEOID, 3,5)

joined.counties2 <- inner_join(joined.counties, pt.acs)

# dec.nc <- get_decennial(geography = "county", variables = vars10, year = 2010,
#                         summary_var = "P001001", state = "NC", geometry = TRUE) %>%
#   mutate(pct = 100 * (value / summary_value))

ggplot() + 
  # geom_path(data = joined.counties2, 
  #           aes(x = lon, y = lat, group = GEOID)) +
  coord_quickmap() +
  geom_polygon(data = joined.counties2[joined.counties2$label == "Estimate!!Total!!Public transportation (excluding taxicab)",], 
               color = "white", 
            aes(x = lon, y = lat, group = GEOID, 
                fill = estimate)) +
  scale_fill_viridis_c(option = "C", trans = "sqrt")
