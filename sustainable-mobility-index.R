library(tidyverse)
library(openxlsx)

# The goal of the script is to implement the methodology of the Sustainable Mobility Index in Lithuania. 
# It will create an index for each municipality at a national level and within each class.  
# The index will be based on the following categories: 
# - Environmental Impact
# - Incentives and Policies 
# - Cycling 
# - Public Transport 
# - Walking 
# - Cars: Fossil Fuels 
# - Cars: Electric and Alternative Fuels 
# - Use of Space 
# We apply a min-max normalization to each variable. 
# Then we calculate the weighted average of the variables for each category. 
# Finally, we calculate the Sustainable Mobility Index as a weighted sum of the categories. 
lt_synthetic <- read.xlsx("lithuania_mobility_synthetic_60_classed_v2 (1).xlsx") 

# In this code chunk, we will transform the data from wide to long format and assign the categories to each variable. 
# We will get the variable names after pivoting the data using pivot_longer and use those to assign the categories. 
lt_synthetic_long <- lt_synthetic %>% 
  pivot_longer(cols = -c(County, Municipality, Class), names_to = "variable", values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(sustainable_mobility_category = case_when(
    variable == "Annual.CO₂-equivalent.emissions.from.road.transport.in.the.city/FUA.(Functional.Urban.Area)" ~ "Environmental Impact",
    variable == "PM10.amount.in.excess.of.EU.standards" ~ "Environmental Impact",
    variable == "PM2.5.amount.in.excess.of.EU.standards" ~ "Environmental Impact",
    variable == "NOx.amount.in.excess.of.EU.standards " ~ "Environmental Impact",
    variable == "Percent.of.population.expose.to.noise.>55.db(A).daytime.and.>50.db(A).nighttime  " ~ "Environmental Impact",
    variable == "Municipality.investment.in.charging.station" ~ "Incentives and Policies",
    variable == "Municipality.investment.in.public.transportation" ~ "Incentives and Policies",
    variable == "Municipality.investment.in.the.mobility.sharing.economy" ~ "Incentives and Policies",
    variable == "Road.user.charging/congestion.pricing.programs .(Number.of.programs./.total.amount.of.investments./.investments.per.population./.Programs.per.population)" ~ "Incentives and Policies",
    variable == "Direct.electric.vehicle.incentivization in.municipality.(Number.of.programs./.total.amount.of.investments./.investments.per.population./.Programs.per.population)" ~ "Incentives and Policies",
    variable == "Carsharing.incentive.policies.(e.g.,.free.or.discounted.parking.for.carsharing.services).(Number.of.programs./.total.amount.of.investments./.investments.per.population./.Programs.per.population)" ~ "Incentives and Policies",
    variable == "Presence.of.climate.targets.and.degree.of.commitment" ~ "Incentives and Policies",
    variable == "EV.incentivization.policies.(could.measure.by.number.of.policies.or.policy.usage.per.year)" ~ "Incentives and Policies",
    variable == "Bike.incentivization.policies.(by.government.or.firms) (could.measure.by.number.of.policies.or.policy.usage.per.year)" ~ "Incentives and Policies",
    variable == "Is.there.a.low emission.zone.established" ~ "Incentives and Policies",
    variable == "Does.the.municipality.have.a.sustainable.mobility.plan" ~ "Incentives and Policies",
    variable == "Number.of.fines per.population" ~ "Incentives and Policies",
    variable == "Number.of.bikes.by.type" ~ "Cycling",
    variable == "Share.of.all.trips.and.share.of.all.trips.to.work/school.by.bicycle" ~ "Cycling",
    variable == "Distance.traveled.by.bike.per.weekday" ~ "Cycling",
    variable == "Total.length.or.share.of.the.road.network.equipped.with.separated.cycling.lanes,.tracks,.or.other.facilities.(km.or.%)" ~ "Cycling",
    variable == "Cycle.tracks.(km) or.share.of.cycle.tracks" ~ "Cycling",
    variable == "Cycle.lanes.(km) or.share.of.cycle.lanes" ~ "Cycling",
    variable == "Share.of.cycle.tracks.renovated/reestablished.(km)" ~ "Cycling",
    variable == "Bicycle.parking.spaces.built.(number.per.year) or.bicycle.parking.spaces.per.population" ~ "Cycling",
    variable == "Per.capita.bike-sharing.bikes.and.stations.(number)" ~ "Cycling",
    variable == "The.share.of.bicycle.paths.in.the.total.road.network" ~ "Cycling",
    variable == "General.satisfaction.with.bike.parking.(%).or.other.bike.infrastructure" ~ "Cycling",
    variable == "Number.of.persons.fatally.or.seriously.injured  per.population" ~ "Cycling",
    variable == "Perceived.safety  of.Cycling" ~ "Cycling",
    variable == "Number.of.passengers.or.km.on.public.transport /.Number.of.passengers.or.km.on.public.transport per.population" ~ "Public Transport",
    variable == "Share.of.all.trips.by.public.transport" ~ "Public Transport",
    variable == "Change.in.public.transport.passengers " ~ "Public Transport",
    variable == "Share/change.of.public.transport.that.is.CO2.neutral/uses.alternative.fuel.sources" ~ "Public Transport",
    variable == "Percent.of.population.within.300.meters.of.a.PT.stop.that.runs.≥1.service/hour.(%)" ~ "Public Transport",
    variable == "Average.wait.time.at.stops" ~ "Public Transport",
    variable == "Average.length.of.walk.to.public.transit" ~ "Public Transport",
    variable == "Average.public.transit.operating.hours" ~ "Public Transport", 
    variable == "Percent.change.in.the.regularity.of.bus.services.(%)" ~ "Public Transport", 
    variable == "Number.of.daily.departures.from.all.stops.per.total.city.population" ~ "Public Transport", 
    variable == "Percent.of.public.transit.services.arriving.≤5.minutes.late.at.key.stops.(%)" ~ "Public Transport", 
    variable == "Standard.monthly.public.transport.fare.as.a.%.of.monthly.GDP.per.capita.(or.a.relative.comparison.of.monthly.ticket.cost.to.average.or.median.income)" ~ "Public Transport", 
    variable == "Price.of.a.monthly.PT.ticket.relative.to.fine " ~ "Public Transport",
    variable == "General.satisfaction.with.public.transportation  " ~ "Public Transport",
    variable == "Perceived.safety.of.public.transportation" ~ "Public Transport",
    variable == "Share.of.all.trips.by.walking" ~ "Walking",
    variable == "Share.of.all.trips.to.work/school.by.walking" ~ "Walking",
    variable == "Distance.traveled.by.walking.per.weekday" ~ "Walking",
    variable == "Percent.of.total.walkable.routes.that.are.off-street" ~ "Walking",
    variable == "Share.of.road.network.designated.as.car-free.or.pedestrian.zones" ~ "Walking",
    variable == "Share.of.public.right-of-way.(e.g.,.roads,.parks,.plazas).designated.as.car-free.or.pedestrian.zones" ~ "Walking",
    variable == "Number.of.pedestrians.fatally.or.seriously.injured.(Detailed.by.age.category,.vehicle.type,.accident.type.etc.)" ~ "Walking",
    variable == "Perceived.safety.of.walking" ~ "Walking",
    variable == "Percentage.of.people.who.say.they.would.walk.more.because.of.improved.conditions" ~ "Walking",
    variable == "Share.of.all.trips.by.car" ~ "Cars: Fossil Fuels",
    variable == "Share.of.all.trips.to.work/school.by.car" ~ "Cars: Fossil Fuels",
    variable == "Distance.traveled.by.car.per.weekday" ~ "Cars: Fossil Fuels",
    variable == "Share.of.zero-car.households" ~ "Cars: Fossil Fuels", 
    variable == "Number/share.of.passenger.cars.by.type" ~ "Cars: Fossil Fuels", 
    variable == "Cost.of.a.taxi.journey.for.specified.distance" ~ "Cars: Fossil Fuels", 
    variable == "Perceived.safety.across.different.modes.(fossil.fuel.cars)" ~ "Cars: Fossil Fuels",  
    variable == "Electric.vehicle.market.share" ~ "Cars: Electric and Alternative Fuels", 
    variable == "Share.of.new.registered.vehicles.that.are.EV" ~ "Cars: Electric and Alternative Fuels",  
    variable == "Number.of.electric.charging.points.and/or.hydrogen.filling.stations" ~ "Cars: Electric and Alternative Fuels",  
    variable == "Electric.vehicle.charging.station.density" ~ "Cars: Electric and Alternative Fuels",   
    variable == "Perceived.safety.across.different.modes.(electric.cars)" ~ "Cars: Electric and Alternative Fuels",  
    variable == "Number.of.parking.spaces.per.square.meter.of.floor.area.by.building.type" ~ "Use of Space",
    variable == "Number.of.parked.cars.to.number.of.legal.parking.spaces" ~ "Use of Space", 
    variable == "Percent.of.total.urban.area.allocated.to.parking" ~ "Use of Space",  
    variable == "Distribution.of.area.between.city.houses.by.type.(e.g.,.sidewalks,.bike.paths,.roadways,.roadways.with.bus.priority,.etc.)" ~ "Use of Space", 
    variable == "Percent.of.total.street.length.that.is.<=.30km/h.or.entirely.car.free" ~ "Use of Space",  
    variable == "Percent.of.total.walkable.routes.>=50km.that.are.off-street.(e.g.,.pedestrian.only.paths,.through.green.space,.etc.)" ~ "Use of Space",  
    variable == "Percent.of.roads.>=.40km/hr.that.have.segregated.or.parallel.cycle.lanes" ~ "Use of Space",   
    variable == "Percent.of.time.lost.to.traffic.compared.to.free-flow.conditions" ~ "Use of Space",  
    str_detect(variable, "electric") ~ "Cars: Electric and Alternative Fuels", 
    str_detect(variable, "public.transport") ~ "Public Transport",  
    str_detect(variable, "public.transit") ~ "Public Transport",  
    str_detect(variable, "bus.services") ~ "Public Transport", 
    str_detect(variable, "daily.departures") ~ "Public Transport", 
    str_detect(variable, "monthly.PT.ticket") ~ "Public Transport", 
    str_detect(variable, "fines") ~ "Incentives and Policies", 
    str_detect(variable, "investment") ~ "Incentives and Policies",  
    str_detect(variable, "Cycling") ~ "Cycling", 
    str_detect(variable, "cycling") ~ "Cycling", 
    str_detect(variable, "Cycle") ~ "Cycling",  
    str_detect(variable, "cycle") ~ "Cycling", 
    str_detect(variable, "Bicycle") ~ "Cycling", 
    str_detect(variable, "injured") ~ "Cycling", 
    str_detect(variable, "bike") ~ "Cycling",  
    str_detect(variable, "Bike") ~ "Cycling", 
    str_detect(variable, "EV") ~ "Cars: Electric and Alternative Fuels",  
    str_detect(variable, "zone") ~ "Incentives and Policies", 
    str_detect(variable, "commitment") ~ "Incentives and Policies", 
    str_detect(variable, "walking") ~ "Walking",
    str_detect(variable, "PT.stop") ~ "Public Transport",  
    str_detect(variable, "wait.time") ~ "Public Transport" 
  )) %>% # now let's clean the variable names, 
  #making them into snake_case, which will require identifying the parts of the variable name that 
  #have multiple spaces between words and replacing them with a single underscore, then replacing any single spaces 
  # or periods with a single underscore, than stripping any other special characters, making them lowercase, and 
  # removing any underscores that are at the beginning or end of the variable name, 
  # and then finally making the full variable name lowercase. And removing underscores at the end of the variable name.
  mutate(variable = str_replace_all(variable, " ", "_")) %>% 
  mutate(variable = str_replace_all(variable, "\\.", "_")) %>% 
  mutate(variable = str_replace_all(variable, "[^a-zA-Z0-9_]", "_")) %>% 
  mutate(variable = str_replace_all(variable, "_+", "_")) %>% 
  mutate(variable = str_replace_all(variable, "_$", "")) %>% 
  mutate(variable = tolower(variable))

list_of_vars <- lt_synthetic_long %>%
    distinct(variable, sustainable_mobility_category)
# label by hand higher_is_better for each variable
#write.csv(list_of_vars, "list_of_vars.csv", row.names = FALSE)

higher_is_better <- read.csv("list_of_vars.csv")

lt_synthetic_long_final <- lt_synthetic_long %>%
    left_join(higher_is_better, by = c("variable", "sustainable_mobility_category"))

# We will now apply the min_max_norm function to each variable in the lt_synthetic_long dataframe.
lt_synthetic_long_minmax <- lt_synthetic_long_final %>% 
    group_by(variable, higher_is_better) %>%
    mutate(min_max_norm = if_else(higher_is_better, (value - min(value)) / (max(value) - min(value)), (max(value) - value) / (max(value) - min(value)))) %>%
    ungroup()

# We will now calculate the average of the min-max normalized values for each category for each municipality. 
# We will use the mean function to calculate the average. 
lt_synthetic_long_mean <- lt_synthetic_long_minmax %>% 
    group_by(Municipality, sustainable_mobility_category) %>%
    summarise(average = mean(min_max_norm)) %>%
    ungroup()

# We will now calculate the Sustainable Mobility Index for each municipality. 
# The Sustainable Mobility Index will be the weighted sum of the average of the min-max normalized values for each category. 
# The weights are as follows: 
# - Environmental Impact: 0.05
# - Incentives and Policies: 0.05
# - Cycling: 0.2
# - Public Transport: 0.2
# - Walking: 0.2
# - Cars: Fossil Fuels: 0.2
# - Cars: Electric and Alternative Fuels: 0.10
# - Use of Space: 0.05
# We will use these weights the calculate the Sustainable Mobility Index for each municipality. 
# We will use the mutate function to calculate the Sustainable Mobility Index for each municipality. 
lt_synthetic_index <- lt_synthetic_long_mean %>%
    pivot_wider(names_from = sustainable_mobility_category, values_from = average) %>% 
    mutate(sustainable_mobility_index = 
    0.05 * `Environmental Impact` + 
    0.05 * `Incentives and Policies` + 
    0.2 * Cycling + 
    0.15 * `Public Transport` + 
    0.2 * Walking + 
    0.2 * `Cars: Fossil Fuels` + 
    0.1 * `Cars: Electric and Alternative Fuels` + 
    0.05 * `Use of Space`)





