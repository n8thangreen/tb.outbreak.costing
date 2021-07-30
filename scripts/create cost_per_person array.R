
# tb incident costing study
# create cost_per_person array
# N Green
#


# this is direct from the Excel model, sheet Incident
dat <-
  readxl::read_excel(
    "costing xls tools/TB_incident_contact_tracing_costing/101019-TB_incident_contact_tracing_costing-Birmingham.xlsm",
    range = "Incidents!P3:U54")


dat$setting <- as.factor(dat$setting)
dat$year <- factor(dat$year)

# include a total level
dat$setting <- factor(dat$setting, levels = c(levels(dat$setting), "total"))

# set empty settings to total
dat$setting[is.na(dat$setting)] <- "total"

write.csv(dat, file = "../data/cost_per_person.csv", row.names = FALSE)
