# total costs per year and setting

test_that("no incidents", {
  expect_equal(
    total_year_cost(inc_sample = 0,
                    id_per_inc = 1,
                    screen_per_inc = 0,
                    ltbi_per_inc = 0), 0)
  expect_equal(
    total_year_cost(inc_sample = 0,
                    id_per_inc = 0,
                    screen_per_inc = 1,
                    ltbi_per_inc = 0), 0)
  expect_equal(
    total_year_cost(inc_sample = 0,
                    id_per_inc = 0,
                    screen_per_inc = 0,
                    ltbi_per_inc = 1), 0)
})


test_that("maximum number later in the cascade from earlier", {
  ## should this return an error or
  ## revert to the maximum value?
  
  # # no-one identified
  # expect_equal(
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 0,
  #                   screen_per_inc = 10,
  #                   ltbi_per_inc = 0),
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 0,
  #                   screen_per_inc = 0,
  #                   ltbi_per_inc = 0))
  # 
  # # one identified
  # expect_equal(
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 1,
  #                   screen_per_inc = 10,
  #                   ltbi_per_inc = 0),
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 1,
  #                   screen_per_inc = 1,
  #                   ltbi_per_inc = 0))
  # 
  # # no-one screened
  # expect_equal(
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 1,
  #                   screen_per_inc = 0,
  #                   ltbi_per_inc = 10),
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 1,
  #                   screen_per_inc = 0,
  #                   ltbi_per_inc = 0))
  # 
  # # one screened
  # expect_equal(
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 1,
  #                   screen_per_inc = 1,
  #                   ltbi_per_inc = 10),
  #   total_year_cost(inc_sample = 1,
  #                   id_per_inc = 1,
  #                   screen_per_inc = 1,
  #                   ltbi_per_inc = 1))
})


## expect posteriors costs similar to mean pop costs
## i.e. if we take mean at start or end of cost calc

## mean pops
n_pop <- 
  dat %>%
  group_by(setting, year) %>%
  summarise(id = mean(`Total No identified`),
            screen = mean(`Total No Screened`),
            ltbi = mean(Latent)) %>% 
  group_by(setting) %>% 
  summarise(id = mean(id),
            screen = mean(screen),
            ltbi = mean(ltbi))

## mean yearly incidents
n_inc <-
  dat %>%
  group_by(year) %>% 
  count(setting) %>% 
  group_by(setting) %>% 
  summarise(inc = mean(n))

for (i in 1:5) {
  out[i] <-
    total_year_cost(
      inc_sample = n_inc$inc[i],
      id_per_inc = n_pop$id[i],
      screen_per_inc = n_pop$screen[i],
      ltbi_per_inc = n_pop$ltbi[i])
}
out
