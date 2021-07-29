#

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

