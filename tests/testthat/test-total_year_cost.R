# tests/testthat/test-total_year_cost.R
library(dplyr)
library(magrittr)

test_that("no incidents", {
  expect_equal(
    total_year_cost(inc_sample = 0,
                    id_per_inc = 1,
                    screen_per_inc = 0,
                    ltbi_per_inc = 0), 0)
  expect_equal(
    total_year_cost(inc_sample = 0,
                    id_per_inc = 1,
                    screen_per_inc = 1,
                    ltbi_per_inc = 0), 0)
  expect_equal(
    total_year_cost(inc_sample = 0,
                    id_per_inc = 1,
                    screen_per_inc = 1,
                    ltbi_per_inc = 1), 0)
})


test_that("maximum number later in the cascade from earlier", {
  # Boundary validations check that inputs satisfy:
  # id_per_inc >= screen_per_inc >= ltbi_per_inc
  expect_error(
    total_year_cost(inc_sample = 1,
                    id_per_inc = 0,
                    screen_per_inc = 10,
                    ltbi_per_inc = 0)
  )
  expect_error(
    total_year_cost(inc_sample = 1,
                    id_per_inc = 1,
                    screen_per_inc = 0,
                    ltbi_per_inc = 10)
  )
})


test_that("comparison with mean pop costs", {
  # Load cleaned data locally
  data_path <- here::here("input_data/cleaned_data.csv")
  skip_if_not(file.exists(data_path), "cleaned_data.csv not available")
  
  dat <- read.csv(data_path, check.names = FALSE)
  
  # Calculate mean populations
  n_pop <- 
    dat %>%
    group_by(setting, year) %>%
    summarise(id = mean(`Total No identified`, na.rm = TRUE),
              screen = mean(`Total No Screened`, na.rm = TRUE),
              ltbi = mean(Latent, na.rm = TRUE),
              .groups = "drop") %>% 
    group_by(setting) %>% 
    summarise(id = mean(id, na.rm = TRUE),
              screen = mean(screen, na.rm = TRUE),
              ltbi = mean(ltbi, na.rm = TRUE),
              .groups = "drop")
  
  # Mean yearly incidents
  n_inc <-
    dat %>%
    group_by(year) %>% 
    count(setting) %>% 
    group_by(setting) %>% 
    summarise(inc = mean(n, na.rm = TRUE),
              .groups = "drop")
  
  out <- numeric(5)
  for (i in 1:5) {
    out[i] <-
      total_year_cost(
        inc_sample = n_inc$inc[i],
        id_per_inc = n_pop$id[i],
        screen_per_inc = n_pop$screen[i],
        ltbi_per_inc = n_pop$ltbi[i])
  }
  
  expect_type(out, "double")
  expect_length(out, 5)
  expect_true(all(out >= 0))
})
