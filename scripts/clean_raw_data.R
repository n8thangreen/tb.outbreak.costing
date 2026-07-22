# clean raw incident data


library(dplyr)
library(purrr)
library(readxl)

# # original data set
# excel_path <- here::here("../../data", "Birmingham", "incidents2.xlsx")

# 2026 data set
excel_path <- here::here("../../data/cleaned - Incidents Data start date 2010 onwards current.xlsx")

# list all sheets/tabs in the Excel workbook
sheets <- readxl::excel_sheets(path = excel_path)

# normalize column names using strict text matching (case-insensitive exact string match)
normalize_col_names <- function(df) {
  igra_aliases <- c("screened with igra", "total by igra", "total no igra")
  mantoux_aliases <- c("screened with mantoux", "total by mantoux", "total no mantoux", "no mantoux")
  setting_aliases <- c("setting", "setting2")
  sputum_smear_positive <- c("sputum  smear positive", "sputum smear positive")
  screening_status <- c("status", "screening status")
  date_case_closed <- "date case closed"
  
  col_names <- names(df)
  
  new_names <- sapply(col_names, function(col) {
    col_clean <- tolower(gsub("\\s+", " ", trimws(col)))
    
    if (col_clean %in% igra_aliases) {
      return("Total No IGRA")
    } else if (col_clean %in% mantoux_aliases) {
      return("Total No Mantoux")
    } else if (col_clean %in% sputum_smear_positive) {
      return("Sputum smear positive")
    } else if (col_clean %in% screening_status) {
      return("Screening status")
    } else if (col_clean %in% date_case_closed) {
      return("Date case closed")
    } else if (col_clean %in% setting_aliases) {
      return("setting")
    } else {
      return(col)
    }
  }, USE.NAMES = FALSE)
  
  names(df) <- new_names
  df
}

# read all sheets and combine into one long table with a year column
dat_raw <- purrr::map_dfr(sheets, function(sheet) {
  df <- readxl::read_xlsx(path = excel_path, sheet = sheet, col_types = "text")
  if (!"year" %in% names(df)) {
    df$year <- sheet
  }
  df <- normalize_col_names(df)
  df
})

cols_to_select <- c("year",
                    "setting",
                    "Total No identified",
                    "Total No Screened",
                    "Total No IGRA",
                    "Total No Mantoux",
                    "Latent")

cols_to_select <- intersect(cols_to_select, names(dat_raw))
dat <- dat_raw[, cols_to_select]

# coerce numeric columns
num_cols <- c("year", "Total No identified", "Total No Screened", "Total No IGRA", "Total No Mantoux", "Latent")
num_cols <- intersect(num_cols, names(dat))

for (col in num_cols) {
  dat[[col]] <- as.numeric(dat[[col]])
}

# remove incidents with missing data
dat <- dat[dat$year %in% 2013:2018, ]
if ("Total No identified" %in% names(dat)) dat <- dat[!is.na(dat$`Total No identified`), ]
if ("Total No Screened" %in% names(dat)) dat <- dat[!is.na(dat$`Total No Screened`), ]

if ("Latent" %in% names(dat)) dat$Latent[is.na(dat$Latent)] <- 0

if ("setting" %in% names(dat)) {
  dat$setting <- factor(dat$setting)
}

if ("Total No Screened" %in% names(dat) && "Total No identified" %in% names(dat)) {
  dat$p_screen <- dat$`Total No Screened` / dat$`Total No identified`
}

if ("Latent" %in% names(dat) && "Total No Screened" %in% names(dat)) {
  dat$p_ltbi <- dat$Latent / dat$`Total No Screened`
  dat$Latent <- pmin(dat$`Total No Screened`, dat$Latent)
}

write.csv(dat, file = "input_data/cleaned_data.csv", row.names = FALSE)
