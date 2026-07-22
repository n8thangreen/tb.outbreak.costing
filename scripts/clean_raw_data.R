# clean raw incident data


library(dplyr)
library(purrr)
library(readxl)

excel_path <- here::here("../../data", "Birmingham", "incidents2.xlsx")

# Read password securely from environment variable EXCEL_PASSWORD or interactive prompt
excel_password <- Sys.getenv("EXCEL_PASSWORD")
if (excel_password == "" && interactive()) {
  if (requireNamespace("getPass", quietly = TRUE)) {
    excel_password <- getPass::getPass("Enter Excel workbook password: ")
  } else {
    excel_password <- readline("Enter Excel workbook password: ")
  }
}

# Helper function to open password-protected Excel files on Windows
open_excel_wb <- function(path, password = NULL) {
  if (is.null(password) || password == "") {
    return(path)
  }
  norm_path <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  temp_file <- tempfile(fileext = ".xlsx")
  norm_temp <- normalizePath(temp_file, winslash = "\\", mustWork = FALSE)
  
  ps_cmd <- sprintf(
    '$excel = New-Object -ComObject Excel.Application; $excel.Visible = $false; $excel.DisplayAlerts = $false; $wb = $excel.Workbooks.Open(\"%s\", 0, $true, 5, \"%s\"); $wb.SaveAs(\"%s\", 51); $wb.Close($false); $excel.Quit()',
    norm_path, password, norm_temp
  )
  
  system2("powershell", args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", ps_cmd), stdout = FALSE, stderr = FALSE)
  
  return(temp_file)
}

# Decrypt to temporary file if password is provided
working_path <- open_excel_wb(excel_path, password = excel_password)
if (working_path != excel_path) {
  on.exit(if (file.exists(working_path)) unlink(working_path), add = TRUE)
}

# list all sheets/tabs in the Excel workbook
sheets <- readxl::excel_sheets(working_path)

# read all sheets and combine into one long table, creating a year column if missing
dat_raw <- purrr::map_dfr(sheets, function(sheet) {
  df <- readxl::read_xlsx(path = working_path, sheet = sheet)
  if (!"year" %in% names(df)) {
    df$year <- sheet
  }
  df
})

setting_col <- if ("setting2" %in% names(dat_raw)) "setting2" else "setting"

dat <- dat_raw[, c("year",
                   setting_col,
                   "Total No identified",
                   "Total No Screened",
                   "Latent")]

names(dat)[names(dat) == setting_col] <- "setting"

# coerce year to numeric and remove incidents with missing data
dat$year <- as.numeric(dat$year)
dat <- dat[dat$year %in% 2013:2018, ]
dat <- dat[!is.na(dat$`Total No identified`), ]
dat$`Total No Screened` <- as.numeric(dat$`Total No Screened`)
dat <- dat[!is.na(dat$`Total No Screened`), ]

dat$Latent <- as.numeric(dat$Latent)
dat$Latent[is.na(dat$Latent)] <- 0

dat <-
  dat %>% 
  mutate(
    setting = factor(setting),
    p_screen = `Total No Screened`/`Total No identified`,  # prop screened of identified per incident
    p_ltbi = `Latent`/`Total No Screened`)                 # prop ltbi of screened per incident

##TODO: confirm what this should be
# row 132 too many latent. should be 1?
dat <-
  mutate(dat, Latent = pmin(`Total No Screened`, Latent))

write.csv(dat, file = "data/cleaned_data.csv", row.names = FALSE)
