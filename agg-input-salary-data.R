library(tidyverse)
library(RSocrata)
library(jsonlite)
library(uuid)
library(readxl)
library(httr)
library(rvest)
library(janitor)

soc_token = "XqcQxLEeCSWFHPAnTvjMXsPaU"

# Public Agencies --------

## New York City ----------

soc_api_endpoint <- "https://data.cityofnewyork.us/resource/k397-673e.json"

depts_of_interest = c("DEPARTMENT OF CITY PLANNING",
                      "DEPARTMENT OF TRANSPORTATION")

soc_query = paste0(soc_api_endpoint,"?$where=agency_name in('DEPARTMENT OF CITY PLANNING','DEPARTMENT OF TRANSPORTATION')")

nyc_raw = read.socrata(soc_query, app_token = soc_token)

nyc_employees <- nyc_raw %>%
  distinct(first_name,last_name,agency_start_date) %>%
  mutate(employee_id = UUIDgenerate(n=n()))

nyc_clean <- nyc_raw %>%
  left_join(nyc_employees) %>%
  select(fiscal_year,employee_id,agency_name,title_description,
         base_salary,pay_basis,regular_hours,regular_gross_paid) %>%
  arrange(employee_id,fiscal_year) %>%
  mutate(annual_fte_salary = case_when(
    pay_basis == "per Annum" ~ as.numeric(base_salary),
    pay_basis == "per Day" ~ as.numeric(base_salary)*(2080/8),
    pay_basis == "per Hour" ~ as.numeric(base_salary)*2080,
    pay_basis == "Prorated Annual" ~ as.numeric(base_salary)
  )) %>%
  select(fiscal_year, employee_id, agency_name, title_description, annual_fte_salary) %>%
  rename(job_title = title_description,
         department = agency_name)


## Chicago ------
soc_api_endpoint = "https://data.cityofchicago.org/resource/xzkq-xp2w.json"

soc_query = paste0(soc_api_endpoint)

chi_raw = read.socrata(soc_query, app_token = soc_token)

chi_clean <- chi_raw %>%
  filter(department %in% c("TRANSPORTN", "HOUSING & ECON DEV")) %>%
  mutate(fiscal_year = 2022,
         employee_id = UUIDgenerate(n=n())) %>%
  mutate(annual_fte_salary = case_when(
    salary_or_hourly == "Hourly" ~ as.numeric(hourly_rate) * 2080,
    TRUE ~ as.numeric(annual_salary)
  )) %>%
  rename(job_title = job_titles) %>%
  select(fiscal_year, employee_id, department, job_title, annual_fte_salary)

## Boston ----

download_page_url = "https://data.boston.gov/dataset/employee-earnings-report"

page_items <- read_html(download_page_url) %>%
  html_nodes(".btn-group") %>%
  html_nodes("a")

download_links <- tibble(
  path = page_items %>%
    html_attr("href")
) %>%
  filter(str_sub(path,-3,-1)=="csv")

bos_pre_clean <- download_links %>%
  mutate(raw = map(path,~read_csv(.x)))

bos_pre_clean_bound <- bos_pre_clean %>%
  mutate(year = 2021:2011) %>%
  mutate(clean = map2(raw,year,function(raw_df,yr){
    
    # raw_df = bos_pre_clean$raw[[9]]
    
    print(yr)
    
    raw_df %>%
      clean_names() %>%
      select(name,
             any_of(c("department_name","department")),
             title,
             any_of(c("total_gross","total_earnings"))) %>%
      mutate(across(.cols = any_of(c("total_gross","total_earnings")),
                    function(x){str_replace(x,fixed("$"),"") %>% as.numeric()})) %>%
      rename_with(.cols = any_of(c("total_gross","total_earnings")),
                  function(x){"annual_fte_salary"}) %>%
      rename_with(.cols = any_of(c("department_name","department")),
                  function(x){"department"}) %>%
      rename(job_title = title)
  })) %>%
  select(-path,-raw) %>%
  unnest(clean)

bos_clean = bos_pre_clean_bound %>%
  left_join(bos_pre_clean_bound %>%
              distinct(name) %>%
              mutate(employee_id = UUIDgenerate(n=n()))) %>%
  select(-name) %>%
  rename(fiscal_year = year) %>%
  arrange(employee_id, fiscal_year) %>%
  filter(department %in% c("Transportation Department", "Traffic Division"))
  
## DC --------

download_page_url = "https://dchr.dc.gov/public-employee-salary-information"

## Seattle --------

soc_api_endpoint = "https://data.seattle.gov/resource/2khk-5ukd.json"

soc_query = paste0(soc_api_endpoint)

sea_raw = read.socrata(soc_query, app_token = soc_token)

sea_clean <- sea_raw %>%
  filter(department %in% c("Seattle Dept of Transportation", 
                           "Planning & Comm Development")) %>%
  mutate(fiscal_year = 2022,
         employee_id = UUIDgenerate(n=n())) %>%
  mutate(annual_fte_salary = as.numeric(hourly_rate)*2080) %>%
  select(fiscal_year, employee_id, department, job_title, annual_fte_salary)


## Portland, OR -------

download_page_url = "https://www.portland.gov/bhr/open-data-analytics"

tmp_2020 = tempfile()
download.file("https://www.portland.gov/sites/default/files/2021/cop-wage-information-cy2020_0.xlsx",destfile = tmp_2020)
pdx_2020_raw_salary = read_excel(tmp_2020,sheet = 2,skip = 1) 
pdx_2020_raw_emps = read_excel(tmp_2020,sheet = 4,skip = 1) 
tmp_2021 = tempfile()
download.file("https://www.portland.gov/sites/default/files/2022/cop-wage-information-cy2021.xlsx",destfile = tmp_2021)
pdx_2021_raw_salary = read_excel(tmp_2021,sheet = 2,skip = 1) 
pdx_2021_raw_emps = read_excel(tmp_2021,sheet = 3,skip = 1) 


pdx_2020_clean = pdx_2020_raw_salary %>%
  rename(join_key = Name) %>%
  left_join(pdx_2020_raw_emps %>%
              mutate(join_key = paste0(str_to_upper(`Last name`),
                                       ", ",
                                       str_to_upper(`First name`)) %>%
                       str_replace(fixed("."),"") %>%
                       str_replace(fixed("-")," "))) 

pdx_2021_clean= pdx_2021_raw_salary %>%
  rename(join_key = Name) %>%
  left_join(pdx_2021_raw_emps %>%
              mutate(join_key = paste0(str_to_upper(`Last name`),
                                       ", ",
                                       str_to_upper(`First name`)) %>%
                       str_replace(fixed("."),"") %>%
                       str_replace(fixed("-")," "))) 

pdx_pre_clean <- bind_rows(pdx_2020_clean %>% mutate(fiscal_year = 2020),
                       pdx_2021_clean %>% mutate(fiscal_year = 2021)) 

pdx_clean <- pdx_pre_clean %>%
  left_join(pdx_pre_clean %>% distinct(`First name`,`Last name`) %>%
              mutate(employee_id = UUIDgenerate(n=n()))) %>%
  rename(department = Bureau, job_title = Position,
         annual_fte_salary = `Total Gross = Remuneration Statement`) %>%
  filter(str_detect(str_to_lower(department),"transportation") |
           str_detect(str_to_lower(department),"planning")) %>%
  select(fiscal_year, employee_id, department, job_title, annual_fte_salary) %>%
  arrange(employee_id, fiscal_year)

## Austin, TX ---------

## Los Angeles, CA --------

soc_api_endpoint = "https://controllerdata.lacity.org/resource/g9h8-fvhu.json"

soc_query = paste0(soc_api_endpoint,"?$where=fms_department_title in('CITY PLANNING','TRANSPORTATION','PUBLIC WORKS - ENGINEERING')")

lax_raw = read.socrata(soc_query, app_token = soc_token)

lax_employees <- lax_raw %>%
  distinct(record_nbr) %>%
  mutate(employee_id = UUIDgenerate(n=n()))

lax_clean <- lax_raw %>%
  left_join(lax_employees) %>%
  rename(department = fms_department_title,
         fiscal_year = pay_year) %>%
  filter(employment_type == "FULL_TIME") %>%
  mutate(annual_fte_salary = as.numeric(regular_pay)) %>%
  select(fiscal_year, employee_id, department, job_title, annual_fte_salary) 

## San Francisco, CA --------

download_page_url = "https://sfdhr.org/classification-compensation-database"

tmp_2020 = tempfile()
download.file("https://sfdhr.org/sites/default/files/documents/Classification-and-Compensation/Hourly-Rates-of-Pay-by-Classification-and-Step-FY21-22.xlsx",destfile = tmp_2020)
sfo_2020_raw= read_excel(tmp_2020,sheet = 1,skip = 4) 
tmp_2021 = tempfile()
download.file("https://sfdhr.org/sites/default/files/documents/Classification-and-Compensation/Hourly-Rates-of-Pay-by-Classification-and-Step-FY20-21.xlsx",destfile = tmp_2021)
sfo_2021_raw= read_excel(tmp_2021,sheet = 1,skip = 4) 

## Oakland, CA ------------

# BLS Data ---------

# ## Quarterly Census of Employment and Wages Data ---------
# 
# download_page_url = "https://www.bls.gov/cew/downloadable-data-files.htm"

# Convenience Samples ---------

## Ask a Manager Salary Survey ------------

gs_link_2021 = "https://docs.google.com/spreadsheets/d/1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw/edit?resourcekey#gid=1625408792"
gs_link_2019 = "https://docs.google.com/spreadsheets/d/1rGCKXIKt-7l5gX06NAwO3pjqEHh-oPXtB8ihkp0vGWo/edit#gid=382484678"