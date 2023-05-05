
# CDISC Repository --------------------------------------------------------

# https://github.com/cdisc-org/sdtm-adam-pilot-project.git

################# 
### LIBRARIES ###
#################


# Installing --------------------------------------------------------------

CRAN_pkgs <- c("htmltools", "magrittr", "broom", "car", "checkmate",
               "cowplot", "dplyr", "emmeans", "forcats", "ggplot2", "gridExtra", 
               "gtable", "labeling", "lifecycle", "Rdpack", "rlang", "scales",
               "tibble", "tidyr", "r2rtf", "remotes", "haven", "qdapRegex")

install.packages(CRAN_pkgs)

remotes::install_github("insightsengineering/formatters")
remotes::install_github("Roche/rtables")
remotes::install_github("insightsengineering/tern")

# OBS: "formatters" package loads some synthetic ADaM datasets (ex_adxx)

# Loading -----------------------------------------------------------------

lapply(c(CRAN_pkgs,     # Loading packages
         "formatters",
         "rtables",
         "tern"), 
       require, 
       character.only = TRUE)


#################### 
### READING DATA ###
####################

# Path --------------------------------------------------------------------

path <- "updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01"

# SDTM --------------------------------------------------------------------

sdtm_filenames <- list.files(paste0(path, "/tabulations/sdtm"), pattern = "\\.xpt$", full.names = TRUE) # Listing all ".xpt" files in "sdtm" folder
list_domains <- lapply(sdtm_filenames, function(x) haven::read_xpt(x)) # Reading all domains
names(list_domains) <- ex_between(sdtm_filenames, "sdtm/", ".") # Extracting domains' names from files
list2env(list_domains, .GlobalEnv) # Loading domains in Environment 


# ADaM --------------------------------------------------------------------

ADaM_filenames <- list.files(paste0(path, "/analysis/adam/datasets"), pattern = "\\.xpt$", full.names = TRUE) # Listing all ".xpt" files in "ADaM" folder
list_datasets <- lapply(ADaM_filenames, function(x) haven::read_xpt(x)) # Reading all datasets
names(list_datasets) <- ex_between(ADaM_filenames, "adam/datasets/", ".") # Extracting datasets' names from files
list2env(list_datasets, .GlobalEnv) # Loading domains in Environment 

######################### 
### DATA MANIPULATION ###
#########################

adsl %>% str() # Data structure

adsl$ARM %<>% factor()
adsl$SEX %<>% factor()
adsl$RACE %<>% factor()
adsl$ETHNIC %<>% factor()


##############
### TABLES ###
##############

### Simplest table

lyt <- basic_table() %>% 
       analyze("AGE")    # "Mean", by default

build_table(lyt, adsl)

### A little more information 

lyt <- basic_table(title = "Title",
                   subtitles = "Subtitle",
                   main_footer = "Footer",
                   prov_footer = "Subfooter") %>% 
       analyze("AGE", 
               sd, # Standard deviation value
               var_labels = "Age", # Variable label
               show_labels = "visible") # Setting variable label to visible

build_table(lyt, adsl)


lyt2 <- basic_table(title = "Title",
                   subtitles = "Subtitle",
                   main_footer = "Footer",
                   prov_footer = "Subfooter",
                   inset = 2) %>% # Number of spaces to start table
        analyze("AGE", 
                sd, 
                format = "xx.x", # Standard deviation with one decimal
                var_labels = "Age",
                show_labels = "visible") 

build_table(lyt2, adsl)

### What if we want more than one descriptive statistics?

lyt <- basic_table(title = "Title",
                   subtitles = "Subtitle",
                   main_footer = "Footer",
                   prov_footer = "Subfooter") %>% 
      analyze("AGE", 
              c(mean, median)) # Mean an median values

build_table(lyt, adsl) # Does not work :/

# Let's create a function!

desc_stats <- function(x){ # Descriptive statistics
  
  in_rows("n" = sum(!is.na(x)),            # Sample size
          "mean (sd)" = c(mean(x), sd(x)), # Mean and standard deviation
          "median" = median(x),            # Median
          "min - max" = range(x),          # Range
          .formats = c("n" = "xx",
                       "mean (sd)" = "xx.x (xx.x)",
                       "median" = "xx.x",
                       "min - max" = "xx.x - xx.x"))
  
}

lyt <- basic_table(title = "Title",
                   subtitles = "Subtitle",
                   main_footer = "Footer",
                   prov_footer = "Subfooter",
                   show_colcounts = TRUE) %>% # Count column sample size
       analyze(c("AGE", "WEIGHTBL"), 
               desc_stats, # AGE and WEIGHT descriptive statistics
               var_labels = c("Age", "Baseline weight"),
               show_labels = "visible") 

build_table(lyt, adsl) 

### And the categorical variables???

lyt <- basic_table(title = "Title",
                   subtitles = "Subtitle",
                   main_footer = "Footer",
                   prov_footer = "Subfooter",
                   show_colcounts = TRUE) %>% # Count column sample size
       analyze(c("AGE", "WEIGHTBL", "SEX"), desc_stats) # AGE, WEIGHT and SEX descriptive statistics

build_table(lyt, adsl) # Does not work :/

# Let's create a GENERAL function!

gen_desc_stats <- function(x){ # Descriptive statistics
  
  if(is.numeric(x)){
    
    in_rows("n" = sum(!is.na(x)),            # Sample size
            "mean (sd)" = c(mean(x), sd(x)), # Mean and standard deviation
            "median" = median(x),            # Median
            "min - max" = range(x),          # Range
            .formats = c("n" = "xx",
                         "mean (sd)" = "xx.x (xx.x)",
                         "median" = "xx.x",
                         "min - max" = "xx.x - xx.x"))
    
  } else if (is.factor(x) || is.character(x)){
    
    in_rows(.list = list_wrap_x(table)(x))
    
  } else {
    
    stop("type not supported")
    
  }
  
}


lyt <- basic_table(title = "Title",
                   subtitles = "Subtitle",
                   main_footer = "Footer",
                   prov_footer = "Subfooter",
                   show_colcounts = TRUE) %>% # Count column sample size
       analyze(c("AGE", "WEIGHTBL", "SEX"), # AGE, WEIGHT and SEX descriptive statistics
               gen_desc_stats,
               var_labels = c("Age", "Baseline weight", "Gender"),
               show_labels = "visible") 

build_table(lyt, adsl) # IT WORKS :)

### Don't want to create anything, want to use what is available... tern package!

lyt2 <- rtables::basic_table(title = "Title",
                             subtitles = "Subtitle",
                             main_footer = "Footer",
                             prov_footer = "Subfooter",
                             show_colcounts = TRUE) %>%
        tern::summarize_vars(vars = c("AGE", "WEIGHTBL", "SEX"),
                             var_labels = c("Age", "Baseline weight", "Gender"),
                             show_labels = "visible")

build_table(lyt2, adsl)

### How to cross variables?

lyt <- rtables::basic_table(title = "Title",
                            subtitles = "Subtitle",
                            main_footer = "Footer",
                            prov_footer = "Subfooter",
                            show_colcounts = TRUE) %>%
       rtables::split_cols_by("ARM") %>%
       tern::summarize_vars(vars = c("AGE", "SEX"),
                            var_labels = c("Age", "Gender"),
                            show_labels = "visible") 

build_table(lyt, adsl)

lyt2 <- rtables::basic_table(title = "Title",
                            subtitles = "Subtitle",
                            main_footer = "Footer",
                            prov_footer = "Subfooter",
                            show_colcounts = TRUE) %>%
        rtables::split_rows_by("SEX", 
                               label_pos = "topleft", 
                               split_label = "Gender") %>%
        tern::summarize_vars(vars = c("AGE"),
                             var_labels = c("Age"),
                             show_labels = "visible") 


build_table(lyt2, adsl)

lyt3 <- rtables::basic_table(title = "Title",
                             subtitles = "Subtitle",
                             main_footer = "Footer",
                             prov_footer = "Subfooter",
                             show_colcounts = TRUE) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_rows_by("SEX", 
                               label_pos = "topleft", 
                               split_label = "Gender") %>%
        tern::summarize_vars(vars = c("AGE"),
                             var_labels = c("Age"),
                             show_labels = "visible")


build_table(lyt3, adsl)

### Categoriziation of numerical variables

lyt <- rtables::basic_table(title = "Title",
                             subtitles = "Subtitle",
                             main_footer = "Footer",
                             prov_footer = "Subfooter",
                             show_colcounts = TRUE) %>%
       rtables::split_cols_by("AGEGR1") %>%
       rtables::split_rows_by("SEX", 
                              label_pos = "topleft", 
                              split_label = "Gender") %>%
       tern::summarize_vars(vars = c("WEIGHTBL"),
                            var_labels = c("Baseline weight"),
                            show_labels = "visible") 


build_table(lyt, adsl)

lyt2 <- rtables::basic_table(title = "Title",
                            subtitles = "Subtitle",
                            main_footer = "Footer",
                            prov_footer = "Subfooter",
                            show_colcounts = TRUE) %>%
        rtables::split_cols_by_cuts("AGE", cuts = c(1, 60, 65, 70, 1000), # Interval limits
                                    cutlabels = c("<60", "60-65", "65-70", "70+")) %>% # Interval labels
        rtables::split_rows_by("SEX", 
                               label_pos = "topleft", 
                               split_label = "Gender") %>%
        tern::summarize_vars(vars = c("WEIGHTBL"),
                             var_labels = c("Baseline weight"),
                             show_labels = "visible") 


build_table(lyt2, adsl)

### Percentages by rows, levels, sublevels...

lyt <- rtables::basic_table(title = "Title",
                            subtitles = "Subtitle",
                            main_footer = "Footer",
                            prov_footer = "Subfooter",
                            show_colcounts = TRUE) %>%
       rtables::split_cols_by("ARM") %>%
       rtables::split_rows_by("SEX", 
                              label_pos = "topleft", 
                              split_label = "Gender") %>%
       rtables::split_rows_by("ETHNIC", 
                              label_pos = "topleft", 
                              split_label = "Ethnic") %>%
       tern::summarize_vars(vars = c("AGE"),
                            .stats = c("n",
                                       "range"),
                            var_labels = c("Age"),
                            show_labels = "visible")


build_table(lyt, adsl)

# Percentage for SEX by ARM 

lyt2 <- rtables::basic_table(title = "Title",
                                   subtitles = "Subtitle",
                                   main_footer = "Footer",
                                   prov_footer = "Subfooter",
                                   show_colcounts = TRUE) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_rows_by("SEX", 
                               label_pos = "topleft", 
                               split_label = "Gender") %>%
        rtables::summarize_row_groups() %>% 
        rtables::split_rows_by("ETHNIC", 
                               label_pos = "topleft", 
                               split_label = "Ethnic") %>%
        tern::summarize_vars(vars = c("AGE"),
                             .stats = c("n",
                                        "range"),
                             var_labels = c("Age"),
                             show_labels = "visible")
      

build_table(lyt2, adsl)
        
# Percentage for SEX AND ETHNIC by ARM

lyt3 <- rtables::basic_table(title = "Title",
                                   subtitles = "Subtitle",
                                   main_footer = "Footer",
                                   prov_footer = "Subfooter",
                                   show_colcounts = TRUE) %>%
        rtables::split_cols_by("ARM") %>%
        rtables::split_rows_by("SEX", 
                               label_pos = "topleft", 
                               split_label = "Gender") %>%

        rtables::split_rows_by("ETHNIC", 
                               label_pos = "topleft", 
                               split_label = "Ethnic") %>%
        rtables::summarize_row_groups() %>% 
        tern::summarize_vars(vars = c("AGE"),
                             .stats = c("n",
                                        "range"),
                             var_labels = c("Age"),
                             show_labels = "visible")


build_table(lyt3, adsl)

### Analysis WITH and WITHOUT sublevels

lyt <- rtables::basic_table(title = "Title",
                            subtitles = "Subtitle",
                            main_footer = "Footer",
                            prov_footer = "Subfooter",
                            show_colcounts = TRUE) %>%
       rtables::split_cols_by("ARM") %>%
       rtables::split_rows_by("SEX",
                              split_fun = drop_split_levels, 
                              label_pos = "topleft", 
                              split_label = "Gender") %>%
       tern::summarize_vars(vars = c("AGE"),
                            .stats = c("n",
                                       "mean_sd"),
                            var_labels = c("Age"),
                            show_labels = "visible") %>% 
       rtables::split_rows_by("SEX",
                              split_fun = keep_split_levels(c("F", 
                                                              "M"))) %>%
       rtables::split_rows_by("RACE", 
                              split_fun = drop_split_levels, 
                              label_pos = "topleft", 
                              split_label = "Race") %>%
       tern::summarize_vars(vars = c("AGE"),
                            .stats = c("n",
                                       "mean_sd"),
                            var_labels = c("Age"),
                            show_labels = "visible")
      

build_table(lyt, adsl) %>% Viewer

tbl <- build_table(lyt, adsl) 

tf <- tempfile(fileext = ".pdf")
export_as_pdf(tbl, file = tf)
