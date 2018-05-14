##------------------------------------
## this file pulls in raw street data and creates intersections

source('../load_packages.R')

data_folder = '../../data/'

load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------##

## load in the school data...

## school_file = paste0(data_folder, 'raw/school/2016-2017-Master-School-List-20170327.xlsx')
## school_master = read_excel(path = school_file, sheet = 2)

school_xy_file = paste0(data_folder, 'raw/school/Schools.csv')
school_xy <- read_csv(school_xy_file)

## in previous life / version, we used to read in a huge
## heap of files, removing schools / adding school / yada yada.
## turns out the gain is nearly zero, at a huge loss of reproducibility
## v high correlation (0.943) between enrollment in this file, and
## when all others are combined. Sad.

##------------------------------------##

## filter away those without enrollment

school_xy %<>% filter(!is.na(ENROLLMENT))

## now, adjust GRADE_LEVEL:
## first, some renaming...:
school_xy %<>%
    mutate(
        grade = 
            case_when(
                GRADE_LEVEL == 'Elem/Mid/High' ~ 'ELEMENTARYMIDDLEHIGH',
                GRADE_LEVEL == 'Elem/Middle' ~ 'ELEMENTARYMIDDLE',
                GRADE_LEVEL == 'Middle/High' ~ 'MIDDLEHIGH',
                GRADE_LEVEL == 'Elementary School' ~ 'ELEMENTARY',
                GRADE_LEVEL == 'Middle School' ~ 'MIDDLE',
                GRADE_LEVEL == 'High School' ~ 'HIGH',
                TRUE ~ toupper(GRADE_LEVEL)))
                                
el_levels = c('ELEMENTARY', 'ELEMENTARYMIDDLE', 'MIDDLE', 'ELEMENTARYMIDDLEHIGH')
hi_levels = c('HIGH', 'MIDDLEHIGH', 'ELEMENTARYMIDDLEHIGH')

school_xy %<>%
    mutate(
        elem_cat = grade %in% el_levels,
        high_cat = grade %in% hi_levels)

## allowing 28 schools to be both,
## 33 are neither (pre-k, kinder, etc), remove:

school_xy %<>% filter(elem_cat | high_cat)

## fix type:
school_xy$TYPE_SPECIFIC[school_xy$TYPE_SPECIFIC == 'DISTRICT'] = 'District'
## add charter to private
## NOTE: this is controversial... see:
## https://www.washingtonpost.com/news/local/wp/2015/02/04/are-charter-schools-public-or-private
## but we only have three, sooooo
school_xy$TYPE_SPECIFIC[school_xy$TYPE_SPECIFIC == 'Charter'] = 'Private'

## last step, spatial:
school_data <- st_as_sf(school_xy,
                      coords = c('X', 'Y'),
                      crs = st_crs(block_geom))

save(school_data,
     file = paste0(data_folder, 'school_data.rdata'))

##------------------------------------
    
