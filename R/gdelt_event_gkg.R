#' Loads needed packages
#'
#' @param required_packages
#'
#' @return
#' @export
#'
#' @examples
#' load_neeeded_packages(c('magrittr', 'dplyr))
load_needed_packages <- function(required_packages = c('dplyr')) {
  loaded_packages <- gsub("package:", "", search())
  package_to_load <- required_packages[!required_packages %in%
                                         loaded_packages]
  if (length(package_to_load) > 0) {
    lapply(package_to_load, library, character.only = T)
  }
}

#' Loads gdelt v2 Global Knowledge Graph master log data, updated every 15 minutes
#'
#' @return
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate with_tz
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_15_minute_log()
get_urls_gkg_15_minute_log <- function() {

  url <-
    'http://data.gdeltproject.org/gdeltv2/masterfilelist.txt'

  log_df <-
    url %>%
    readr::read_tsv(col_names = F) %>%
    tidyr::separate(
      col = X1,
      into = c('id.file', 'id.hash', 'url.data'),
      sep = '\\ '
    ) %>%
    suppressWarnings()

  log_df <-
    log_df %>%
    dplyr::mutate(
      time_stamp.file = url.data %>% str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')
    ) %>%
    tidyr::separate(time_stamp.file,
                    into = c('timestamp', 'name.file', 'type.file', 'is.zip'))

  log_df <-
    log_df %>%
    dplyr::mutate(
      date.data.hms = timestamp %>% as.numeric %>% ymd_hms() %>% with_tz(Sys.timezone()),
      date.data = date.data.hms %>% as.Date(),
      type.file = type.file %>% str_to_lower(),
      id.file = id.file %>% as.integer()
    ) %>%
    dplyr::mutate_each_(funs(str_trim), c('id.hash', 'name.file', 'url.data')) %>%
    suppressWarnings()

  return(log_df)

}

#' Gets GDELT Event data, by year from 1979-2005, by year month 2006 - 2013, then by dat
#'
#' @param return_message
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @importFrom lubridate ymd
#' @export
#'
#' @examples
#' get_urls_gdelt_event_log()
get_urls_gdelt_event_log <- function(return_message = T) {
  url <-
    'http://data.gdeltproject.org/events/md5sums'

  url.data <-
    url %>%
    readr::read_tsv(col_names = F) %>%
    tidyr::separate(
      col = X1,
      into = c('id.hash', 'stem.data'),
      sep = '\\  '
    ) %>%
    dplyr::mutate(
      url.data = 'http://data.gdeltproject.org/events/' %>% paste0(stem.data),
      id.database.gdelt = 'EVENTS',
      is.zip_file = ifelse(stem.data %>% str_detect(".zip"), T, F)
    )

  url.data <-
    url.data %>%
    separate(
      col = stem.data,
      into = c('period.data', 'name.file', 'type.file', 'zip_file'),
      sep = '\\.'
    ) %>%
    dplyr::select(-zip_file) %>%
    dplyr::mutate(
      period.data = ifelse(period.data == 'GDELT', type.file, period.data),
      is.days_data = ifelse(period.data %>% nchar == 8, T, F)
    ) %>%
    dplyr::select(-c(name.file, type.file)) %>%
    suppressWarnings()

  url.data <-
    url.data %>%
    dplyr::filter(is.days_data == F) %>%
    dplyr::mutate(date.data = NA) %>%
    bind_rows(
      url.data %>%
        dplyr::filter(is.days_data == T) %>%
        dplyr::mutate(date.data = period.data %>% ymd %>% as.Date())
    ) %>%
    dplyr::select(id.hash,
                  date.data,
                  is.zip_file,
                  is.days_data,
                  url.data,
                  everything())

  if (return_message == T) {
    count.files <-
      url.data %>%
      nrow

    min.date <-
      url.data$date.data %>% min(na.rm = T)

    max.date <-
      url.data$date.data %>% max(na.rm = T)

    "You got " %>%
      paste0(count.files,
             ' GDELT Global Knowledge Graph URLS from ',
             min.date,
             ' to ',
             max.date) %>%
      message()
  }

  return(url.data)
}

#' Gets most recent GKG log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @export
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_gkg_most_recent_log <- function() {
  log_df <-
    'http://data.gdeltproject.org/gdeltv2/lastupdate.txt' %>%
    readr::read_tsv(col_names = F) %>%
    tidyr::separate(
      col = X1,
      into = c('id.file', 'id.hash', 'url.data'),
      sep = '\\ '
    )

  log_df %<>%
    dplyr::mutate(
      time_stamp.file = url.data %>% str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')
    ) %>%
    tidyr::separate(time_stamp.file,
                    into = c('timestamp', 'name.file', 'type.file', 'is.zip')) %>%
    dplyr::mutate(
      type.file = type.file %>% str_to_lower(),
      is.zip = ifelse(is.zip %>% str_detect("ZIP|zip"), T, F),
      id.file = id.file %>% as.integer()
    ) %>%
    dplyr::mutate_each_(funs(str_trim), c('id.hash', 'name.file', 'url.data'))

  return(log_df)
}

#' Gets Global Knowledge Graph summary files by day since April 2013
#'
#' @param remove_count_files
#' @param return_message
#'
#' @return
#' @export
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom lubridate ymd
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_daily_summaries(remove_count_files = T)
get_urls_gkg_daily_summaries <-
  function(remove_count_files = F,
           return_message = T) {
    url <-
      'http://data.gdeltproject.org/gkg/md5sums'

    url.data <-
      url %>%
      readr::read_tsv(col_names = F) %>%
      tidyr::separate(
        col = X1,
        into = c('id.hash', 'stem.data'),
        sep = '\\  '
      ) %>%
      dplyr::mutate(url.data = 'http://data.gdeltproject.org/gkg/' %>% paste0(stem.data),
             id.database.gdelt = 'gkg') %>%
      separate(
        col = stem.data,
        into = c('date.data', 'name.file', 'type.file', 'is.zip_file'),
        sep = '\\.'
      ) %>%
      dplyr::mutate(
        is.zip_file = ifelse(is.zip_file == "zip", T, F),
        is.gkg_count_file = ifelse(name.file == 'gkgcounts', T, F),
        date.data = date.data %>% ymd %>% as.Date()
      ) %>%
      dplyr::select(-c(name.file, type.file)) %>%
      dplyr::select(id.hash,
                    date.data,
                    is.zip_file,
                    is.gkg_count_file,
                    url.data,
                    everything())

    if (remove_count_files == T) {
      url.data <-
        url.data %>%
        dplyr::filter(is.gkg_count_file == F)
    }

    if (return_message == T) {
      count.files <-
        url.data %>%
        nrow

      min.date <-
        url.data$date.data %>% min(na.rm = T)

      max.date <-
        url.data$date.data %>% max(na.rm = T)

      "You got " %>%
        paste0(count.files,
               ' GDELT Global Knowledge Graph URLS from ',
               min.date,
               ' to ',
               max.date) %>%
        message()
    }

    return(url.data)
  }

#' Retrives most recent GDELT Global Content Analysis Measures (GCAM) code book
#'
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @examples
#' get_codes_gcam()
get_codes_gcam <- function() {
  url <-
    'http://data.gdeltproject.org/documentation/GCAM-MASTER-CODEBOOK.TXT'
  gcam_data <-
    url %>%
    read_tsv()

  names(gcam_data) <-
    c(
      'id.gcam',
      'id.dictonary',
      'id.dimension',
      'type',
      'code.language',
      'dictonary.human_name',
      'dimension.human_name',
      'dictonary.citation'
    )

  return(gcam_data)
}

#' Retrives GDELT CAMEO religion code book
#'
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @examples
#' get_codes_cameo_religion
get_codes_cameo_religion <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.religion.txt'
  code_df <-
    url %>%
    readr::read_tsv

  names(code_df) <-
    c('code.religion', 'religion')

  return(code_df)
}

#' Retrieves GDELT CAMEO country code book
#'
#'
#' @return
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' get_codes_cameo_country
get_codes_cameo_country <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.country.txt'
  code_df <-
    url %>%
    read_tsv

  names(code_df) <-
    c('code.country', 'country')

  return(code_df)
}

#' Retrieves GDELT CAMEO type code book
#'
#' @return
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' get_codes_cameo_type()
get_codes_cameo_type <- function() {
  url <- 'http://gdeltproject.org/data/lookups/CAMEO.type.txt'
  code_df <-
    url %>%
    read_tsv

  names(code_df) <-
    c('code.type', 'type')

  return(code_df)
}

#' Retrieves CAMEO CAMEO event code book
#'
#'
#' @return
#' @export
#'
#' @examples
#' get_codes_cameo_events()

get_codes_cameo_events <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.eventcodes.txt'
  code_df <-
    url %>%
    read_tsv

  names(code_df) <-
    c('code.event', 'event')

  code_df <-
    code_df %>%
    dplyr::mutate(is.parent_code = ifelse(code.event %>% nchar == 2, T, F),
                  id.parent_code = code.event %>% substr(1,2)) %>%
    dplyr::select(id.parent_code, everything())

  return(code_df)
}

#' Retrieves GDELT CAMEO known group code book
#'
#' @return
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @export
#' @examples
#' get_codes_cameo_known_groups()
get_codes_cameo_known_groups <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.knowngroup.txt'
  code_df <-
    url %>%
    read_tsv

  names(code_df) <-
    c('code.known_group', 'known_group')

  return(code_df)
}

#' Retrieves GDELT CAMEO ethnic code book
#'
#' @return
#' @export
#'
#' @examples
#' get_codes_cameo_ethnic()
get_codes_cameo_ethnic <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.ethnic.txt'
  code_df <-
    url %>%
    read_tsv

  names(code_df) <-
    c('code.ethnic', 'ethnicity')

  return(code_df)
}

#' Retrieves GKG theme code book
#'
#' @return
#' @export
#'
#' @examples
#' get_codes_gkg_themes()
get_codes_gkg_themes <- function(split_word_bank_codes = F) {
  url <-
    'http://data.gdeltproject.org/documentation/GKG-MASTER-THEMELIST.TXT'
  code_df <-
    url %>%
    read_tsv(col_names = F)

  names(code_df) <-
    c('code.theme')
  code_df <-
    code_df %>%
    mutate(
      code.2 = code.theme %>% str_to_lower(),
      is.world_bank_code = ifelse(code.2 %>% str_detect("wb_"), T, F),
      is.economic_event = ifelse(code.2 %>% str_detect("econ_"), T, F),
      is.social_event = ifelse(code.2 %>% str_detect("soc"), T, F),
      is.tax_event = ifelse(code.2 %>% str_detect("tax_"), T, F),
      is.social_event = ifelse(code.2 %>% str_detect("soc_"), T, F),
      is.military_event = ifelse(code.2 %>% str_detect("military|mil_"), T, F),
      is.government_event = ifelse(code.2 %>% str_detect("gov_|government"), T, F),
      is.medical_event = ifelse(code.2 %>% str_detect("med_|medical"), T, F),
      is.agression_act = ifelse(code.2 %>% str_detect("act_"), T, F),
      is.media_event = ifelse(code.2 %>% str_detect("media_|_media"), T, F),
      is.emergency_event = ifelse(code.2 %>% str_detect("emerg_"), T, F),
      is.movement = ifelse(code.2 %>% str_detect("movement_"), T, F),
      is.criminal_event = ifelse(code.2 %>% str_detect("crime|crm_"), T, F)
    ) %>%
    dplyr::select(-code.2)

  wb_codes <-
    code_df %>%
    dplyr::filter(is.world_bank_code == T)

  wb_codes <-
    wb_codes %>%
    mutate(code.theme = code.theme %>% sub('\\_', '\\.', .)) %>%
    separate(
      code.theme,
      into = c('id.dictionary', 'code_name'),
      remove = F,
      sep = '\\.'
    ) %>%
    mutate(code_name = code_name %>% sub('\\_', '\\.', .)) %>%
    separate(
      code_name,
      into = c('id.wb_code', 'code_name'),
      remove = T,
      sep = '\\.'
    ) %>%
    mutate(
      id.wb_code = id.wb_code %>% as.numeric,
      code_name = code_name %>% str_replace_all('\\_', ' ') %>% str_to_lower
    ) %>%
    dplyr::select(-id.dictionary)

  non_wb <-
    code_df %>%
    dplyr::filter(is.world_bank_code == F)

  code_df <-
    non_wb %>%
    bind_rows(wb_codes)

  return(code_df)

}

#' Retrieves GDELT event summary by period
#'
#' @param period can be \code{c("yearly", "daily", "monthly")}
#' @param by_country can be \code{c(TRUE, FALSE)}
#' @param return_message
#' @importFrom readr read_csv
#' @return
#' @export
#'
#' @examples
#' get_data_gdelt_period_event_totals(period = 'monthly', by_country = T)

get_data_gdelt_period_event_totals <- function(period = 'yearly',
                                                by_country = T,
                                                return_message = T) {
  periods <-
    c('daily', 'monthly', 'yearly')
  if (!period %in% periods) {
    "Sorry periods can only be:\n" %>%
      stop(paste0(paste0(periods, collapse = '\n')))
  }

  if (by_country == T) {
    period_slug <-
      period %>%
      paste0('_country.csv')
  } else {
    period_slug <-
      period %>%
      paste0('.csv')
  }
  base <-
    'http://data.gdeltproject.org/normfiles/'

  url_data <-
    base %>%
    paste0(period_slug)

  period_data <-
    url_data %>%
    read_csv(col_names = F)

  if (by_country == T) {
    names(period_data) <-
      c('id.date', 'id.country', 'count.events')
  } else {
    names(period_data) <-
      c('id.date', 'count.events')
  }

  period_data <-
    period_data %>%
    mutate(period, is.by_country = by_country)

  if (period == 'daily') {
    period_data <-
      period_data %>%
      mutate(date = id.date %>% ymd %>% as.Date()) %>%
      dplyr::select(period, is.by_country, date, everything())
  }

  if (period == 'monthly') {
    period_data <-
      period_data %>%
      mutate(year.month = id.date) %>%
      dplyr::select(period, is.by_country, year.month, everything())
  }

  if (period == 'yearly') {
    period_data <-
      period_data %>%
      mutate(year = id.date) %>%
      dplyr::select(period, is.by_country, everything())
  }

  if (return_message == T)  {
    from_date <-
      period_data$id.date %>% min

    to_date <-
      period_data$id.date %>% max

    total_events <-
      period_data$count.events %>% sum() / 1000000
    events_slug <-
      total_events %>% paste0(" million GDELT events from ")
    "There have been " %>%
      paste0(events_slug, from_date,
             ' to ', to_date) %>%
      message
  }
  return(period_data)
}


#' Retrieves GDELT event database schema
#'
#' @return
#' @importFrom dplyr data_frame
#' @examples
#' get_schema_gdelt_events

get_schema_gdelt_events <- function() {
  gdelt_events_schema <-
    data_frame(
      name.gdelt = c(
        "GLOBALEVENTID",
        "SQLDATE",
        "MonthYear",
        "Year",
        "FractionDate",
        "Actor1Code",
        "Actor1Name",
        "Actor1CountryCode",
        "Actor1KnownGroupCode",
        "Actor1EthnicCode",
        "Actor1Religion1Code",
        "Actor1Religion2Code",
        "Actor1Type1Code",
        "Actor1Type2Code",
        "Actor1Type3Code",
        "Actor2Code",
        "Actor2Name",
        "Actor2CountryCode",
        "Actor2KnownGroupCode",
        "Actor2EthnicCode",
        "Actor2Religion1Code",
        "Actor2Religion2Code",
        "Actor2Type1Code",
        "Actor2Type2Code",
        "Actor2Type3Code",
        "IsRootEvent",
        "EventCode",
        "EventBaseCode",
        "EventRootCode",
        "QuadClass",
        "GoldsteinScale",
        "NumMentions",
        "NumSources",
        "NumArticles",
        "AvgTone",
        "Actor1Geo_Type",
        "Actor1Geo_FullName",
        "Actor1Geo_CountryCode",
        "Actor1Geo_ADM1Code",
        "Actor1Geo_ADM2Code",
        "Actor1Geo_Lat",
        "Actor1Geo_Long",
        "Actor1Geo_FeatureID",
        "Actor2Geo_Type",
        "Actor2Geo_FullName",
        "Actor2Geo_CountryCode",
        "Actor2Geo_ADM1Code",
        "Actor2Geo_ADM2Code",
        "Actor2Geo_Lat",
        "Actor2Geo_Long",
        "Actor2Geo_FeatureID",
        "ActionGeo_Type",
        "ActionGeo_FullName",
        "ActionGeo_CountryCode",
        "ActionGeo_ADM1Code",
        "ActionGeo_ADM2Code",
        "ActionGeo_Lat",
        "ActionGeo_Long",
        "ActionGeo_FeatureID",
        "DATEADDED",
        "SOURCEURL"
      ),
      name.actual = c(
        "id.global_event",
        "date.event",
        "month_year.event",
        "year.event",
        "date.fraction.event",
        "code.actor.1",
        "name.actor.1",
        "code.country.actor.1",
        "code.known_group.actor.1",
        "code.ethnic.actor.1",
        "code.religion.actor.1",
        "code.religion_2.actor.1",
        "code.type.actor.1",
        "code.type_2.actor.1",
        "code.type_3.actor.1",
        "code.actor.2",
        "name.actor.2",
        "code.country.actor.2",
        "code.known_group.actor.2",
        "code.ethnic.actor.2",
        "code.religion.actor.2",
        "code.religion_2.actor.2",
        "code.type.actor.2",
        "code.type_2.actor.2",
        "code.type_3.actor.3",
        "is.root_event",
        "code.event",
        "code.event.base",
        "code.event.root",
        "class.quad",
        "score.goldstein_scale",
        "count.mentions",
        "count.sources",
        "count.articles",
        "avg.tone",
        "id.location_type.actor.1",
        "location.actor.1",
        "id.country.actor.1",
        "id.adm1code.actor.1",
        "id.adm2code.actor.1",
        "latitude.actor.1",
        "longitude.actor.1",
        "id.feature.actor.1",
        "id.location_type.actor.2",
        "location.actor.2",
        "id.country.actor.2",
        "id.adm1code.actor.2",
        "id.adm2code.actor.2",
        "latitude.actor.2",
        "longitude.actor.2",
        "id.feature.actor.2",
        "id.location_type.action",
        "location.action",
        "id.country.action",
        "id.adm1code.action",
        "id.adm2code.action",
        "latitude.action",
        "longitude.action",
        "id.feature.action",
        "date.hms.added",
        "url.source"
      )
    )
  return(gdelt_events_schema)
}

#' Gets gkg general schema
#'
#' @return
#'
#' @examples
#' get_schema_gkg_general()
get_schema_gkg_general <- function() {
  schema_df <-
    data_frame(
      name.gdelt = c(
        "GKGRECORDID",
        "DATE",
        "SourceCollectionIdentifier",
        "SourceCommonName",
        "DocumentIdentifier",
        "Counts",
        "V2Counts",
        "Themes",
        "V2Themes",
        "Locations",
        "V2Locations",
        "Persons",
        "V2Persons",
        "Organizations",
        "V2Organizations",
        "V2Tone",
        "Dates",
        "GCAM",
        "SharingImage",
        "RelatedImages",
        "SocialImageEmbeds",
        "SocialVideoEmbeds",
        "Quotations",
        "AllNames",
        "Amounts",
        "TranslationInfo",
        "Extras",
        "NUMARTS",
        "COUNTS",
        "THEMES",
        "LOCATIONS",
        "PERSONS",
        "ORGANIZATIONS",
        "TONE",
        "CAMEOEVENTIDS",
        "SOURCES",
        "SOURCEURLS"

      ),
      name.actual = c(
        "id.gkg.record",
        "date.url",
        "id.source_collection_identifer",
        "name.source",
        "document.source",
        "counts",
        "counts.char_loc",
        "themes",
        "themes.char_loc",
        "locations",
        "locations.char_loc",
        "persons",
        "persons.char_loc",
        "organizations",
        "organizations.char_loc",
        "tone",
        "dates",
        "gcam",
        "url.image",
        "url.image.related",
        "url.social_media_image.embeds",
        "url.social_media_video.embeds",
        "quotations",
        "mentioned.names.counts",
        "mentioned.numerics.counts",
        "translation.information",
        "xml.extras",
        "count.articles",
        "counts",
        "themes",
        "locations",
        "persons",
        "organizations",
        "tone",
        "id.cameo_events",
        "sources",
        "url.sources"
      )
    )
  return(schema_df)
}

#' Gets gkg count schema
#'
#' @return
#'
#' @examples
#' get_schema_gkg_counts
get_schema_gkg_counts <- function() {
  counts_schema <-
    data_frame(
      name.gdelt = c(
        "DATE",
        "NUMARTS",
        "COUNTTYPE",
        "NUMBER",
        "OBJECTTYPE",
        "GEO_TYPE",
        "GEO_FULLNAME",
        "GEO_COUNTRYCODE",
        "GEO_ADM1CODE",
        "GEO_LAT",
        "GEO_LONG",
        "GEO_FEATUREID",
        "CAMEOEVENTIDS",
        "SOURCES",
        "SOURCEURLS"
      ),
      name.actual = c(
        "date.event",
        "count.articles",
        "type.event",
        "count.object",
        "type.object",
        "id.location_type",
        "location",
        "id.country",
        "id.adm1code.action",
        "latitude",
        "longitude",
        "id.feature",
        "id.cameo_events",
        "sources",
        "url.sources"
      )
    )
  return(counts_schema)
}

#' Gets gkg mention schema
#'
#' @return
#'
#' @examples
#' get_schema_gkg_mentions()
get_schema_gkg_mentions <- function() {
  mentions_schema <-
    data_frame(
      name.gdelt =
        c(
          "GLOBALEVENTID",
          "EventTimeDate",
          "MentionTimeDate",
          "MentionType",
          "MentionSourceName",
          "MentionIdentifier",
          "SentenceID",
          "Actor1CharOffset",
          "Actor2CharOffset",
          "ActionCharOffset",
          "InRawText",
          "Confidence",
          "MentionDocLen",
          "MentionDocTone",
          "MentionDocTranslationInfo",
          "Extras"
        ),
      name.actual =
        c(
          "id.global_event",
          "date.event",
          "date.mention",
          "id.mention_type",
          "name.source",
          "document.source",
          "id.sentence",
          "char.loc.actor.1",
          "char.loc.actor.2",
          "char.loc.action",
          "is.in_raw_text",
          "score.confidence",
          "length.mentioned_document",
          "tone.mentioned_document",
          "translation.mentioned_document",
          "extra"
        )
    )

  return(mentions_schema)
}

#' Retrieves GDELT Event or GKG data from a given URL
#'
#' @param url
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @importFrom purrr flatten_chr
#' @importFrom tidyr extract_numeric(.)
#' @import dplyr
#' @import utils
#' @import urltools
#' @importFrom curl curl_download
#' @return
#' @export
#'
#' @examples
#' get_gdelt_url_data(url = "http://data.gdeltproject.org/gdeltv2/20160531000000.gkg.csv.zip", file_directory = 'Desktop/temp_gdelt_data', remove_files = T, empty_trash = T, return_message = T)
get_gdelt_url_data <-
  function(url = "http://data.gdeltproject.org/gdeltv2/20160531000000.gkg.csv.zip",
           file_directory = 'Desktop/temp_gdelt_data',
           remove_files = T,
           empty_trash = T,
           return_message = T) {
    files <-
      url %>%
      str_replace_all(
        'http://data.gdeltproject.org/gdeltv2/|http://data.gdeltproject.org/gkg/|http://data.gdeltproject.org/events/',
        ''
      ) %>%
      str_split('\\.') %>%
      flatten_chr

    file_name <-
      files %>%
      .[1] %>%
      paste0(".zip")

    file_type <-
      files %>%
      .[2]

    temp.dir <-
      file_directory

    file_path <-
      temp.dir %>% str_split('/') %>% flatten_chr() %>% .[1:length(.)] %>% paste0(collapse = '/')

    if (!dir.exists(paths = file_path)) {
      dir.create(temp.dir)
    }

    file <-
      temp.dir %>%
      paste0('/', file_name)

    url %>%
      curl_download(url = ., destfile = file)

    file %>%
      unzip(exdir = paste0(temp.dir, '/'))

    dir_files <-
      temp.dir %>%
      list.files()

    csv_file_loc <-
      dir_files[dir_files %>%
                  str_detect(".csv|.CSV")] %>%
      paste0(temp.dir, '/', .)

    gdelt_cols <-
      csv_file_loc %>%
      read_tsv(col_names = F,
               n_max = 1) %>% ncol %>% suppressWarnings() %>% extract_numeric(.)()


    if (gdelt_cols == 16) {
      gdelt_data <-
        csv_file_loc %>%
        read_tsv(col_names = F)

      names(gdelt_data) <-
        get_schema_gkg_mentions() %>% .$name.actual

      gdelt_data <-
        gdelt_data %>%
        mutate(
          date_time.event = date.event %>% ymd_hms %>% with_tz(Sys.timezone()),
          date.event = date_time.event %>% as.Date(),
          date_time.mention = date.mention %>% ymd_hms %>% with_tz(Sys.timezone()),
          date.mention = date_time.mention %>% as.Date()
        ) %>%
        dplyr::select(id.global_event,
                      date_time.event,
                      date_time.mention,
                      everything()) %>%
        dplyr::left_join(data_frame(
          id.mention_type = 1:6,
          mention_type = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
        )) %>%
        dplyr::select(id.global_event:id.mention_type,
                      mention_type,
                      everything()) %>%
        suppressMessages()

      gdelt_data <-
        gdelt_data %>%
        mutate_each_(funs(as.logical(.)),
                     gdelt_data %>% dplyr::select(matches("is.")) %>% names)

    }

    if (gdelt_cols == 15) {
      gdelt_data <-
        csv_file_loc %>%
        read_tsv(col_names = T)

      names(gdelt_data) <-
        get_schema_gkg_counts() %>% .$name.actual

      gdelt_data <-
        gdelt_data %>%
        dplyr::left_join(data_frame(
          id.location_type = 1:5,
          location_type = c(
            'country',
            'us_state',
            'us_city',
            'world_city',
            'world_state'
          )
        )) %>%
        suppressMessages() %>%
        dplyr::mutate(
          id.record = 1:n(),
          id.gkg.record = date.event %>% paste0('.', id.record),
          url.sources = url.sources %>% str_replace_all("<UDIV>", ';')
        ) %>%
        dplyr::mutate(date.event = date.event %>% ymd()) %>%
        dplyr::select(date.event:id.location_type, location_type, everything()) %>%
        dplyr::select(id.record, id.gkg.record, everything())
    }

    if (gdelt_cols == 61) {
      load_needed_packages(c('urltools'))
      gdelt_data <-
        csv_file_loc %>%
        readr::read_tsv(col_names = F)
      names(gdelt_data) <-
        get_schema_gdelt_events() %>% .$name.actual

      gdelt_data <-
        gdelt_data %>%
        dplyr::rename(date_time.url = date.hms.added) %>%
        dplyr::mutate(
          date.event = date.event %>% lubridate::ymd,
          date_time.url = date_time.url %>% ymd_hms() %>% with_tz(Sys.timezone()),
          name.source = url.source %>% domain() %>% str_replace_all("www.", '')
        )

      gdelt_data <-
        gdelt_data %>%
        dplyr::mutate_each_(funs(as.logical(.)),
                     gdelt_data %>% dplyr::select(matches("is.")) %>% names)

      gdelt_data <-
        gdelt_data %>%
        dplyr::left_join(data_frame(
          class.quad = 1:4,
          quad = c(
            'Verbal Cooperation',
            'Material Cooperation',
            'Verbal Conflict',
            'Material Conflict'
          )
        )) %>%
        suppressMessages()
    }

    if (gdelt_cols == 57) {
      load_needed_packages(c('urltools'))
      gdelt_data <-
        csv_file_loc %>%
        readr::read_tsv(col_names = F)
      names(gdelt_data) <-
        c(
          "id.global_event",
          "date.event",
          "month_year.event",
          "year.event",
          "date.fraction.event",
          "code.actor.1",
          "name.actor.1",
          "code.country.actor.1",
          "code.known_group.actor.1",
          "code.ethnic.actor.1",
          "code.religion.actor.1",
          "code.religion_2.actor.1",
          "code.type.actor.1",
          "code.type_2.actor.1",
          "code.type_3.actor.1",
          "code.actor.2",
          "name.actor.2",
          "code.country.actor.2",
          "code.known_group.actor.2",
          "code.ethnic.actor.2",
          "code.religion.actor.2",
          "code.religion_2.actor.2",
          "code.type.actor.2",
          "code.type_2.actor.2",
          "code.type_3.actor.3",
          "is.root_event",
          "code.event",
          "code.event.base",
          "code.event.root",
          "class.quad",
          "score.goldstein_scale",
          "count.mentions",
          "count.sources",
          "count.articles",
          "avg.tone",
          "id.location_type.actor.1",
          "location.actor.1",
          "id.country.actor.1",
          "id.adm1code.actor.1",
          "latitude.actor.1",
          "longitude.actor.1",
          "id.feature.actor.1",
          "id.location_type.actor.2",
          "location.actor.2",
          "id.country.actor.2",
          "id.adm1code.actor.2",
          "latitude.actor.2",
          "longitude.actor.2",
          "id.feature.actor.2",
          "id.location_type.action",
          "location.action",
          "id.country.action",
          "id.adm1code.action",
          "latitude.action",
          "longitude.action",
          "id.feature.action",
          "date.added"
        )

      gdelt_data <-
        gdelt_data %>%
        dplyr::rename(date.url = date.added) %>%
        dplyr::mutate(
          date.event = date.event %>% lubridate::ymd,
          date.url = date.url %>% lubridate::ymd
        ) %>%
        suppressWarnings()

      gdelt_data <-
        gdelt_data %>%
        dplyr::mutate_each_(funs(as.logical(.)),
                     gdelt_data %>% dplyr::select(matches("is.")) %>% names)

      gdelt_data <-
        gdelt_data %>%
        dplyr::left_join(data_frame(
          class.quad = 1:4,
          quad = c(
            'Verbal Cooperation',
            'Material Cooperation',
            'Verbal Conflict',
            'Material Conflict'
          )
        )) %>%
        suppressMessages()
    }

    if (gdelt_cols == 58) {
      load_needed_packages(c('urltools'))
      gdelt_data <-
        csv_file_loc %>%
        readr::read_tsv(col_names = F)
      names(gdelt_data) <-
        c(
          "id.global_event",
          "date.event",
          "month_year.event",
          "year.event",
          "date.fraction.event",
          "code.actor.1",
          "name.actor.1",
          "code.country.actor.1",
          "code.known_group.actor.1",
          "code.ethnic.actor.1",
          "code.religion.actor.1",
          "code.religion_2.actor.1",
          "code.type.actor.1",
          "code.type_2.actor.1",
          "code.type_3.actor.1",
          "code.actor.2",
          "name.actor.2",
          "code.country.actor.2",
          "code.known_group.actor.2",
          "code.ethnic.actor.2",
          "code.religion.actor.2",
          "code.religion_2.actor.2",
          "code.type.actor.2",
          "code.type_2.actor.2",
          "code.type_3.actor.3",
          "is.root_event",
          "code.event",
          "code.event.base",
          "code.event.root",
          "class.quad",
          "score.goldstein_scale",
          "count.mentions",
          "count.sources",
          "count.articles",
          "avg.tone",
          "id.location_type.actor.1",
          "location.actor.1",
          "id.country.actor.1",
          "id.adm1code.actor.1",
          "latitude.actor.1",
          "longitude.actor.1",
          "id.feature.actor.1",
          "id.location_type.actor.2",
          "location.actor.2",
          "id.country.actor.2",
          "id.adm1code.actor.2",
          "latitude.actor.2",
          "longitude.actor.2",
          "id.feature.actor.2",
          "id.location_type.action",
          "location.action",
          "id.country.action",
          "id.adm1code.action",
          "latitude.action",
          "longitude.action",
          "id.feature.action",
          "date.added",
          "url.source"
        )

      gdelt_data <-
        gdelt_data %>%
        dplyr::rename(date_time.url = date.added) %>%
        dplyr::mutate(
          date.event = date.event %>% lubridate::ymd,
          date_time.url %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone()),
          date.url = date_time.url %>% as.Date(),
          name.source = url.source %>% domain() %>% str_replace_all("www.", '')
        ) %>%
        suppressWarnings()

      gdelt_data <-
        gdelt_data %>%
        dplyr::mutate_each_(funs(as.logical(.)),
                     gdelt_data %>% dplyr::select(matches("is.")) %>% names)

      gdelt_data <-
        gdelt_data %>%
        dplyr::left_join(data_frame(
          class.quad = 1:4,
          quad = c(
            'Verbal Cooperation',
            'Material Cooperation',
            'Verbal Conflict',
            'Material Conflict.'
          )
        )) %>%
        suppressMessages()


    }

    if (gdelt_cols == 11) {
      gdelt_data <-
        csv_file_loc %>%
        readr::read_tsv(col_names = T)

      schema_df <-
        get_schema_gkg_general()

      names(gdelt_data) <-
        schema_df$name.actual[names(gdelt_data) %>% match(schema_df$name.gdelt)]

      names(gdelt_data)[1] <-
        c('date')

      gdelt_data <-
        gdelt_data %>%
        dplyr::mutate(
          id.record = 1:n(),
          id.gkg.record = date %>% paste0('.', id.record),
          date = date %>% lubridate::ymd(),
          url.sources = url.sources %>% str_replace_all("<UDIV>", ';')
        ) %>%
        dplyr::select(id.record, id.gkg.record, everything())

    }

    if (gdelt_cols == 27) {
      gdelt_data <-
        csv_file_loc %>%
        readr::read_tsv(col_names = F)

      schema_df <-
        get_schema_gkg_general()

      names(gdelt_data) <-
        schema_df$name.actual[1:27]

      gdelt_data <-
        gdelt_data %>%
        dplyr::mutate(
          id.source_collection_identifer = id.source_collection_identifer %>% as.numeric(),
          is.document_url = ifelse(document.source %>% str_detect('http'), T, F)
        ) %>%
        dplyr::select(id.gkg.record:id.source_collection_identifer,
                      is.document_url,
                      everything()) %>%
        dplyr::rename(date_time.url = date.url) %>%
        dplyr::mutate(date_time.url = date_time.url %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone())) %>%
        separate(
          id.gkg.record,
          into = c('date_time', 'id.date_time.article'),
          sep = '\\-',
          remove = F
        ) %>%
        dplyr::select(-date_time) %>%
        dplyr::mutate(id.date_time.article = id.date_time.article %>% as.numeric) %>%
        suppressMessages() %>%
        suppressWarnings()

    }

    if (remove_files == T) {
      "rm -R " %>%
        paste0(temp.dir) %>%
        system()
      if (empty_trash == T) {
        system('rm -rf ~/.Trash/*')
      }
    }

    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        message()

    }

    return(gdelt_data)

  }

#' Returns long or wide mentioned numerics from a GKG data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param include_char_loc
#' @param return_wide
#' @importFrom tidyr gather
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#' @importFrom purrr map
#' @importFrom purrr compact
#' @import stringr
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_numerics <- function(gdelt_data,
                                       filter_na = T,
                                       include_char_loc = T,
                                       return_wide = F) {
  parse_mentioned_numerics <-
    function(field = "170,Scotland Road,1600;170,Scotland Road,2475;",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(
              amount.value.1 = NA,
              amount.term.1 = NA,
              char.loc = NA
            )
        } else {
          field_data <-
            data_frame(
              amount.value = NA,
              amount.term = NA,
              char.loc = NA,
              id.article.numeric_item = 1
            )
        }
      } else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field = fields) %>%
          dplyr::mutate(id.article.numeric_item = 1:n()) %>%
          separate(
            field,
            into = c('amount.value', 'amount.term', 'char.loc'),
            sep = '\\,'
          ) %>%
          dplyr::mutate(amount.term = amount.term %>% str_trim) %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.numeric_item) %>%
            arrange(id.article.numeric_item) %>%
            unite(item, item, id.article.numeric_item, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(extract_numeric(.)),
                         vars =
                           field_data %>% dplyr::select(matches('amount.value|char.loc')) %>% names)
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.numeric_item,
                          amount.value,
                          amount.term,
                          char.loc) %>%
            dplyr::mutate(char.loc = char.loc %>% as.numeric,
                   amount.value = amount.value %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!'mentioned.numerics.counts' %in% names(gdelt_data)) {
    stop("Sorry missing numeric count column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(id.gkg.record, mentioned.numerics.counts)

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!mentioned.numerics.counts %>% is.na)
  }

  all_counts <-
    1:length(counts_data$mentioned.numerics.counts) %>%
    purrr::map(function(x) {
      parse_mentioned_numerics(field = counts_data$mentioned.numerics.counts[x],
                               return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  if (include_char_loc == F) {
    all_counts <-
      all_counts %>%
      dplyr::select(-char.loc)
  }

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  return(all_counts)
}

#' Returns long or wide mentioned people from a GKG data frame
#'
#' @param gdelt_data
#' @param people_column options \code{c('person', 'persons', 'persons.count', 'persons.char_loc', 'char_loc'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_people <- function(gdelt_data,
                                     people_column = 'persons',
                                     filter_na = T,
                                     return_wide = F) {

  people_count_cols <-
    c('person',
      'persons',
      'persons.count',
      'persons.char_loc',
      'char_loc')

  if (!people_column %in% people_count_cols) {
    stop("Sorry people column can only be\n" %>%
           paste0(paste0(people_count_cols, collapse = '\n')))
  }

  if (people_column %in% c('person', 'persons')) {
    people_column <-
      'persons'
  }

  if (people_column %in% c('persons.count', 'persons.char_loc', 'char_loc')) {
    people_column <-
      'persons.char_loc'
  }

  parse_mentioned_people_counts <-
    function(field = "Chaudhry Nisar Ali Khan,63",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(name.person.1 = NA, char.loc.1 = NA)
        } else {
          field_data <-
            data_frame(
              name.person = NA,
              char.loc = NA,
              id.article.person = 1
            )
        }
      } else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field = fields) %>%
          dplyr::mutate(id.article.person = 1:n()) %>%
          separate(field,
                   into = c('name.person', 'char.loc'),
                   sep = '\\,') %>%
          suppressWarnings() %>%
          suppressMessages()

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.person) %>%
            arrange(id.article.person) %>%
            unite(item, item, id.article.person, sep = '.') %>%
            suppressWarnings()

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(extract_numeric(.)),
                         vars =
                           field_data %>% dplyr::select(matches('char.loc')) %>% names)
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.person, name.person, char.loc) %>%
            dplyr::mutate(char.loc = char.loc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!people_column %in% names(gdelt_data)) {
    stop("Sorry missing people column")
  }

  col_names <-
    c('id.gkg.record', people_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'people_col'

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!people_col %>% is.na())
  }

  all_counts <-
    1:length(counts_data$people_col) %>%
    purrr::map(function(x) {
      parse_mentioned_people_counts(field = counts_data$people_col[x],
                                    return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  if (people_column == 'persons') {
    all_counts <-
      all_counts %>%
      dplyr::select(-char.loc)
  }

  return(all_counts)
}

#' Returns long or wide mentioned organizations from a GKG data frame
#'
#' @param gdelt_data
#' @param organization_column options \code{c('organization', 'organizations', 'organizations.count', 'organizations.char_loc', 'char_loc'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_organizations <- function(gdelt_data,
                                            organization_column = 'organizations',
                                            filter_na = T,
                                            return_wide = F) {

  organization_count_cols <-
    c(
      'organization',
      'organizations',
      'organizations.count',
      'organizations.char_loc',
      'char_loc'
    )

  if (!organization_column %in% organization_count_cols) {
    stop("Sorry people column can only be\n" %>%
           paste0(paste0(organization_count_cols, collapse = '\n')))
  }

  if (organization_column %in% c('organization', 'organizations')) {
    organization_column <-
      'organizations'
  }

  if (organization_column %in% c('organizations.count',
                                 'organizations.char_loc',
                                 'char_loc')) {
    organization_column <-
      'persons.char_loc'
  }

  parse_mentioned_organization_counts <-
    function(field = "Twitter,2912;Pegasystems,169;Pegasystems,1238;Pegasystems,1829;Pegasystems,2079;Pegasystems,2888;Nasdaq,193;Nasdaq,2086;Ilena Ryan Pegasystems Inc,2892;Pegasystems Inc,173;Pegasystems Inc,2892",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(name.organization.1 = NA,
                       char.loc.1 = NA)
        } else {
          field_data <-
            data_frame(
              name.organization = NA,
              char.loc = NA,
              id.article.organization = 1
            )
        }
      } else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field = fields) %>%
          dplyr::mutate(id.article.organization = 1:n()) %>%
          separate(field,
                   into = c('name.organization', 'char.loc'),
                   sep = '\\,') %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.organization) %>%
            arrange(id.article.organization) %>%
            unite(item, item, id.article.organization, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(extract_numeric(.)),
                         vars =
                           field_data %>% dplyr::select(matches('char.loc')) %>% names)
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.organization, name.organization, char.loc) %>%
            dplyr::mutate(char.loc = char.loc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!organization_column %in% names(gdelt_data)) {
    stop("Sorry missing organization column")
  }

  col_names <-
    c('id.gkg.record', organization_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'org_col'

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!org_col %>% is.na())
  }

  all_counts <-
    1:length(counts_data$org_col) %>%
    purrr::map(function(x) {
      parse_mentioned_organization_counts(field = counts_data$org_col[x],
                                          return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  if (organization_column == 'organizations') {
    all_counts <-
      all_counts %>%
      dplyr::select(-char.loc)
  }

  return(all_counts)
}

#' Returns mentioned names from a GKG data frame.
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_names <- function(gdelt_data,
                                    filter_na = T,
                                    return_wide = F) {
  parse_mentioned_names_counts <-
    function(field = "Interior Minister Chaudhry Nisar Ali Khan,47;Mullah Mansour,87;Afghan Taliban,180;Mullah Mansour,382;Mullah Mansor,753;Mullah Mansour,815;Mullah Mansour,1025",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(name.mentioned_name.1 = NA,
                       char.loc.1 = NA)
        } else {
          field_data <-
            data_frame(
              name.mentioned_name = NA,
              char.loc = NA,
              id.article.mentioned_name = 1
            )
        }
      } else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field = fields) %>%
          dplyr::mutate(id.article.mentioned_name = 1:n()) %>%
          separate(field,
                   into = c('name.mentioned_name', 'char.loc'),
                   sep = '\\,') %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.mentioned_name) %>%
            arrange(id.article.mentioned_name) %>%
            unite(item, item, id.article.mentioned_name, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(extract_numeric(.)),
                         vars =
                           field_data %>% dplyr::select(matches('char.loc')) %>% names)
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.mentioned_name,
                          name.mentioned_name,
                          char.loc) %>%
            dplyr::mutate(char.loc = char.loc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!'mentioned.names.counts' %in% names(gdelt_data)) {
    stop("Sorry missing metioned name column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(id.gkg.record, mentioned.names.counts)

  all_counts <-
    1:length(counts_data$mentioned.names.counts) %>%
    purrr::map(function(x) {
      parse_mentioned_names_counts(field = counts_data$mentioned.names.counts[x],
                                   return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())
  if (filter_na == T) {
    if ('name.mentioned_name' %in% names(all_counts)) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!name.mentioned_name %>% is.na)
    }
    if ('name.mentioned_name.1' %in% names(all_counts)) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!name.mentioned_name.1 %>% is.na)
    }
  }
  return(all_counts)
}

#' Returns mentioned themes from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param organization_column options \code{c('theme', 'themes', 'themes.count', 'themes.char_loc', 'char_loc'))}
#' @param return_wide
#' @importFrom purrr map
#'
#' @return
#' @export
#'
#' @examples

get_mentioned_gkg_themes <- function(gdelt_data,
                                     filter_na = T,
                                     theme_column = 'themes',
                                     return_wide = F) {
  theme_count_cols <-
    c('theme',
      'themes',
      'themes.count',
      'themes.char_loc',
      'char_loc')

  if (!theme_column %in% theme_count_cols) {
    stop("Sorry theme column can only be\n" %>%
           paste0(paste0(theme_count_cols, collapse = '\n')))
  }

  if (theme_column %in% c('theme', 'themes')) {
    theme_column <-
      'themes'
  }

  if (theme_column %in% c('themes.count', 'themes.char_loc', 'char_loc')) {
    theme_column <-
      'themes.char_loc'
  }

  parse_mentioned_names_themes <-
    function(field = "https://youtube.com/esctodaytv;https://youtube.com/embed/5ymFX91HwM0?wmode=transparent&#038;modestbranding=1&#038;autohide=1&#038;showinfo=0&#038;rel=0;",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(code.theme.1 = NA, char.loc.1 = NA)
        } else {
          field_data <-
            data_frame(
              code.theme = NA,
              char.loc = NA,
              id.article.gkg_theme = 1
            )
        }
      } else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field = fields) %>%
          dplyr::mutate(id.article.gkg_theme = 1:n()) %>%
          separate(field,
                   into = c('code.theme', 'char.loc'),
                   sep = '\\,') %>%
          suppressWarnings()

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.gkg_theme) %>%
            arrange(id.article.gkg_theme) %>%
            unite(item, item, id.article.gkg_theme, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(extract_numeric(.)),
                         vars =
                           field_data %>% dplyr::select(matches('char.loc')) %>% names)
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.gkg_theme, code.theme, char.loc) %>%
            dplyr::mutate(char.loc = char.loc %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!theme_column %in% names(gdelt_data)) {
    stop("Sorry missing organization column")
  }

  col_names <-
    c('id.gkg.record', theme_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)
  names(counts_data)[2] <-
    'theme_col'

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!theme_col %>% is.na())
  }

  all_counts <-
    1:length(counts_data$theme_col) %>%
    map(function(x) {
      parse_mentioned_names_themes(field = counts_data$theme_col[x],
                                   return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  if (theme_column == 'themes') {
    all_counts <-
      all_counts %>%
      dplyr::select(-char.loc)
  }
  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  return(all_counts)
}

#' Returns social embed information from a gkg data frame
#'
#' @param gdelt_data
#' @param organization_column options \code{c('url.social_media_image.embeds', 'images', 'url.social_media_video.embeds', 'video', 'videos'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples

get_mentioned_gkg_social_embeds <- function(gdelt_data,
                                  social_embed_column = 'url.social_media_image.embeds',
                                  filter_na = T,
                                  return_wide = F) {
  image_video_cols <-
    c(
      'url.social_media_image.embeds',
      'images',
      'image',
      'url.social_media_video.embeds',
      'video',
      'videos'
    )
  if (!social_embed_column %in% image_video_cols) {
    stop("Social embed column can only be\n" %>% paste0(paste0(image_video_cols, collapse = '\n')))
  }

  if (social_embed_column %in% c("image", "images")) {
    social_embed_column <-
      'url.social_media_image.embeds'
  }

  if (social_embed_column  %in% c("video", "videos")) {
    social_embed_column <-
      'url.social_media_video.embeds'
  }
  parse_embeds <-
    function(field = "http://instagram.com/p/9YfHJtMx0N;http://instagram.com/p/BFz1t7Tsx8t;http://instagram.com/p/BEZrBSKsx8U;http://instagram.com/p/BEw5_T-Mx3B;",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(url.social_media.image_embed = NA)
        } else {
          field_data <-
            data_frame(
              url.social_media.image_embed = NA,
              id.article.social_media.image_embed = 1
            )
        }
      }  else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(url.social_media.image_embed = fields) %>%
          dplyr::mutate(
            id.article.social_media.image_embed = 1:n(),
            domain.social_media.image_embed = url.social_media.image_embed %>% urltools::domain()
          )
        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.social_media.image_embed) %>%
            arrange(id.article.social_media.image_embed) %>%
            unite(item, item, id.article.social_media.image_embed, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(
              id.article.social_media.image_embed,
              domain.social_media.image_embed,
              url.social_media.image_embed
            )
        }
      }

      return(field_data)
    }


  if (!social_embed_column %in% names(gdelt_data)) {
    stop("Sorry missing source embed column")
  }

  col_names <-
    c('id.gkg.record', social_embed_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'source_col'

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!source_col %>% is.na())
  }

  all_counts <-
    1:length(counts_data$source_col) %>%
    purrr::map(function(x) {
      parse_embeds(field = counts_data$source_col[x],
                   return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  if (social_embed_column == 'url.social_media_image.embeds') {
    names(all_counts)[3] <-
      c('url.social_media_image.embed')

    names(all_counts)[1] <-
      c('id.article.social_media.image_embed')
  }

  if (social_embed_column == 'url.social_media_video.embeds') {
    names(all_counts)[3] <-
      c('url.social_media_video.embed')
    names(all_counts)[1] <-
      c('id.article.social_media.video_embed')
  }

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  return(all_counts)
}

#' Returns article tones from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_article_tone <- function(gdelt_data,
                                 filter_na = T,
                                 return_wide = F) {
  parse_article_tones <-
    function(field = "-4.65116279069767,1.55038759689922,6.2015503875969,7.75193798449612,13.1782945736434,0,134",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(amount.tone = NA)
        } else {
          field_data <-
            data_frame(amount.tone = NA, id.article.tone = 1)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\,') %>%
          flatten_chr() %>%
          .[!. %in% ''] %>%
          as.numeric()

        fields_df <-
          data_frame(amount.tone = fields) %>%
          dplyr::mutate(id.article.tone = 1:n())
        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.tone) %>%
            arrange(id.article.tone) %>%
            unite(item, item, id.article.tone, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.tone, amount.tone)
        }
      }

      return(field_data)
    }

  if (!'tone' %in% names(gdelt_data)) {
    stop("Sorry missing tone column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(id.gkg.record, tone)

  all_counts <-
    1:length(counts_data$tone) %>%
    purrr::map(function(x) {
      parse_article_tones(field = counts_data$tone[x], return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())
  if (filter_na == T) {
    all_counts <-
      all_counts %>%
      dplyr::filter(!amount.tone %>% is.na)
  }
  return(all_counts)
}

#' Returns mentioned CAMEO event count from a gkg data frame
#'
#' @param gdelt_data
#' @param count_column options \code{c('count', 'counts'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_event_counts <- function(gdelt_data,
                                           count_column = 'counts',
                                           filter_na = T,
                                           return_wide = F) {
  count_cols <-
    c('counts',
      'count',
      'counts.char_loc',
      'count.char_loc',
      'char_loc')

  if (!count_column %in% count_cols) {
    stop("Sorry count column can only be\n" %>%
           paste0(paste0(count_cols, collapse = '\n')))
  }

  if (count_column %in% c('counts', 'count')) {
    count_column <-
      'counts'
  }

  if (count_column %in% c('counts.char_loc', 'count.char_loc', 'char_loc')) {
    count_column <-
      'counts.char_loc'
  }

  parse_field_count <-
    function(field = "KIDNAP#60##4#Beirut, Beyrouth, Lebanon#LE#LE04#33.8719#35.5097#-801546;KIDNAP#2##1#Lebanon#LE#LE#33.8333#35.8333#LE;KIDNAP#4##1#Australia#AS#AS#-27#133#AS;",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(code.gkg.theme = NA)
        } else {
          field_data <-
            data_frame(code.gkg.theme = NA,
                       id.article.field = 1)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field_item = fields) %>%
          dplyr::mutate(id.article.field = 1:n()) %>%
          separate(
            col = field_item,
            sep = '\\#',
            into = c(
              'code.gkg.theme',
              'count.event',
              'entity.event',
              'id.location_type',
              'location',
              'id.country',
              'id.adm1code',
              'latitude',
              'longitude',
              'id.feature',
              'char.loc'
            )
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          dplyr::mutate_each_(funs(as.numeric),
                              vars =
                                fields_df %>% dplyr::select(matches("count|char.loc|id.location_type")) %>% dplyr::select(-id.country) %>% names) %>%
          dplyr::mutate_each_(funs(as.numeric(., digits = 5)),
                              vars =
                                fields_df %>% dplyr::select(matches("latitude|longitude")) %>% names)

        fields_df$entity.event[fields_df$entity.event == ''] <-
          NA

        fields_df$location[fields_df$location == ''] <-
          NA

        fields_df$id.country[fields_df$id.country == ''] <-
          NA

        fields_df$id.adm1code[fields_df$id.adm1code == ''] <-
          NA

        fields_df$id.feature[fields_df$id.feature == ''] <-
          NA

        fields_df <-
          fields_df %>%
          dplyr::left_join(data_frame(
            id.location_type = 1:5,
            location_type = c(
              'country',
              'us_state',
              'us_city',
              'world_city',
              'world_state'
            )
          )) %>%
          dplyr::select(code.gkg.theme:id.location_type,
                        location_type,
                        everything()) %>%
          suppressMessages()

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.field) %>%
            arrange(id.article.field) %>%
            unite(item, item, id.article.field, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(as.numeric),
                         vars =
                           field_data %>% dplyr::select(matches("count.gkg_event")) %>% names) %>%
            dplyr::mutate_each_(funs(as.numeric(., digits = 5)),
                         vars =
                           field_data %>% dplyr::select(matches("latitude|longitude")) %>% names)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.field, everything())
        }
      }

      return(field_data)
    }

  if (!count_column %in% names(gdelt_data)) {
    stop("Sorry missing count column")
  }

  col_names <-
    c('id.gkg.record', count_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'count_col'

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!count_col %>% is.na())
  }

  all_counts <-
    1:length(counts_data$count_col) %>%
    purrr::map(function(x) {
      parse_field_count(field = counts_data$count_col[x],
                        return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  if (count_column == "counts") {
    all_counts <-
      all_counts %>%
      dplyr::select(-char.loc)
  }

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  return(all_counts)
}

#' Returns mentioned locations from a gkg data frame
#'
#' @param gdelt_data
#' @param location_column options \code{c('location', 'locations', 'locations.char_loc', 'location.char_loc', 'char.loc'))}
#' @param is.char_loc
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_locations <- function(gdelt_data,
                                        location_column = 'locations',
                                        is.char_loc = F,
                                        filter_na = T,
                                        return_wide = F) {
  location_cols <-
    c('location',
      'locations',
      'locations.char_loc',
      'location.char_loc',
      'char_loc')

  if (!location_column %in% location_cols) {
    stop("Sorry location column can only be\n" %>%
           paste0(paste0(location_cols, collapse = '\n')))
  }

  if (location_column %in% c('location', 'locations')) {
    location_column <-
      'locations'
  }

  if (location_column %in% c('locations.char_loc', 'location.char_loc', 'char_loc')) {
    location_column <-
      'locations.char_loc'
  }
  parse_location_count <-
    function(field = "4#Leichhardt, New South Wales, Australia#AS#AS02#4944#-33.8833#151.15#-1583352#203;4#Daintree, Queensland, Australia#AS#AS04#40202#-16.25#145.317#-1568710#421;4#Daintree, Queensland, Australia#AS#AS04#40202#-16.25#145.317#-1568710#2224",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(location = NA)
        } else {
          field_data <-
            data_frame(location = NA, id.article.location = 1)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field_item = fields) %>%
          dplyr::mutate(id.article.location = 1:n())


        if (is.char_loc == T) {
          fields_df <-
            fields_df %>%
            separate(
              col = field_item,
              sep = '\\#',
              into = c(
                'id.location_type',
                'location',
                'id.country',
                'id.adm1code',
                'id.adm2code',
                'latitude',
                'longitude',
                'id.feature',
                'char.loc'
              )
            ) %>%
            suppressMessages() %>%
            suppressWarnings()

        } else {
          fields_df <-
            fields_df %>%
            separate(
              col = field_item,
              sep = '\\#',
              into = c(
                'id.location_type',
                'location',
                'id.country',
                'id.adm1code',
                'latitude',
                'longitude',
                'id.feature'
              )
            ) %>%
            suppressMessages() %>%
            suppressWarnings()
        }

        fields_df <-
          fields_df %>%
          dplyr::mutate_each_(funs(as.numeric),
                       vars =
                         fields_df %>% dplyr::select(matches("id.location_type|char.loc")) %>% names) %>%
          dplyr::mutate_each_(funs(as.numeric(., digits = 5)),
                       vars =
                         fields_df %>% dplyr::select(matches("latitude|longitude")) %>% names) %>%
          dplyr::left_join(data_frame(
            id.location_type = 1:5,
            location_type = c(
              'country',
              'us_state',
              'us_city',
              'world_city',
              'world_state'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::select(id.location_type, location_type, everything()) %>%
          suppressWarnings()


        fields_df$location[fields_df$location == ''] <-
          NA

        fields_df$id.country[fields_df$id.country == ''] <-
          NA

        fields_df$id.adm1code[fields_df$id.adm1code == ''] <-
          NA

        fields_df$id.feature[fields_df$id.feature == ''] <-
          NA

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.location) %>%
            arrange(id.article.location) %>%
            unite(item, item, id.article.location, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(as.numeric),
                         vars =
                           field_data %>% dplyr::select(matches("id.location_type")) %>% names) %>%
            dplyr::mutate_each_(funs(as.numeric(., digits = 5)),
                         vars =
                           field_data %>% dplyr::select(matches("latitude|longitude")) %>% names)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.location, everything())
        }
      }

      return(field_data)
    }

  if (!location_column %in% names(gdelt_data)) {
    stop("Sorry missing location column")
  }

  col_names <-
    c('id.gkg.record', location_column)

  counts_data <-
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'loc_col'

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!loc_col %>% is.na())
  }

  all_counts <-
    1:length(counts_data$loc_col) %>%
    purrr::map(function(x) {
      parse_location_count(field = counts_data$loc_col[x],
                           return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  if (filter_na == T) {
    all_counts <-
      all_counts %>%
      dplyr::filter(!location %>% is.na())
  }
  return(all_counts)
}

#' Returns mentioned dates from gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_dates <- function(gdelt_data,
                                    filter_na = T,
                                    return_wide = F) {
  parse_dates <-
    function(field = "4#6#16#0#734;4#4#26#0#2258",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(id.date_resolution = NA)
        } else {
          field_data <-
            data_frame(id.date_resolution = NA,
                       id.date.article = NA)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(field_item = fields) %>%
          dplyr::mutate(id.date.article = 1:n()) %>%
          separate(
            col = field_item,
            sep = '\\#',
            into = c('id.date_resolution', 'month', 'day', 'year', 'char.loc')
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          dplyr::mutate_each_(funs(as.numeric),
                       vars =
                         fields_df  %>% names) %>%
          dplyr::left_join(data_frame(
            id.date_resolution = 1:4,
            date_resolution = c(
              'ex_mon_date',
              'year_only',
              'month_date' ,
              'fully_resolved'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::select(id.date_resolution, date_resolution, everything())

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.date.article) %>%
            arrange(id.date.article) %>%
            unite(item, item, id.date.article, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(as.numeric),
                         vars =
                           field_data %>% dplyr::select(
                             matches("id.date_resolution|month|day|year|char.loc")
                           ) %>% names)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.date.article, everything())
        }
      }

      return(field_data)
    }

  if (!'dates' %in% names(gdelt_data)) {
    stop("Sorry missing date column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(id.gkg.record, dates)

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!dates %>% is.na)
  }

  all_counts <-
    1:length(counts_data$dates) %>%
    purrr::map(function(x) {
      parse_dates(field = counts_data$dates[x], return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())


  return(all_counts)
}

#' Returns mentioned quotes from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_quotes <- function(gdelt_data,
                                     filter_na = T,
                                     return_wide = F) {
  parse_quotes <-
    function(field = "495|51||knowingly aided and abetted an international kidnap#865|50||nothing less than an international child abduction#2764|49|| staff member should be singled out for dismissal#3373|48||make any serious attempt to independently verify#4059|46||wants to go through every single little detail#4802|156||And xC2 ; xA0 ; you're keeping all of xC2 ; xA0 ; them xC2 ; xA0 ; - xC2 ; xA0 ; except one sacrificial lamb - xC2 ; xA0 ; to run the show?#4879|28||How do you think that looks?#6093|60||an extraordinary conspiracy to remove the children illegally#6150|50||nothing less than an international child abduction#6828|408||I xE2 ; x80 ; xA6 ; have found nothing that supports a finding that any Australian Government official somehow knowingly assisted the mother to do something that was wrong xE2 ; x80 ; xA6 ; I do not find xE2 ; x80 ; xA6 ; that any Australian Embassy officials who helped the mother did so knowing that the mother did not have the father consent to remove the girls permanently from Italy",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(id.article.quote = NA)
        } else {
          field_data <-
            data_frame(quote = NA,
                       id.article.quote = NA)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\#') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          data_frame(quote_items = fields) %>%
          dplyr::mutate(id.article.quote = 1:n()) %>%
          separate(
            col = quote_items,
            sep = '\\|',
            into = c('char.loc', 'length.quote', 'verb.intro', 'quote')
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          dplyr::mutate(quote = quote %>% str_trim) %>%
          dplyr::mutate_each_(funs(as.numeric),
                       vars =
                         c('char.loc', 'length.quote')) %>%
          dplyr::select(id.article.quote, everything())

        fields_df$verb.intro[fields_df$verb.intro == ''] <-
          NA

        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -id.article.quote) %>%
            arrange(id.article.quote) %>%
            unite(item, item, id.article.quote, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(as.numeric),
                         vars =
                           field_data %>% dplyr::select(matches("char.loc|length.quote")) %>% names)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.quote, everything())
        }
      }

      return(field_data)
    }

  if (!'quotations' %in% names(gdelt_data)) {
    stop("Sorry missing quotations column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(id.gkg.record, quotations)

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!quotations %>% is.na())
  }

  all_counts <-
    1:length(counts_data$quotations) %>%
    purrr::map(function(x) {
      parse_quotes(field = counts_data$quotations[x], return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows

  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  return(all_counts)
}

#' Returns GCAM codes from a gkg data frame
#'
#' @param gdelt_data
#' @param merge_gcam_codes
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_gcams <- function(gdelt_data,
                                   merge_gcam_codes = F,
                                   filter_na = T,
                                   return_wide = F) {
  parse_gcam_data <-
    function(field = "wc:284,c12.1:5",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
        if (return_wide == T) {
          field_data <-
            data_frame(id.gcam = NA)
        } else {
          field_data <-
            data_frame(id.gcam = NA,
                       id.article.gcam = 1)
        }
      }  else {
        fields <-
          field %>%
          str_split('\\,') %>%
          flatten_chr() %>%
          .[!. %in% '']

        article.word_count <-
          fields[1] %>%
          extract_numeric(.)()

        fields_df <-
          data_frame(article.word_count,
                     id.gcam.score = fields[2:length(fields)]) %>%
          separate(id.gcam.score,
                   into = c('id.gcam', 'score_words'),
                   sep = '\\:') %>%
          dplyr::mutate(
            id.article.gcam = 1:n(),
            score_words = score_words %>% as.numeric(., digits = 4)
          )


        if (return_wide == T) {
          fields_df <-
            fields_df %>%
            gather(item, value, -c(article.word_count, id.article.gcam)) %>%
            arrange(id.article.gcam) %>%
            unite(item, item, id.article.gcam, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_each_(funs(as.numeric),
                         vars =
                           field_data %>% dplyr::select(matches("score_words")) %>% names)

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(id.article.gcam, everything())
        }
      }

      return(field_data)
    }

  if (!'gcam' %in% names(gdelt_data)) {
    stop("Sorry missing video embed column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(id.gkg.record, gcam)

  if (filter_na == T) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!gcam %>% is.na())
  }

  all_counts <-
    1:length(counts_data$gcam) %>%
    purrr::map(function(x) {
      parse_gcam_data(field = counts_data$gcam[x],
                      return_wide = return_wide) %>%
        dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
    }) %>%
    compact %>%
    bind_rows
  if (merge_gcam_codes == T) {
    all_counts <-
      all_counts %>%
      dplyr::left_join(
        get_codes_gcam() %>%
          dplyr::select(id.gcam, type, dictonary.human_name,
                        dimension.human_name)
      )
  }
  all_counts <-
    all_counts %>%
    dplyr::select(id.gkg.record, everything())

  return(all_counts)
}

#' Returns source name or source url from a gkg data frame
#'
#' @param gdelt_data
#' @param source_column options \code{c('sources', 'source', 'sources.url', 'source.url'))}
#' @param filter_na
#' @param return_wide
#' @importFrom urltools domain
#'
#' @return
#' @export
#'
#' @examples
get_mentioned_gkg_source_data <-
  function(gdelt_data,
           source_column = 'sources',
           filter_na = T,
           return_wide = F) {
    source_options <-
      c('sources', 'source', 'url.sources', 'url.source')
    if (!source_column %in% source_options) {
      stop("Sorry source column can only be\n" %>%
             paste0(paste0(source_options, collapse = '\n')))
    }

    if (source_column %in% c('sources', 'source')) {
      source_column <-
        'sources'
    }

    if (source_column %in% c('url.sources', 'url.source')) {
      source_column <-
        'url.sources'
    }
    parse_source_name <-
      function(field = "businesstimes.com.sg;businesstimes.com.sg",
               return_wide = F) {
        options(scipen = 99999)
        if (field %>% is.na) {
          if (return_wide == T) {
            field_data <-
              data_frame(name.source = NA)
          } else {
            field_data <-
              data_frame(name.source = NA,
                         id.article.source.1 = 1)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% ''] %>%
            unique

          fields_df <-
            data_frame(name.source = fields) %>%
            dplyr::mutate(id.article.source = 1:n())
          if (return_wide == T) {
            fields_df <-
              fields_df %>%
              gather(item, value, -id.article.source) %>%
              arrange(id.article.source) %>%
              unite(item, item, id.article.source, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
              dplyr::select(id.article.source,
                            everything())
          }
        }

        return(field_data)
      }

    if (!source_column %in% names(gdelt_data)) {
      stop("Sorry missing source column")
    }

    col_names <-
      c('id.gkg.record', source_column)

    counts_data <-
      gdelt_data %>%
      dplyr::select_(.dots = col_names)

    names(counts_data)[2] <-
      'source_col'

    if (filter_na == T) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!source_col %>% is.na())
    }

    all_counts <-
      1:length(counts_data$source_col) %>%
      purrr::map(function(x) {
        parse_source_name(field = counts_data$source_col[x],
                          return_wide = return_wide) %>%
          dplyr::mutate(id.gkg.record = counts_data$id.gkg.record[x])
      }) %>%
      compact %>%
      bind_rows

    if (source_column == 'url.sources') {
      names(all_counts)[2] <-
        c('url.source')
    }

    if (source_column == 'sources') {
      names(all_counts)[2] <-
        c('name.source')
    }

    all_counts <-
      all_counts %>%
      dplyr::select(id.gkg.record, everything())

    return(all_counts)
  }


#' Retrieves detailed GKG data for a given day from a specified table
#'
#' @param date_data must be a date in Year - Month - Day format
#' @param table_name options \code{c('gkg', 'export', 'mentions'))}
#' @param file_directory
#' @param empty_trash
#' @param return_message
#' @return
#'
#' @examples

get_data_gkg_day_detailed <- function(date_data = "2016-06-01",
                                      table_name = "gkg",
                                      file_directory = 'Desktop/temp_gdelt_data',
                                      remove_files = T,
                                      empty_trash = T,
                                      return_message = T) {
  if (!date_data %>% substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }
  tables <-
    c('gkg', 'export', 'mentions')
  if (!table_name %in% tables) {
    stop("Sorry tables can only be:\n" %>% paste0(paste0(tables, collapse = '\n')))
  }

  date_data <-
    date_data %>%
    ymd %>% as.Date()


  if (date_data < "2015-02-18") {
    stop("Sorry data starts on February 18th, 2015")
  }

  if (date_data > Sys.Date()) {
    stop("Sorry data can't go into the future")
  }

  if (!'gdelt_detailed_logs' %>% exists) {
    paste("To save memory and time next time you should run the function get_urls_gkg_15_minute_log and save to data frame called gdelt_detailed_logs") %>%
      message
    gdelt_detailed_logs <-
      get_urls_gkg_15_minute_log()
  }

  urls <-
    gdelt_detailed_logs %>%
    dplyr::filter(date.data == date_data) %>%
    dplyr::filter(name.file == table_name) %>%
    .$url.data

  get_gdelt_url_data_safe <-
    failwith(NULL, get_gdelt_url_data)

  all_data <-
    urls %>%
    map(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        return_message = return_message,
        empty_trash = empty_trash
      )
    }) %>%
    compact %>%
    bind_rows %>%
    distinct %>%
    suppressMessages() %>%
    suppressWarnings()

  if (return_message == T) {
    "You retrieved " %>%
      paste0(all_data %>% nrow, " gkg detailed events for ", date_data) %>%
      message()
  }

  return(all_data)
}


#' Get dates detailed data from a specified table
#'
#' @param dates
#' @param table_name
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_data_gkg_days_detailed <- function(dates = c("2016-06-01"),
                                       table_name = "gkg",
                                       file_directory = 'Desktop/temp_gdelt_data',
                                       remove_files = T,
                                       empty_trash = T,
                                       return_message = T) {
  get_data_gkg_day_detailed_safe <-
    failwith(NULL, get_data_gkg_day_detailed)

  all_data <-
    dates %>%
    map(
      function(x)
        get_data_gkg_day_detailed_safe(
          date_data = x,
          table_name = table_name,
          file_directory = file_directory,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        )
    ) %>%
    compact %>%
    bind_rows

  return(all_data)
}

#' Retrieves gkg summary file for a given day
#'
#' @param date_data
#' @param file_directory
#' @param is_count_file options \code{c(TRUE, FALSE)}
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#'
#' @examples

get_data_gkg_day_summary <- function(date_data = "2016-06-01",
                                     file_directory = 'Desktop/temp_gdelt_data',
                                     is_count_file = F,
                                     remove_files = T,
                                     empty_trash = T,
                                     return_message = T) {
  options(scipen = 99999)
  if (!date_data %>% substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data %>%
    ymd %>% as.Date()


  if (date_data < "2013-04-01") {
    stop("Sorry data starts on April 1st, 2013")
  }

  if (date_data > Sys.Date()) {
    stop("Sorry data can't go into the future")
  }
  if (!'summary_data_urls' %>% exists) {
    paste0("To save time and memory next time you should run the function get_urls_gkg_daily_summaries and save it to a data frame called summary_data_urls") %>% message
    summary_data_urls <-
      get_urls_gkg_daily_summaries(return_message = return_message)
  }

  if (is_count_file == T) {
    summary_data_urls <-
      summary_data_urls %>%
      dplyr::filter(is.gkg_count_file == T)
  } else {
    summary_data_urls <-
      summary_data_urls %>%
      dplyr::filter(is.gkg_count_file == F)
  }

  urls <-
    summary_data_urls %>%
    dplyr::filter(date.data == date_data) %>%
    .$url.data

  get_gdelt_url_data_safe <-
    failwith(NULL, get_gdelt_url_data)

  all_data <-
    urls %>%
    map(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        return_message = return_message,
        empty_trash = empty_trash
      )
    }) %>%
    compact %>%
    bind_rows %>%
    distinct

  all_data <-
    all_data %>%
    dplyr::mutate(count.object = count.object %>% as.numeric(),
           id.cameo_events = id.cameo_events %>% as.character())

  if (return_message == T) {
    "You retrieved " %>%
      paste0(all_data %>% nrow, " gkg summary events for ", date_data) %>%
      message()
  }

  return(all_data)
}

#' Gets days summary GDELT GKG data by table
#'
#' @param dates
#' @param table_name
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' get_data_gkg_days_summary(dates = c("2016-06-01"), is_count_file = F)

get_data_gkg_days_summary <- function(dates = c("2016-06-01"),
                                      is_count_file = F,
                                      file_directory = 'Desktop/temp_gdelt_data',
                                      remove_files = T,
                                      empty_trash = T,
                                      return_message = T) {
  get_data_gkg_day_summary_safe <-
    failwith(NULL, get_data_gkg_day_summary)

  all_data <-
    dates %>%
    map(
      function(x)
        get_data_gkg_day_summary_safe(
          date_data = x,
          is_count_file = is_count_file,
          file_directory = file_directory,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        )
    ) %>%
    compact %>%
    bind_rows

  return(all_data)
}

#' Retreive GDELT data for a given period
#'
#' @param period
#' @param file_directory
#' @param is_count_file
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#'
#' @examples
get_data_gdelt_period_event <- function(period = 1983,
                                        file_directory = 'Desktop/temp_gdelt_data',
                                        is_count_file = F,
                                        remove_files = T,
                                        empty_trash = T,
                                        return_message = T) {
  period <-
    period %>%
    as.character()

  if (!'gdelt_event_urls' %>% exists) {
    paste0("To save memory and time you want to run the function get_urls_gdelt_event_log into a data_frame called gdelt_event_urls") %>%
      message()
    gdelt_event_urls <-
      get_urls_gdelt_event_log(return_message = return_message)
  }
  periods <-
    gdelt_event_urls$period.data
  if (!period %in% periods) {
    gdelt_event_message <-
      "Period can only be a 4 digit year between 1979 and 2005\nEXAMPLE: 1983\nA 6 digit year and month from 2006 to March 2013\nEXAMPLE: 201208\nOr an 8 digit year, month, day from March 1, 2013 until today\nEXAMPLE: 20140303"

    stop(gdelt_event_message)
  }

  urls <-
    gdelt_event_urls %>%
    dplyr::filter(period.data == period) %>%
    .$url.data

  get_gdelt_url_data_safe <-
    failwith(NULL, get_gdelt_url_data)

  all_data <-
    urls %>%
    map(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        return_message = return_message,
        empty_trash = empty_trash
      )
    }) %>%
    compact %>%
    bind_rows %>%
    distinct

  if (return_message == T) {
    "You retrieved " %>%
      paste0(all_data %>% nrow, " GDELT events for the period of ", period) %>%
      message()
  }

  return(all_data)
}


#' Returns GDELT event data for a given periods
#'
#' @param periods
#' @param file_directory
#' @param is_count_file
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' get_data_gdelt_periods_event (periods = c(1983))
get_data_gdelt_periods_event <- function(periods = c(1983, 1984),
                                        file_directory = 'Desktop/temp_gdelt_data',
                                        is_count_file = F,
                                        remove_files = T,
                                        empty_trash = T,
                                        return_message = T) {
  get_data_gdelt_period_event_safe <-
    failwith(NULL, get_data_gdelt_period_event)
  all_data <-
    1:length(periods) %>%
    map(
      function(x)
        get_data_gdelt_period_event_safe(
          period = periods[x],
          file_directory = file_directory,
          is_count_file = is_count_file,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        )
    ) %>%
    compact %>%
    bind_rows

  return(all_data)
}