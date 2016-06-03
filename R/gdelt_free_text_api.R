#' Loades Packages
#'
#' @param required_packages
#'
#' @return
#'
#' @examples
load_needed_packages <- function(required_packages = c('dplyr')) {
  loaded_packages <- gsub("package:", "", search())
  package_to_load <- required_packages[!required_packages %in%
                                         loaded_packages]
  if (length(package_to_load) > 0) {
    lapply(package_to_load, library, character.only = T)
  }
}


#' Parses Source
#'
#' @param source
#'
#' @return
#'
#' @examples
parse_source <- function(source = "netsdaily.com - writedate('06/02/2016 12:00 UTC'); (English / United States)") {
  load_needed_packages(c('magrittr', 'tidyr', 'readr', 'dplyr', 'stringr', 'lubridate'))
  source_df <-
    data_frame(source) %>%
    tidyr::separate(source,
                    sep = '\\ - ',
                    into = c('source', 'date.language')) %>%
    tidyr::separate(date.language,
                    sep = '\\;',
                    into = c('date', 'language')) %>%
    mutate(
      language = language %>% str_replace('\\(', '') %>% str_replace('\\)', '')  %>% str_replace('\\ /', ','),
      date = date %>% gsub('\\writedate', '', .) %>% str_replace_all('\\(', '') %>% str_replace_all('\\)', '') %>%
        str_replace_all("\\'", '')
    ) %>%
    mutate(date_time = date %>% mdy_hm %>% with_tz(Sys.timezone())) %>%
    mutate(date = date_time %>% as.Date) %>%
    tidyr::separate(language,
                    into = c('language', 'country'),
                    sep = '\\, ') %>%
    mutate_each_(funs(str_trim),
                 vars = c('source', 'language', 'country')) %>%
    suppressWarnings()

  return(source_df)
}

#' Returns data frame for given term from GDELT free text API
#'
#' @param term any word, can be quoted or not
#' @param domain \code{c(NA,'domain_name')}
#' @param return_image_url
#' @param last_minutes
#' @param max_rows
#' @param tone_less_than
#' @param tone_more_than
#' @param search_language
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param only_english
#' @param return_message
#'
#' @return
#'
#' @examples
#' get_data_ft_api_term(term = '"Brooklyn Nets"')

get_data_ft_api_term <-
  function(term = '"Brooklyn Nets"',
           domain = NA,
           return_image_url = T,
           last_minutes = NA,
           max_rows = 1000,
           tone_less_than = NA,
           tone_more_than = NA,
           search_language = 'English',
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           only_english = T,
           return_message = T) {
    load_needed_packages(
      c(
        'urltools',
        'rvest',
        'httr',
        'jsonlite',
        'tidyr',
        'stringr',
        'dplyr',
        'magrittr',
        'purrr',
        'lubridate'
      )
    )
    url_base <-
      'http://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

    if (term %>% is.na) {
      term_slug <-
        ''
      term_word <-
        'all words'
    } else {
      term_slug <-
        term %>%
        str_to_lower() %>%
        urltools::url_encode()
      term_word <-
        term
    }

    if (term %>% is.na() & !domain %>% is.na) {
      term_word <-
        domain
    }

    if (!domain %>% is.na()) {
      domain_slug <-
        '%20domain:' %>%
        paste0(domain)
    } else {
      domain_slug <-
        ''
    }


    if (!search_language %>% is.na()) {
      search_lang_slug <-
        '&searchlang:' %>%
        paste0(search_language %>% str_to_lower())
    } else {
      search_lang_slug <-
        ''
    }

    if (!source_language %>% is.na()) {
      source_lang_slug <-
        '&sourcelang:' %>%
        paste0(source_language %>% str_to_lower())
    } else {
      source_lang_slug <-
        ''
    }

    if (!last_minutes %>% is.na()) {
      last_minute_slug <-
        '%20lastminutes:' %>%
        paste0(last_minutes)
    } else {
      last_minute_slug <-
        ''
    }

    if (!tone_more_than %>% is.na()) {
      if (tone_more_than >= 100) {
        stop("Tone can't be over 100")
      }
      tone_more_slug <-
        '%20tonemorethan:' %>%
        paste0(tone_more_than)
    } else {
      tone_more_slug <-
        ''
    }

    if (!tone_less_than %>% is.na()) {
      if (tone_less_than >= 100) {
        stop("Tone can't be under 100")
      }
      tone_less_slug <-
        '%20tonelessthan:' %>%
        paste0(tone_less_than)
    } else {
      tone_less_slug <-
        ''
    }

    term_slug <-
      term_slug %>%
      paste0(
        domain_slug,
        last_minute_slug,
        tone_more_slug,
        tone_less_slug,
        search_lang_slug,
        source_lang_slug
      )

    sort_df <-
      data_frame(
        sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
        sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
      )

    if (sort_by %in% sort_df$sort_term == F) {
      stop("Sorry sort terms can only be\n" %>%
             paste0(paste0(sort_df$sort_term, collapse = '\n')))
    }

    slug_sort <-
      sort_df %>%
      dplyr::filter(sort_term == sort_by) %>%
      .$sort_slug

    slug_sort <-
      '&sortby:' %>%
      paste0(slug_sort)

    if (!max_rows %>% is.na) {
      max_row_slug <-
        '&maxrows=' %>%
        paste0(max_rows)

    } else {
      max_row_slug <-
        ''
    }

    if (dedeup_results == T) {
      dup_slug <-
        '&dropdup=true'
    } else {
      dup_slug <-
        ''
    }

    if (return_image_url == T) {
      image_slug <-
        '&output=artimglist'
    } else {
      image_slug <-
        '&output=artlist'
    }

    url <-
      url_base %>%
      paste0(term_slug,
             image_slug,
             dup_slug,
             last_minute_slug,
             slug_sort,
             max_row_slug)

    page.has.content <-
      url %>%
      GET

    page_size_df <-
      page.has.content$headers  %>%
      flatten_df %>%
      mutate(`content-length` = `content-length` %>% as.numeric)

    if (page_size_df$`content-length` <= 41) {
      stop("This search has no data")
    }

    page <-
      url %>%
      read_html

    url.text <-
      page %>%
      html_nodes(xpath = '//b') %>%
      html_text %>%
      str_trim

    url.source <-
      page %>%
      html_nodes(xpath = '//a') %>%
      html_attr('href') %>%
      .[c(T, F)]

    sources <-
      page %>%
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "sourceinfo", " " ))]') %>%
      html_text

    url_df <-
      data_frame(
        term,
        url.text,
        url.article = url.source,
        date.data = Sys.time(),
        url.search = url
      ) %>%
      bind_cols(sources %>%
                  parse_source()) %>%
      suppressWarnings()

    if (return_image_url == T) {
      url.thumbnail <-
        page %>%
        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "thumbimg", " " ))]') %>%
        html_attr('src')

      url.thumbnail[url.thumbnail == ''] <-
        NA

      url_df %<>%
        mutate(url.thumbnail)

    }

    if (!domain %>% is.na) {
      url_df %<>%
        mutate(domain = domain) %>%
        dplyr::select(term, domain, everything())
    }

    if (!tone_more_than %>% is.na) {
      url_df %<>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na) {
      url_df %<>%
        mutate(tone_less_than)
    }

    if (only_english == T) {
      url_df <-
        url_df %>%
        dplyr::filter(language == 'English')
    }

    if (return_message == T) {
      "You got " %>%
        paste0(url_df %>% nrow, ' urls for ', term_word, ' at ', Sys.time()) %>%
        message()
    }

    return(url_df)

  }

#' Returns GDELT free text API results for multiple terms
#'
#' @param terms
#' @param domain
#' @param return_image_url
#' @param last_minutes
#' @param max_rows
#' @param search_language
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param only_english
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' get_data_ft_api_terms(terms = c('"Kevin Durant"','"Stephen Curry"'), only_english = T)
get_data_ft_api_terms <-
  function(terms = c('"Brooklyn Nets"', '"New York Knicks"'),
           domain = NA,
           return_image_url = T,
           last_minutes = NA,
           max_rows = 1000,
           search_language = 'English',
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           only_english = F,
           return_message = T) {
    load_needed_packages(c('dplyr', 'purrr'))
    get_data_ft_api_term_safe <-
      failwith(NULL, get_data_ft_api_term)
    all_data <-
      1:length(terms) %>%
      map(
        function(x)
          get_data_ft_api_term_safe(
            term = terms[x],
            return_image_url = return_image_url,
            last_minutes = last_minutes,
            max_rows = max_rows,
            search_language = search_language,
            source_language = source_language,
            sort_by = sort_by,
            only_english = T,
            dedeup_results = dedeup_results
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }

#' Returns GDELT free text API results for multiple webdomains
#'
#' @param term
#' @param domains
#' @param return_image_url
#' @param last_minutes
#' @param max_rows
#' @param search_language
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param only_english
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' get_data_ft_api_domains(domains = c('realdeal.com', 'pehub.com', 'sbnation.com', 'wsj.com', 'seekingalpha.com')) %>% View
get_data_ft_api_domains <- function(domains = c('washingtonpost.com', 'nytimes.com'),
                                    term = NA,
                                    return_image_url = T,
                                    last_minutes = NA,
                                    max_rows = 1000,
                                    search_language = 'English',
                                    source_language = 'English',
                                    sort_by = 'date',
                                    dedeup_results = T,
                                    only_english = F,
                                    return_message = T) {
  load_needed_packages(c('dplyr', 'purrr'))
  get_data_ft_api_term_safe <-
    failwith(NULL, get_data_ft_api_term)

  all_data <-
    1:length(domains) %>%
    map(
      function(x)
        get_data_ft_api_term_safe(
          term = term,
          domain = domains[x],
          return_image_url = return_image_url,
          last_minutes = last_minutes,
          max_rows = max_rows,
          only_english = only_english,
          search_language = search_language,
          source_language = source_language,
          sort_by = sort_by,
          dedeup_results = dedeup_results
        )
    ) %>%
    compact %>%
    bind_rows

  if (term %>% is.na()) {
    all_data <-
      all_data %>%
      dplyr::select(-term)
  }

  return(all_data)

}

#' Retrives word cloud data from GDELT free text API for a given term, domain or term and domain
#'
#' @param term
#' @param domain
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#'
#' @examples
#' get_data_wordcloud_ft_api(term = NA, domain = 'wsj.com', tone_more_than = 5)
#' get_data_wordcloud_ft_api(term = '"Brooklyn Nets"')
get_data_wordcloud_ft_api <-
  function(term = '"Brooklyn Nets"',
           domain = NA,
           last_minutes = NA,
           search_language = 'English',
           tone_more_than = NA,
           tone_less_than = NA,
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           return_message = T) {
    load_needed_packages(
      c(
        'urltools',
        'rvest',
        'readr',
        'jsonlite',
        'stringr',
        'dplyr',
        'magrittr',
        'purrr',
        'lubridate',
        'httr'
      )
    )
    url_base <-
      'http://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

    if (term %>% is.na) {
      term_slug <-
        ''
      term_word <-
        'all words'
    } else {
      term_slug <-
        term %>%
        str_to_lower() %>%
        urltools::url_encode()
      term_word <-
        term
    }

    if (term %>% is.na() & !domain %>% is.na) {
      term_word <-
        domain
    }

    if (!domain %>% is.na()) {
      domain_slug <-
        '%20domain:' %>%
        paste0(domain)
    } else {
      domain_slug <-
        ''
    }

    if (!search_language %>% is.na()) {
      search_lang_slug <-
        '&searchlang:' %>%
        paste0(search_language %>% str_to_lower())
    } else {
      search_lang_slug <-
        ''
    }

    if (!source_language %>% is.na()) {
      source_lang_slug <-
        '&sourcelang:' %>%
        paste0(source_language %>% str_to_lower())
    } else {
      source_lang_slug <-
        ''
    }

    if (!last_minutes %>% is.na()) {
      last_minute_slug <-
        '%20lastminutes:' %>%
        paste0(last_minutes)
    } else {
      last_minute_slug <-
        ''
    }

    if (!tone_more_than %>% is.na()) {
      if (tone_more_than >= 100) {
        stop("Tone can't be over 100")
      }
      tone_more_slug <-
        '%20tonemorethan:' %>%
        paste0(tone_more_than)
    } else {
      tone_more_slug <-
        ''
    }

    if (!tone_less_than %>% is.na()) {
      if (tone_less_than >= 100) {
        stop("Tone can't be under 100")
      }
      tone_less_slug <-
        '%20tonelessthan:' %>%
        paste0(tone_less_than)
    } else {
      tone_less_slug <-
        ''
    }

    term_slug <-
      term_slug %>%
      paste0(
        domain_slug,
        last_minute_slug,
        tone_more_slug,
        tone_less_slug,
        search_lang_slug,
        source_lang_slug
      )

    sort_df <-
      data_frame(
        sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
        sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
      )

    if (sort_by %in% sort_df$sort_term == F) {
      stop("Sorry sort terms can only be\n" %>%
             paste0(paste0(sort_df$sort_term, collapse = '\n')))
    }

    slug_sort <-
      sort_df %>%
      dplyr::filter(sort_term == sort_by) %>%
      .$sort_slug

    slug_sort <-
      '&sortby:' %>%
      paste0(slug_sort)

    if (dedeup_results == T) {
      dup_slug <-
        '&dropdup=true'
    } else {
      dup_slug <-
        ''
    }
    output_slug <-
      '&output=wordcloudcsv'

    url <-
      url_base %>%
      paste0(term_slug,
             dup_slug,
             slug_sort,
             output_slug)

    page.has.content <-
      url %>%
      GET

    page_size_df <-
      page.has.content$headers  %>%
      flatten_df %>%
      mutate(`content-length` = `content-length` %>% as.numeric)

    if (page_size_df$`content-length` <= 41) {
      stop("This search has no data")
    }

    wordcloud_data <-
      url %>%
      read_csv() %>%
      mutate(term, url, date.date = Sys.time()) %>%
      dplyr::select(term, everything())

  names(wordcloud_data)[2:3] <-
      c('word', 'articles')

    wordcloud_data <-
      wordcloud_data %>%
      tidyr::separate(articles,
                      into = c('count.articles', 'size'),
                      sep = '\\(') %>%
      mutate(count.articles = count.articles %>% extract_numeric(),
             size = size %>% extract_numeric)

    if (!domain %>% is.na) {
      wordcloud_data %<>%
        mutate(domain = domain) %>%
        dplyr::select(term, domain, everything())
    }

    if (term %>% is.na()) {
      wordcloud_data <-
        wordcloud_data %>%
        dplyr::select(-term)
    }

    if (!tone_more_than %>% is.na) {
      wordcloud_data %<>%
        mutate(tone_more_than)
    }

    if (!tone_less_than %>% is.na) {
      wordcloud_data %<>%
        mutate(tone_less_than)
    }

    if (return_message == T) {
      "You got " %>%
        paste0(wordcloud_data %>% nrow,
               ' words for ',
               term_word,
               ' at ',
               Sys.time()) %>%
        message()
    }

    return(wordcloud_data)

  }

#' Returns GDELT free text API word clouds for a given domain, can be term restricted
#'
#' @param domains
#' @param term options \code{c(NA, "term_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples

get_data_wordcloud_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
           term = NA,
           last_minutes = NA,
           search_language = 'English',
           tone_more_than = NA,
           tone_less_than = NA,
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           return_message = T) {
    load_needed_packages(c('dplyr', 'purrr'))
    get_data_wordcloud_ft_api_safe <-
      failwith(NULL, get_data_wordcloud_ft_api)

    all_data <-
      1:length(domains) %>%
      map(
        function(x)
          get_data_wordcloud_ft_api(
            term = term,
            domain = domains[x],
            last_minutes = last_minutes,
            search_language = search_language,
            tone_more_than = tone_more_than,
            tone_less_than = tone_less_than,
            source_language = source_language,
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }

#' Returns GDELT free text API word clouds for a given term, can be domain restricted
#'
#' @param terms
#' @param domain  \code{c(NA, "domain_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_data_wordcloud_ft_api_terms <-
  function(terms = c('"Donald Trump"', '"Hilary Clinton"'),
           domain = NA,
           last_minutes = NA,
           search_language = 'English',
           tone_more_than = NA,
           tone_less_than = NA,
           source_language = 'English',
           sort_by = 'date',
           dedeup_results = T,
           return_message = T) {
    load_needed_packages(c('dplyr', 'purrr'))
    get_data_wordcloud_ft_api_safe <-
      failwith(NULL, get_data_wordcloud_ft_api)

    all_data <-
      1:length(terms) %>%
      map(
        function(x)
          get_data_wordcloud_ft_api(
            term = terms[x],
            domain = domain,
            last_minutes = last_minutes,
            search_language = search_language,
            tone_more_than = tone_more_than,
            tone_less_than = tone_less_than,
            source_language = source_language,
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }

#' Returns GDELT free text API sentiment DF for a domain or a term
#'
#' @param term
#' @param last_minutes
#' @param is_tone
#' @param domain
#' @param tone_less_than
#' @param tone_more_than
#' @param search_language
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#'
#' @examples
get_data_sentiment_ft_api <- function(term = 'Clinton',
                                                 domain = NA,
                                                 last_minutes = NA,
                                                 is_tone = T,
                                                 tone_less_than = NA,
                                                 tone_more_than = NA,
                                                 search_language = NA,
                                                 source_language = NA,
                                                 sort_by = 'date',
                                                 dedeup_results = T,
                                                 return_message = T) {
  load_needed_packages(
    c(
      'urltools',
      'rvest',
      'httr',
      'jsonlite',
      'stringr',
      'dplyr',
      'readr',
      'magrittr',
      'purrr',
      'lubridate'
    )
  )
  url_base <-
    'http://api.gdeltproject.org/api/v1/search_ftxtsearch/search_ftxtsearch?query='

  if (term %>% is.na) {
    term_slug <-
      ''
    term_word <-
      'all words'
  } else {
    term_slug <-
      term %>%
      str_to_lower() %>%
      urltools::url_encode()

    term_word <-
      term
  }

  if (term %>% is.na() & !domain %>% is.na) {
    term_word <-
      domain
  }

  if (!domain %>% is.na()) {
    domain_slug <-
      '%20domain:' %>%
      paste0(domain)
  } else {
    domain_slug <-
      ''
  }


  if (!search_language %>% is.na()) {
    search_lang_slug <-
      '&searchlang:' %>%
      paste0(search_language %>% str_to_lower())
  } else {
    search_lang_slug <-
      ''
  }

  if (!source_language %>% is.na()) {
    source_lang_slug <-
      '&sourcelang:' %>%
      paste0(source_language %>% str_to_lower())
  } else {
    source_lang_slug <-
      ''
  }

  if (!last_minutes %>% is.na()) {
    last_minute_slug <-
      '%20lastminutes:' %>%
      paste0(last_minutes)
  } else {
    last_minute_slug <-
      ''
  }

  if (!tone_more_than %>% is.na()) {
    if (tone_more_than >= 100) {
      stop("Tone can't be over 100")
    }
    tone_more_slug <-
      '%20tonemorethan:' %>%
      paste0(tone_more_than)
  } else {
    tone_more_slug <-
      ''
  }

  if (!tone_less_than %>% is.na()) {
    if (tone_less_than >= 100) {
      stop("Tone can't be under 100")
    }
    tone_less_slug <-
      '%20tonelessthan:' %>%
      paste0(tone_less_than)
  } else {
    tone_less_slug <-
      ''
  }

  term_slug <-
    term_slug %>%
    paste0(
      domain_slug,
      last_minute_slug,
      tone_more_slug,
      tone_less_slug,
      search_lang_slug,
      source_lang_slug
    )
  sort_df <-
    data_frame(
      sort_term = c('date', 'relevence', 'tone.ascending', 'tone.descending'),
      sort_slug = c('date', 'rel', 'toneasc', 'tonedesc')
    )

  if (sort_by %in% sort_df$sort_term == F) {
    stop("Sorry sort terms can only be\n" %>%
           paste0(paste0(sort_df$sort_term, collapse = '\n')))
  }

  slug_sort <-
    sort_df %>%
    dplyr::filter(sort_term == sort_by) %>%
    .$sort_slug

  slug_sort <-
    '&sortby:' %>%
    paste0(slug_sort)



  if (dedeup_results == T) {
    dup_slug <-
      '&dropdup=true'
  } else {
    dup_slug <-
      ''
  }
  if (is_tone == T) {
    value_name <-
      'value.tone'
    output_slug <-
      '&output=timelinecsv&outputtype=tone'
  } else {
    output_slug <-
      '&output=timelinecsv&outputtype=volume'
    value_name <-
      'value.volume'
  }

  url <-
    url_base %>%
    paste0(term_slug,
           dup_slug,
           slug_sort,
           output_slug)

  page.has.content <-
    url %>%
    GET

  page_size_df <-
    page.has.content$headers  %>%
    flatten_df %>%
    mutate(`content-length` = `content-length` %>% as.numeric)

  if (page_size_df$`content-length` <= 41) {
    stop("This search has no data")
  }

  sentiment_data <-
    url %>%
    readr::read_csv() %>%
    mutate(term, url, date.date = Sys.time()) %>%
    dplyr::select(term, everything())


  names(sentiment_data)[2:3] <-
    c('date_time.url', 'date_time_human.url')

  names(sentiment_data)[4] <-
    value_name

  sentiment_data %<>%
    mutate(
      date_time.sentiment = date_time_human.url %>% mdy_hms(tz = 'UTC') %>%  with_tz(Sys.timezone()),
      date.sentiment = date_time_human.url %>% mdy_hms(tz = 'UTC') %>% as.Date()
    ) %>%
    dplyr::select(-c(date_time.url, date_time_human.url)) %>%
    dplyr::select(term, date_time.sentiment, date.sentiment, everything())

  if (!domain %>% is.na) {
    sentiment_data %<>%
      mutate(domain = domain) %>%
      dplyr::select(term, domain, everything())
  }

  if (term %>% is.na()) {
    sentiment_data <-
      sentiment_data %>%
      dplyr::select(-term)
  }

  if (return_message == T) {
    "You got " %>%
      paste0(sentiment_data %>% nrow,
             ' words for ',
             term_word,
             ' at ',
             Sys.time()) %>%
      message()
  }

  return(sentiment_data)

}

#' Returns GDELT free text API word clouds for a given domain, can be term restricted
#'
#' @param domains
#' @param term options \code{c(NA, "term_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples

get_data_sentiment_ft_api_domains <-
  function(domains = c('nytimes.com', 'washingtonpost.com'),
           term = NA,
           last_minutes = NA,
           is_tone = T,
           tone_less_than = NA,
           tone_more_than = NA,
           search_language = NA,
           source_language = NA,
           sort_by = 'date',
           dedeup_results = T,
           return_message = T) {
    load_needed_packages(c('dplyr', 'purrr'))
    get_data_sentiment_ft_api_safe <-
      failwith(NULL, get_data_sentiment_ft_api)

    all_data <-
      1:length(domains) %>%
      map(
        function(x)
          get_data_sentiment_ft_api_safe(
            term = term,
            domain = domains[x],
            last_minutes = last_minutes,
            is_tone = is_tone,
            tone_less_than = tone_less_than,
            tone_more_than = tone_more_than,
            search_language = search_language,
            source_language = source_language,
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }

#' Returns GDELT free text API sentiment for a given term, can be domain restricted
#'
#' @param terms
#' @param domain  \code{c(NA, "domain_name")}
#' @param last_minutes
#' @param search_language
#' @param tone_more_than
#' @param tone_less_than
#' @param source_language
#' @param sort_by
#' @param dedeup_results
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' get_data_sentiment_ft_api_terms(terms = c("Zika", '"Golden State Warriors"')) %>% View
get_data_sentiment_ft_api_terms <-
  function(terms = c("Zika", '"Golden State Warriors"'),
           domain = NA,
           last_minutes = NA,
           is_tone = T,
           tone_less_than = NA,
           tone_more_than = NA,
           search_language = NA,
           source_language = NA,
           sort_by = 'date',
           dedeup_results = T,
           return_message = T) {
    load_needed_packages(c('dplyr', 'purrr'))
    get_data_sentiment_ft_api_safe <-
      failwith(NULL, get_data_sentiment_ft_api)

    all_data <-
      1:length(terms) %>%
      map(
        function(x)
          get_data_sentiment_ft_api_safe(
            term = terms[x],
            domain = domain,
            last_minutes = last_minutes,
            is_tone = is_tone,
            tone_less_than = tone_less_than,
            tone_more_than = tone_more_than,
            search_language = search_language,
            source_language = source_language,
            sort_by = sort_by,
            dedeup_results = dedeup_results,
            return_message = return_message
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }