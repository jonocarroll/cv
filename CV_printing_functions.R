# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!

#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?
#' @param pdf_mode Is the output being rendered into a pdf? Aka do links need
#'   to be stripped?
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
create_CV_object <- function(
  data_location,
  pdf_mode = FALSE,
  sheet_is_publicly_readable = TRUE
) {
  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(
    data_location,
    "docs\\.google\\.com"
  )

  if (is_google_sheets_location) {
    if (sheet_is_publicly_readable) {
      # This tells google sheets to not try and authenticate. Note that this will only
      # work if your sheet has sharing set to "anyone with link can view"
      googlesheets4::gs4_deauth()
    } else {
      # My info is in a public sheet so there's no need to do authentication but if you want
      # to use a private sheet, then this is the way you need to do it.
      # designate project-specific cache so we can render Rmd without problems
      options(gargle_oauth_cache = ".secrets")
    }

    read_gsheet <- function(sheet_id) {
      googlesheets4::read_sheet(
        data_location,
        sheet = sheet_id,
        skip = 1,
        col_types = "c"
      )
    }
    cv$entries_data <- read_gsheet(sheet_id = "entries")
    cv$skills <- read_gsheet(sheet_id = "language_skills")
    cv$text_blocks <- read_gsheet(sheet_id = "text_blocks")
    cv$contact_info <- read_gsheet(sheet_id = "contact_info")
  } else {
    # Want to go old-school with csvs?
    cv$entries_data <- readr::read_csv(
      paste0(data_location, "entries.csv"),
      skip = 1
    )
    cv$skills <- readr::read_csv(
      paste0(data_location, "language_skills.csv"),
      skip = 1
    )
    cv$text_blocks <- readr::read_csv(
      paste0(data_location, "text_blocks.csv"),
      skip = 1
    )
    cv$contact_info <- readr::read_csv(
      paste0(data_location, "contact_info.csv"),
      skip = 1
    )
  }

  extract_year <- function(dates) {
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) +
      10

    date_year
  }

  parse_dates <- function(dates) {
    date_month <- stringr::str_extract(
      dates,
      "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})"
    )
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(
        description_bullets != "",
        paste0("- ", description_bullets),
        ""
      ),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start & no_end ~ "N/A",
        no_start & has_end ~ as.character(end),
        has_start & no_end ~ paste("Current", "-", start),
        TRUE ~ paste(end, "-", start)
      )
    ) %>%
    dplyr::arrange(desc(parse_dates(end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

  cv
}


# Remove links from a text block and add to internal list
sanitize_links <- function(cv, text) {
  if (cv$pdf_mode) {
    link_titles <- stringr::str_extract_all(text, '(?<=\\[).+?(?=\\])')[[1]]
    link_destinations <- stringr::str_extract_all(text, '(?<=\\().+?(?=\\))')[[
      1
    ]]

    n_links <- length(cv$links)
    n_new_links <- length(link_titles)

    if (n_new_links > 0) {
      # add links to links array
      cv$links <- c(cv$links, link_destinations)

      # Build map of link destination to superscript
      link_superscript_mappings <- purrr::set_names(
        paste0("<sup>", (1:n_new_links) + n_links, "</sup>", collapse = ""),
        paste0("(", link_destinations, ")", collapse = "")
      )

      # Replace the link destination and remove square brackets for title
      text <- text %>%
        stringr::str_replace_all(stringr::fixed(link_superscript_mappings)) %>%
        stringr::str_replace_all('\\[(.+?)\\]', "\\1")
    }
  }

  list(cv = cv, text = text)
}


#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id, glue_template = "default") {
  if (glue_template == "default") {
    glue_template <- "
### {title}

{loc}

{institution}

{timeline}

{description_bullets}
\n\n\n"
  }

  section_data <- dplyr::filter(cv$entries_data, section == section_id)

  # Take entire entries data frame and removes the links in descending order
  # so links for the same position are right next to each other in number.
  for (i in 1:nrow(section_data)) {
    for (col in c('title', 'description_bullets')) {
      strip_res <- sanitize_links(cv, section_data[i, col])
      section_data[i, col] <- strip_res$text
      cv <- strip_res$cv
    }
  }

  print(glue::glue_data(section_data, glue_template))

  invisible(strip_res$cv)
}


#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label) {
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)

  strip_res <- sanitize_links(cv, text_block)

  cat(strip_res$text)

  invisible(strip_res$cv)
}


#' @description Construct a bar chart of skills
#' @param out_of The relative maximum for skills. Used to set what a fully filled in skill bar is.
print_skill_bars <- function(
  cv,
  out_of = 5,
  bar_color = "#969696",
  bar_background = "#ccdcea",
  glue_template = "default"
) {
  if (glue_template == "default") {
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{skill}</div>"
  }
  cv$skills %>%
    dplyr::mutate(width_percent = round(100 * as.numeric(level) / out_of)) %>%
    glue::glue_data(glue_template) %>%
    print()

  invisible(cv)
}


#' @description List of all links in document labeled by their superscript integer.
print_links <- function(cv) {
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat(
      "
Links {data-icon=link}
--------------------------------------------------------------------------------

<br>


"
    )

    purrr::walk2(cv$links, 1:n_links, function(link, index) {
      print(glue::glue('{index}. {link}'))
    })
  }

  invisible(cv)
}


#' @description Contact information section with icons
print_contact_info <- function(cv) {
  res <- glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}'></i> {contact}"
  )
  m <- cv$contact_info$icon == "mastodon"
  o <- cv$contact_info$icon == "orcid"
  mastodon <- '<svg id="mastodon_svg" xmlns="http://www.w3.org/2000/svg" width="12" height="12" fill="currentColor" class="bi bi-mastodon" viewBox="0 0 16 16">
  <path d="M11.19 12.195c2.016-.24 3.77-1.475 3.99-2.603.348-1.778.32-4.339.32-4.339 0-3.47-2.286-4.488-2.286-4.488C12.062.238 10.083.017 8.027 0h-.05C5.92.017 3.942.238 2.79.765c0 0-2.285 1.017-2.285 4.488l-.002.662c-.004.64-.007 1.35.011 2.091.083 3.394.626 6.74 3.78 7.57 1.454.383 2.703.463 3.709.408 1.823-.1 2.847-.647 2.847-.647l-.06-1.317s-1.303.41-2.767.36c-1.45-.05-2.98-.156-3.215-1.928a3.614 3.614 0 0 1-.033-.496s1.424.346 3.228.428c1.103.05 2.137-.064 3.188-.189zm1.613-2.47H11.13v-4.08c0-.859-.364-1.295-1.091-1.295-.804 0-1.207.517-1.207 1.541v2.233H7.168V5.89c0-1.024-.403-1.541-1.207-1.541-.727 0-1.091.436-1.091 1.296v4.079H3.197V5.522c0-.859.22-1.541.66-2.046.456-.505 1.052-.764 1.793-.764.856 0 1.504.328 1.933.983L8 4.39l.417-.695c.429-.655 1.077-.983 1.934-.983.74 0 1.336.259 1.791.764.442.505.661 1.187.661 2.046v4.203z"/>
</svg>'
  orcid <- '<svg id="orcid_svg" xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 512 512"><!--! Font Awesome Pro 6.4.0 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license (Commercial License) Copyright 2023 Fonticons, Inc. -->
  <path d="M294.75 188.19h-45.92V342h47.47c67.62 0 83.12-51.34 83.12-76.91 0-41.64-26.54-76.9-84.67-76.9zM256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm-80.79 360.76h-29.84v-207.5h29.84zm-14.92-231.14a19.57 19.57 0 1 1 19.57-19.57 19.64 19.64 0 0 1-19.57 19.57zM300 369h-81V161.26h80.6c76.73 0 110.44 54.83 110.44 103.85C410 318.39 368.38 369 300 369z"/>
  </svg>'
  res[m] <- glue::glue(
    "- &nbsp;{mastodon} <a id='mastodon'>{cv$contact_info$contact[m]}</a>"
  )
  res[o] <- glue::glue(
    "- &nbsp;{orcid} <a id='orcid'>{cv$contact_info$contact[o]}</a>"
  )
  print(res)

  invisible(cv)
}
