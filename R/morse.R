
count_seq_breaks <- function(seq, seq_step = 1) {
  first_diff <- c(seq_step, diff(seq)) - seq_step
  periods <- as.numeric(as.factor(cumsum(abs(first_diff))))
  periods
}

#' Convert ASCII text to morse code
#'
#' @param text The text to convert. Ignores anything except the letters a-z
#'   (both upper and lower case), numbers, and spaces.
#'
#' @return A character string of Morse code.
#' @export
#'
#' @examples
#' text_to_morse_chars("SOS SOS SOS")
text_to_morse_chars <- function(text) {
  text <- toupper(text)

  morse_code <- morse_code %>%
    dplyr::mutate(morse = ifelse(morse == "",
                          " ", morse))

  text <- text %>% stringr::str_remove_all("[^A-Z0-9 ]")

  chars <- strsplit(text, "") %>%
    unlist()

  morse <- chars %>%
    purrr::map_chr(~morse_code$morse[ morse_code$char %in% .]) %>%
    paste(collapse = " ")

  morse

}

#' Converts a text into a binary representation of Morse code
#'
#' @param text The text to convert. Ignores anything except the letters a-z
#'   (both upper and lower case), numbers, and spaces.
#'
#' @return A numeric vector with a binary representation of Morse code (with 1 =
#'   dot, 111 = bar, 0 = separator).
#' @export
#'
#' @examples
#' text_to_morse_numeric("SOS SOS SOS")
text_to_morse_numeric <- function(text) {
  text <- toupper(text)

  text <- text %>% stringr::str_remove_all("[^A-Z0-9 ]")

  chars <- strsplit(text, "") %>%
    unlist()

  morse <- chars %>%
    purrr::map_chr(~morse_code$morse_numeric[ morse_code$char %in% .]) %>%
    paste(collapse = "") %>%
    strsplit("") %>%
    unlist() %>%
    as.integer()

  morse

}

#' Convert text to numeric matrix of Morse code
#'
#' @param text The text to convert. Ignores anything except the letters a-z
#'   (both upper and lower case), numbers, and spaces.
#' @param line_length Line length to use (number of columns in the resulting
#'   matrix). Default is 40.
#'
#' @return A matrix with a numeric binary representation of Morse code. Three 1s
#'   in a row represent a bar; a 1 represents a dot; a 0 is a separator. Each
#'   row of Morse code is separated from the others by a row of zeros. This is
#'   mostly useful for plotting and art.
#' @export
#'
#' @examples
#' text_to_morse_matrix("SOS SOS SOS")
#' text_to_morse_matrix("SOS SOS SOS", line_length = 10)
text_to_morse_matrix <- function(text, line_length = 40) {
  morse <- text_to_morse_numeric(text)

  padding <- line_length - (length(morse) %% line_length)

  morse <- c(morse, rep(0, padding))

  num_lines <- length(morse)/line_length

  morse %>%
    split(rep(1:num_lines, each = line_length)) %>%
    purrr::map(~c(rep(0, line_length), ., rep(0, line_length))) %>%
    purrr::reduce(c) %>%
    matrix(ncol = line_length, byrow = TRUE)

}

#' Convert text to a data frame with coordinates of Morse code segments
#'
#' @param text The text to convert. Ignores anything except the letters a-z
#'   (both upper and lower case), numbers, and spaces.
#' @param line_length Line length to use (number of columns in the resulting
#'   matrix). Default is 40.
#'
#' @return A [tibble::tibble] with segment coordinates of Morse code. This is
#'   mostly useful for plotting and art.
#' @export
#'
#' @examples
#' library(ggplot2)
#' text_to_morse_segments("SOS SOS SOS") %>%
#'   ggplot() +
#'   geom_segment(aes(x = x, xend = xend, y = y, yend = yend), size = 2)
#'
#' text_to_morse_segments("SOS SOS SOS", line_length = 10)
text_to_morse_segments <- function(text, line_length = 40) {

  x <- xend <- y <- yend <- group <- layer <- NULL

  morse <- text_to_morse_matrix(text, line_length = line_length)

  morse_raster <- morse %>%
    raster::raster()

  res <- raster::res(morse_raster)

  morse_segments <- morse_raster %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::mutate(group = count_seq_breaks(ceiling(abs(layer)), seq_step = 0)) %>%
    dplyr::filter(layer != 0) %>%
    dplyr::group_by(group, y) %>%
    dplyr::summarise(xend = max(x) + (res[1]/2),
                     x = min(x) - (res[1]/2),
                     yend = y) %>%
    dplyr::select(x, xend, y, yend, group) %>%
    dplyr::ungroup()

  morse_segments

}

#' Converts text to Morse code sounds
#'
#' @param text The text to convert. Ignores anything except the letters a-z
#'   (both upper and lower case), numbers, and spaces.#' @param pulse_duration
#' @param pulse_duration How long the dot lasts, in seconds. Default is 0.25
#'   seconds. Bars last for three times as long.
#' @param play Whether to play the resulting sound immediately. Default is
#'   `TRUE`.
#' @param sampling_freq The sampling frequency. Default is 8000.
#' @param carrier_freq The carrier tone. Deafult is 440 Hz (flat A).
#'
#' @return A soundwave matrix that can be played via [audio::play].
#' @export
#'
#' @examples
#' \dontrun{
#' sound <- text_to_morse_sounds("SOS SOS SOS")
#' audio::play(sound, rate = 8000)
#' audio::play(sound, rate = 24000)}
text_to_morse_sounds <- function(text,
                                 pulse_duration = 0.25,
                                 play = TRUE,
                                 sampling_freq = 8000,
                                 carrier_freq = 440) {
  morse <- text_to_morse_numeric(text)

  morse <- paste(morse, collapse = "") %>%
    stringr::str_replace_all("111", "3") %>%
    strsplit("") %>%
    unlist() %>%
    as.numeric()

  sound <- matrix(numeric(0))

  for(i in 1:length(morse)) {
    if(morse[i] == 0) {
      sound <- seewave::addsilw(sound, f = sampling_freq, d = pulse_duration)
    } else {
      sound <- rbind(sound, seewave::synth(f = sampling_freq,
                              d = morse[i] * pulse_duration,
                              cf = carrier_freq))
    }
  }
  if(play) {
    audio::play(sound, rate = sampling_freq)
  }
  sound


}

#' Convert Morse characters back to text
#'
#' @param morse String with Morse characters separated by spaces. Must use `.`
#'   and `-` as dots and bars.
#'
#' @return A string of letters from whatever valid Morse code it can find in the
#'   input. It's fault tolerant - it will ignore bad Morse code.
#' @export
#'
#' @examples
#' text_to_morse_chars("Hello how are you") %>% morse_to_text()
#' # This is fault tolerant of bad Morse code:
#' morse <-  ".... . ..-.. .-AA.. ---"
#' morse_to_text(morse)
morse_to_text <- function(morse) {
  morse_separated <- stringr::str_split(morse, " ") %>%
    unlist()

  chars <- morse_separated %>%
    purrr::map_chr(~(morse_code$char[ morse_code$morse %in% .] %0% "")) %>%
    paste(collapse = " ")

  chars


}

`%0%` <- function(lhs, rhs) {
  ifelse(length(lhs) == 0,
         rhs,
         lhs)
}
