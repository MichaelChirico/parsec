# some documentation:
#   https://gcc.gnu.org/onlinedocs/cpp/
#   https://docs.microsoft.com/en-us/cpp/preprocessor/preprocessor-directives
parse_directive = function(txt) {
  # skip whitespace after #
  char_i = skip_white(txt, 2L)
  end_directive = skip_identifier(txt, char_i) - 1L
  directive = paste(txt[char_i:end_directive], collapse='')
  switch(
    directive,
    include = {
      char_i = skip_white(txt, end_directive+1L)
      end_arg = switch(
        txt[char_i],
        '<' = skip_pair_delim(txt, char_i, '<'),
        '"' = skip_quoted(txt, char_i, '"')-1L,
        stop(domain=NA, gettextf(
          "Improper usage of #include directive: %s",
          paste(txt, collapse=''), domain="R-parsec"
        ))
      )
      call('directive_include', paste(txt[char_i:end_arg], collapse=''))
    },
    define = {

    },
    stop("Unknown preprocessor directive")
  )
}
