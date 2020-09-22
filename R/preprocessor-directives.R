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
    include = parse_include(txt, skip_white(txt, end_directive+1L)),
    define = {
    },
    stop("Unknown preprocessor directive")
  )
}

parse_include = function(txt, i) {
  end_arg = switch(
    txt[i],
    '<' = skip_pair_delim(txt, i, '<'),
    '"' = skip_quoted(txt, i, '"')-1L,
    stop(domain=NA, gettextf(
      "Improper usage of #include directive: %s",
      paste(txt, collapse=''), domain="R-parsec"
    ))
  )
  call('directive_include', paste(txt[i:end_arg], collapse=''))
}

# TODO: this is probably gonna cause issues -- unexpanded macros
#    can generally lead to "non-syntactic" C-code which parses
#    only after the macros are expanded. Substantially complicates
#    the task of making the C AST. Tabled for later. Perhaps
#    need an argument to parsec: `expand_macros`? With four
#    values: "no", "local", "project", "recursive" -- to allow
#    expansion of macros (1) from the same file (2) from
#    headers in the same project (3) across all #includes etc
# See: https://gcc.gnu.org/onlinedocs/cpp/Macro-Arguments.html#Macro-Arguments
parse_define_object = function(txt, i) {
}

parse_define_function = function(txt, i) {
}
