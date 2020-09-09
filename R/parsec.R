parsec = function(file) {
  src = strsplit(readChar(file, file.info(file)$size), NULL)[[1L]]
  # logic below will assume terminal newline
  if (tail(src, 1L) != '\n') src = c(src, '\n')
  src = skip_white(preprocess(src))

  n_char = length(src)

  # (i think) conservative guess of 40 characters/expression up front
  exprs = vector('list', n_char %/% 40L)
  expr_i = char_i = 1L

  # strip out top-level expressions
  while (char_i <= length(src)) {
    if (char_i > n_char) break
    # pre-processor directives
    if (src[char_i] == '#') {
      end_expr = find_end_directive(src, char_i)
      exprs[[expr_i]] = src[char_i:end_expr]
      char_i = end_expr+1L

    }
    char_i = skip_white(char_i)
  }
}

# three preprocessing steps to normalize the code:
#   (1) collapse lines linked with a continuation (\)
#   (2) remove in-line comments (from // to a newline) and
#   (3) remove block comments (delimited by /* ... */)
preprocess = function(txt) {
  remaining = length(txt)
  i = 1L
  while (i <= remaining) {
    if (txt[i] == '/' && txt[i+1L] == '/') {
      j = i+2L
      while (txt[j] != '\n') { j=j+1L }
      txt = c(txt[1:(i-1L)], txt[j:remaining])
      remaining = remaining - j + i
    } else if (txt[i] == '/' && txt[i+1L] == '*') {
      j = i+2L
      while (txt[j] != '*' || txt[j+1L] != '/') { j=j+1L }
      txt = c(txt[1:(i-1L)], txt[j:remaining])
      remaining = remaining - j + i
    } else if (txt[i] == '"' || txt[i] == "'") {
      # bump ourselves outside of char/char array literals so
      #   we're sure any other \ we find is for line continuation
      delim = txt[i]
      i = i+1L
      while (txt[i] != delim && txt[i-1L] != '\\') { i=i+1L }
    } else if (txt[i] == '\\') {
      # overwrite from line continuation to newline with blanks
      j = i+1L
      while (txt[j] != '\n') { j=j+1L }
      txt[i:j] = ''
    } else {
      i = i+1L
    }
  }
  txt
}

# move the "cursor" along until non-whitespace is found.
#   intended to be used at the beginning of an expression
skip_white = function(txt, i) {
  n = length(txt)
  while (i <= n && txt[i] %in% c(' ', '\t', '\n')) {i = i+1L}
  return(i)
}

#
# initial position is at the # of a preprocessor directive. to find the end:
#   - TODO: for ifdef, ifndef, if, there could be nesting of directives
#   - for all others, end at the next newline (continuations are already removed)
find_end_directive = function(txt, i) {
  while (txt[i] != '\n') { i = i+1L }
  return( i-1L )
}
