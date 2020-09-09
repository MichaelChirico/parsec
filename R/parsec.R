parsec = function(file) {
  src = strsplit(readChar(file, file.info(file)$size), NULL)[[1L]]
  # logic below will assume terminal newline
  if (tail(src, 1L) != '\n') src = c(src, '\n')
  src = preprocess(src)
  n_char = length(src)

  # (i think) conservative guess of 40 characters/expression up front
  exprs = vector('list', n_char %/% 40L)
  expr_i = 1L
  char_i = skip_white(src, i)

  # strip out top-level expressions
  while (char_i <= length(src)) {
    # pre-processor directives
    if (src[char_i] == '#') {
      end_expr = find_end_directive(src, char_i)
      exprs[[expr_i]] = src[char_i:end_expr]
      expr_i = expr_i+1L
      char_i = end_expr+1L
    # "regular' C expressions
    } else {
      end_expr = char_i
      while (end_expr <= n_char) {
        end_expr = skip_white(src, skip_identifier(src, end_expr))
        # function call definition, or function prototype
        if (src[end_expr] == '(') {
          n_lparen = 1L
          while (n_lparen > 0L) {
            end_expr = end_expr + 1L
            if (src[end_expr] == ')') n_lparen = n_lparen-1L
            else if (src[end_expr] == '(') n_lparen = n_lparen+1L
          }
          end_expr = skip_white(src, end_expr+1L)
          # function call definition
          if (src[end_expr] == '{') {
            n_lbracket = 1L
            while (n_lbracket > 0L) {
              end_expr = end_expr + 1L
              if (src[end_expr] == '}') n_lbracket = n_lbracket-1L
              else if (src[end_expr] == '{') n_lbracket = n_lbracket+1L
            }
          # function prototype
          } else if (src[end_expr] != ';') stop("I'm not sure this is possible?")
          exprs[[expr_i]] = src[char_i:end_expr]
          expr_i = expr_i+1L
          char_i = end_expr+1L
          break
        } else if (src[end_expr] == '{') {
        }
      }
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

WHITESPACE_REX = '[ \t\n]'
# move the "cursor" along until non-whitespace is found.
#   intended to be used at the beginning of an expression
skip_white = function(txt, i) {
  n = length(txt)
  while (i <= n && grepl(WHITESPACE_REX, txt[i])) {i = i+1L}
  return(i)
}

# reference https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html#Identifiers
IDENTIFIER_REX0 = '[a-zA-Z_]'    # initial character
IDENTIFIER_REX1 = '[a-zA-Z_0-9]' # subsequent characters
skip_identifier = function(txt, i) {
  if (!grepl(IDENTIFIER_REX0, txt[i])) return(i)
  i = i+1L
  n = length(txt)
  while (i <= n && grepl(IDENTIFIER_REX1, txt[i])) { i=i+1L }
  return(i)
}

# initial position is at the # of a preprocessor directive. to find the end:
#   - for ifdef, ifndef, if, there could be nesting of directives
#   - for all others, end at the next newline (continuations are already removed)
# reference: https://gcc.gnu.org/onlinedocs/gcc-3.3.6/cpp/The-preprocessing-language.html
find_end_directive = function(txt, i) {
  # whitespace allowed after #; we land on the first character after #
  i = skip_white(txt, i+1L)
  if (txt[i] == 'i' && txt[i+1L] == 'f') {
    i = i+2L
    n_if = 1L
    # each iteration finds the next # anchor until it's an endif.
    #   other ifs found along the way increase the nesting
    while (n_if > 0L) {
      while (txt[i] != '#') { i=i+1L }
      i = skip_white(txt, i+1L)
      if (all(txt[i + 0:4] == c('e','n','d','i','f'))) { i=i+5L; n_if=n_if-1L }
      else if (txt[i] == 'i' && txt[i+1L] == 'f')   { i=i+2L; n_if=n_if+1L }
    }
  }
  while (txt[i] != '\n') { i=i+1L }
  return( i-1L )
}
