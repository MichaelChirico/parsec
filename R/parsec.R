parsec = function(file) {
  src = strsplit(readChar(file, file.info(file)$size), NULL)[[1L]]
  # logic below will assume terminal newline
  if (tail(src, 1L) != '\n') src = c(src, '\n')
  src = strip_comments(src)

  # (i think) conservative guess of 40 characters/expression up front
  exprs = vector('list', length(src) %/% 40L)
  expr_i = char_i = 1L

  # define locally to have src in parent frame
  skip_white = function(i) {
    while (src[i] %in% c(' ', '\t', '\n')) {i = i+1L}
    char_i <<- i
    return(invisible())
  }
  skip_comment = function(i) {
    if (src[i] == '/' && src[i+1L] == '/') {
      i = i + 2L
      while (src[i] != '\n') {i = i+1L}
    } else if (src[i] == '/' && src[i+1L] == '*') {
      i = i + 2L
      while (src[i] != '*' || src[i+1L] != '/') {i = i+1L}
    }
    char_i <<- i
  }
  while (char_i <= length(src)) {
    skip_white(char_i)

  }
}

strip_comments = function(txt) {
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
    } else {
      i = i+1L
    }
  }
  txt
}

skip_white = function(

# starting from first position of input txt, split it into expressions
expr_split = function(txt) {

}
