# setup (required LaTeX and R packages)
rmd_file = '2020-rstudio-conf-rmarkdown-demo.Rmd'
rmd_yaml = '2020-rstudio-conf-rmarkdown-demo.yml'
for (f in c(rmd_file, rmd_yaml, 'coffee4.sty', 'rstudio-conf20.docx', 'rstudio-conf20.pptx'))
  if (!file.exists(f)) xfun::download_file(
    paste0('https://slides.yihui.org/', f),
    mode = if (grep('^text/', mime::guess_type(f))) 'w' else 'wb'
  )
for (pkg in c('tufte', 'rolldown', 'flexdashboard', 'rticles', 'pagedown', 'learnr', 'rstudioapi'))
  if (!xfun::loadable(pkg, new_session = TRUE)) install.packages(pkg)
file.copy(
  system.file('rmarkdown/templates/jss_article/skeleton/jsslogo.jpg', package = 'rticles'),
  'jsslogo.jpg', overwrite = TRUE
)

options(digits = 3)
t0 = Sys.time()

dark_css = c(
  '```{css, echo=FALSE}',
  '@media (prefers-color-scheme: dark) {',
  '  body {',
  '    background-color: black;',
  '    filter: invert(1);',
  '  }',
  '}',
  '```'
)

type_it = function(x) {
  xfun::rstudio_type(
    x, pause = function() rbeta(1, 1, 9)/60, mistake = .001
  )
  rstudioapi::documentSave()
}

focus_console = function() {
  rstudioapi::sendToConsole('', execute = FALSE, echo = FALSE)
}

pause = function(x = 'Continue...') {
  focus_console()
  x = paste0('[', format(Sys.time() - t0), '] ', x)
  x = strwrap(x, 50)
  readline(paste(x, collapse = '\n'))
}

pause('I want to tell you a horror story---iris. Not horrible enough? Alright. Another one---pie charts. Still does not scare you? 3D and animated pie charts! Did I see someone left the room?')

rmd_sans = xfun::sans_ext(rmd_file)
system(sprintf('git checkout -- %s', rmd_file))
Sys.sleep(.5)
rmd_text = readLines(rmd_file)
writeLines(character(0), rmd_file)
file.edit(rmd_file)
Sys.sleep(1)
type_it(rmd_text)

cache_idx = 0  # different cache dirs for different output formats
n = which(rmd_text == '---')[2]  # position of the second ---
fmts = readLines(rmd_yaml)
fmts = split(fmts, cumsum(fmts == ''))
for (fmt in fmts) {
  rstudioapi::navigateToFile(rmd_file, n)
  pause()
  fmt = fmt[fmt != '']
  type_it(c('output:', fmt, ''))
  rstudioapi::setCursorPosition(c(n + length(fmt) + 2, 1))
  pause()

  cache_idx = cache_idx + 1
  opts = options(
    knitr.chunk.cache.path = sprintf('%s_cache/%d/', rmd_sans, cache_idx),
    knitr.chunk.fig.path = sprintf('%s_files/%d/', rmd_sans, cache_idx)
  )
  is_format = function(x) any(grepl(sprintf('^\\s*%s:', x), fmt))
  compile_it = function() {
    if (is_format('learnr::tutorial')) {
      type_it(c('```{r, include=FALSE}', 'library(learnr)', '```'))
      rmarkdown::run(rmd_file, render_args = list(clean = FALSE))
    } else {
      rmarkdown::render(rmd_file, clean = FALSE)
    }
  }
  out = compile_it()
  # add random coffee stains in the .tex output
  if (is_format('latex_document')) {
    tex = readLines(out)
    for (i in grep('^\\\\(sub)section', tex)) {
      tex[i] = paste(
        sprintf('\\cofe%sm{1}{1.0}{0}{0}{0}', sample(LETTERS[1:4], 1)), tex[i],
        sep = '\n'
      )
    }
    writeLines(tex, out)
    out = tinytex::pdflatex(out)
  }

  open_it = function() {
    if (xfun::is_windows()) {
      shell.exec(out)
    } else if (xfun::is_macos()) {
      system2('open', shQuote(out))
    } else {
      system2('xdg-open', shQuote(out))
    }
  }

  open_it()
  pause()

  if (is_format('html_document')) {
    type_it(c('', dark_css[-c(2, 7)], ''))
    pause()
    compile_it(); open_it()
    rstudioapi::setCursorPosition(c(16, 1))
    type_it(c(dark_css[2], ''))
    rstudioapi::setCursorPosition(c(21, 1))
    type_it(c(dark_css[7], ''))
    pause()
    compile_it(); open_it()
    pause()
  }
  writeLines(rmd_text, rmd_file)
  rstudioapi::documentClose()
  options(opts)
}

# blogdown demo
rmd_text[4] = 'date: "2020-01-30"'
writeLines(rmd_text, rmd_file)
if (!dir.exists('rmarkdown-demo'))
  blogdown::new_site('rmarkdown-demo', serve = FALSE)
file.copy(rmd_file, 'rmarkdown-demo/content/post/', overwrite = TRUE)
xfun::in_dir('rmarkdown-demo', blogdown::serve_site())

message('They said Markdown was too simple?')

# clean up
if (FALSE) {
  system('git clean -d -f')
  system('git checkout -- .')
}
