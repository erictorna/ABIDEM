.PATH = sprintf('.tmp/%s', OUT)
dir.create(.PATH, showWarnings = F, recursive= T)
rmarkdown::render(input=IN, output_dir=dirname(OUT), output_file=basename(OUT), intermediates_dir=.PATH, clean=F,
                  knit_root_dir = '.')

