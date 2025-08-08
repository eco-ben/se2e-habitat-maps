
purrr::walk(c("./R scripts/Barents Sea.R",
              "./R scripts/Celtic Sea.R",
              "./R scripts/Clyde and Irish Sea.R",
              "./R scripts/English Channel.R",
              #"./R scripts/Greenland Sea.R",
              "./R scripts/North Sea.R",
              "./R scripts/W Scotland.R"), 
             MiMeMo.tools::execute)
