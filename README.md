# toolboxR

A collection of convenience wrappers and simple functions for bioinformatics, clinical/biological data curation, and fileIO. 

Version 1.0 is a the first major revision. The intent is to consistently implement tidy ethos of common table formats, vectorization, unquoted parameterization and pipeability. Many of my custom functions ahave also been generically implemented in the tidyverse removing the need to maintain them here (e.g. drop_column("col", df) is now accomplished with df %>% select(-col) ).
