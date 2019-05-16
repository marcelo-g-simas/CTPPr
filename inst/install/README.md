# CTPP Install Instructions

### For each record, use the Latest Download link to browse for the most recent version, or use the Direct Download link for your operating system

### Accept the program installation defaults as you are prompted

1) R
    - Windows
	    - Latest Download: https://cran.r-project.org/bin/windows/
        - Direct Download: https://cran.r-project.org/bin/windows/base/R-3.6.0-win.exe
    - Mac
        - Latest Download: https://cran.r-project.org/bin/macosx/
        - Direct Download: https://cran.r-project.org/bin/macosx/R-3.6.0.pkg

2) Tools
    - Windows:
        - Latest Download: https://cran.r-project.org/bin/windows/Rtools/
        - Direct Download: https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe 
    - Mac:
		- Latest Download: https://cran.r-project.org/bin/macosx/tools/
        - Direct Download: 
            - https://cran.r-project.org/bin/macosx/tools/gfortran-4.2.3.pkg
            - https://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.pkg
            - https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg

3) RStudio
    - Windows
        - Latest Download: https://www.rstudio.com/products/rstudio/download/#download
        - Direct Download: https://download1.rstudio.org/desktop/windows/RStudio-1.2.1335.exe
    - Mac
        - Latest Download: https://www.rstudio.com/products/rstudio/download/#download
        - Direct Download: https://download1.rstudio.org/desktop/macos/RStudio-1.2.1335.dmg

### Now in RStudio, install CTPP
```r
install.packages('remotes')
remotes::install_github('Westat-Transportation/CTPP')
```
