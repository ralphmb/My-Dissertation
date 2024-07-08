#let c = counter("appendix")
#let appendix(it) = block[
  #c.step()
  #heading([Appendix #c.display("1"): #it], numbering: none, outlined:false)
]
#set par(first-line-indent: 0pt)
#show table: set text(7pt)
#show table: set align(center)

Blank/commented lines have been automatically removed from each script. This makes it a bit harder to read but saves about 20 pages.
#appendix([Weibull model selection])
R output for model selection in survival analysis section. `hyptester` is a custom function for easier to read significance tests.\
#raw(read("../assets/misc/r_output.txt"), lang:"R")


#appendix([Data Prep])
Code for data preparation. \
#raw(read("../assets/code/code/data_prep.R"), lang:"R")


#appendix([Exploratory])
Code for exploratory analysis. \
#raw(read("../assets/code/code/exploratory.R"), lang:"R")


#appendix([Logistic Regression])
Code for logistic regression. \
#raw(read("../assets/code/code/glm.R"), lang:"R")


#appendix([Survival Analysis])
Code for survival analysis. \
#raw(read("../assets/code/code/survival.R"), lang:"R")


#set page(columns: 2)
#set text(9pt)

#appendix([Bradley Terry models])
Unfinished section on Bradley Terry models. Due to a different approach and a few errors this was written under the impression that the logistic regression portion of this thesis was much less fruitful.\
#include("../other/bradleyterry.typ")


#set page(columns: 1)
#set text(7pt)

#appendix([Miscellaneous Scripts])
Python script used for parsing text copied from the Mirror into valid R code defining the columns of a data frame. \

#raw(read("../assets/code/code/derbies/siteparser.py"),lang:"Python")

Python script for converting R output from `summary()` into Typst tables.\
#raw(read("../assets/code/tablemaker.py"), lang:"Python")

Almost automated backups of all project files to Google Drive.\
#raw(read("../assets/code/committer"), lang: "zsh")

Word counter for source files, since it can be annoying to word count pdf files.\
#raw(read("../assets/code/wordcounter"), lang: "zsh")

#appendix([Diary of work])

#raw(read("../assets/misc/DIARY.md"))
