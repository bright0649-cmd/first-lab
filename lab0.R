# Lab 0: read a line of input and print a result
con <- file("stdin")
line <- readLines(con, n = 1)
close(con)

cat("You typed:", line, "\n")
cat("Number of characters:", nchar(line), "\n")
