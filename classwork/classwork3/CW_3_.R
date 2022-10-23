msg <- readLines('putin-2018.txt')

#количество строк
length(msg)

#количество символов - способ 1
l <- lapply(msg, nchar)  #nchar - counts symbols in rows
Reduce(sum, l)           #reduce - combines symbols

#количество символов - способ 2
txt <- paste(msg, collapse = " ")
nchar(txt)

#строки, в которых встречаются "мы"
lcw <- grep(' мы ', msg)
lcw

#строки, в которых встречаются "Мы"
ucw <- grep('Мы ', msg)
ucw

#строки, в которых встречаются "мы" и "Мы"
w <- c(lcw, ucw)
sort(unique(w))

#разделяем текст на слова
words1 <- unlist(strsplit(txt, (' ')))

#Количество слов
length(words1)

#таблица самых часто встречающихся слов
wc1 <- table(words1)
wc1 <- sort(wc1, decreasing = TRUE)
head(wc1, 20)


# дополнение

#удаляем пунктуационные знаки
words2 <- words1
words2 <- gsub("[[:punct:] ]+",' ', words2)
#удаляем пробелы
words2 <- trimws(words2)
#удаляем пустые значения
words2 <- words2[words2 != ""]

#Количество слов
length(words2)

#таблица самых часто встречающихся слов
wc2 <- table(words2)
wc2 <- sort(wc2, decreasing = TRUE)
#head(wc1, 20)
head(wc2, 20)

#Процентное сравнение
fr2 = round(wc2/length(words2)*100, 2)
head(fr2, 20)

