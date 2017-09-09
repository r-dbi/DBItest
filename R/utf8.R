text_cyrillic <- "\u041a\u0438\u0440\u0438\u043b\u043b"

text_latin <- "M\u00fcller"

text_latin_encoded <- iconv(text_latin, from = "UTF-8", to = "latin1")

text_chinese <- "\u6211\u662f\u8c01"

text_ascii <- iconv("ASCII", to = "ASCII")

texts <- c(text_cyrillic, text_latin, text_latin_encoded, text_chinese, text_ascii)
