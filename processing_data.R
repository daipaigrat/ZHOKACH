## Установка необходимых пакетов

#install.packages(c("dplyr", "lubridate", "stringr", "tidyr", "readr"))
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)



source("to_drive.R")

members_path <- "members_improved_.csv"
chat_path <- "chats_improved_.csv"

## 1. Обработка таблицы участников (members) 
process_members <- function(members_path) {
  members <- read_from_drive(members_path, folder_id) %>% read_delim(
    members_path,
    delim = ";",
    col_types = cols(
      ID = col_character(),
      Имя = col_character(),
      Username = col_character(),
      Телефон = col_character(),
      Статус = col_character()),
    locale = locale(encoding = "UTF-8")
  ) %>%
    rename(
      member_status = `Статус`
    ) %>% 
    select(-Имя, -Телефон)
  return(members)
}

## 2. Обработка таблицы сообщений (chat_data) 
  process_chat_data <- function(chat_path) {
    chat <- read_from_drive(chat_path, folder_id) %>% read_delim(
      chat_path,
      delim = ";",
      col_types = cols(
        ID = col_character(),
        Дата = col_datetime(format = "%d.%m.%Y %H:%M"),
        id_Автора = col_character(),
        Автор = col_character(),
        Username = col_character(),
        Тип = col_character(),
        Текст = col_character()
      ),
      locale = locale(encoding = "UTF-8")
    ) %>%
      # Очистка данных
      mutate(
        # Очистка текста (сохраняем эмодзи и пунктуацию)
        Текст = str_remove_all(Текст, "[\\x00-\\x1F]"), # Удаляем управляющие символы
        Текст = str_squish(Текст) # Удаляем лишние пробелы
      ) %>%
      # Отбор только текстовых сообщений
      filter(Тип == "Текст" & !is.na(Текст)) %>%
      # Переименование колонок
      rename(
        message_id = ID,
        timestamp = Дата,
        id_author = ID_Автор,
        author_username = Автор,
        text = Текст
      ) %>%
      select(-Тип) # Удаляем ненужные колонки
    
    return(chat)
  }


## 3. Объединение таблиц и финальная обработка 
preprocess_data <- function(members_path, chat_path) {
  # Загрузка данных
  members <- process_members(members_path)
  chat <- process_chat_data(chat_path)
  
  # Объединение данных
  combined_data <- chat %>%
    left_join(members, by = c("id_author" = "ID")) %>%
    #Учитываем только реальных пользователей, т.к. есть системные сообщения от бота
    filter((grepl("@", author_username))) %>% 
    # Упорядочивание колонок
    select(
      message_id,
      timestamp,
      author_username,
      author_name,
      text,
      member_status,
      everything()
    )
  
  
  return(combined_data)
}

