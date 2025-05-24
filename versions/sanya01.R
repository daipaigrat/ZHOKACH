library(ini)
library(reticulate)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(googledrive)

# Читаем конфигурационный файл
config <- read.ini("config.ini")

# Функции для работы с последним ID
save_last_id <- function(last_id) {
  writeLines(as.character(last_id), "last_message_id.txt")
}

load_last_id <- function() {
  if (file.exists("last_message_id.txt")) {
    as.integer(readLines("last_message_id.txt"))
  } else {
    NULL
  }
}

# Python-код с плейсхолдером для last_id
python_code_template <- '
import asyncio
import json
import configparser
from datetime import datetime
from telethon import TelegramClient
from telethon.tl.functions.messages import GetHistoryRequest
from telethon.tl.types import (
    MessageMediaPhoto,
    MessageMediaDocument,
    DocumentAttributeVideo,
    DocumentAttributeAudio,
    DocumentAttributeSticker
)

def safe_getattr(obj, attr, default=None):
    return getattr(obj, attr, default) if obj else default

async def fetch_data(last_id=None):
    config = configparser.ConfigParser()
    config.read("config.ini", encoding="utf-8")
    api_id = config.getint("Telegram", "api_id")
    api_hash = config.get("Telegram", "api_hash")
    phone = config.get("Telegram", "phone")
    password = config.get("Telegram", "password", fallback=None)
    target_chat_title = config.get("Telegram", "target_chat")

    client = TelegramClient(f"{phone}.session", api_id, api_hash)
    await client.start(phone=phone, password=password)

    # Поиск чата
    target_group = None
    async for dialog in client.iter_dialogs():
        if dialog.name == target_chat_title:
            target_group = dialog.entity
            break

    if not target_group:
        print("Чат не найден")
        return None

    # Участники
    all_participants = await client.get_participants(target_group)
    participants_data = []
    for user in all_participants:
        last_online = safe_getattr(safe_getattr(user, "status", None), "was_online", None)
        participants_data.append({
            "id": user.id,
            "first_name": user.first_name or "",
            "last_name": user.last_name or "",
            "username": user.username or "",
            "phone": user.phone or "",
            "bot": user.bot,
            "premium": safe_getattr(user, "premium", False),
            "last_online": last_online.strftime("%Y-%m-%d %H:%M") if last_online else ""
        })

    # Сообщения
    messages_data = []
    offset_id = 0
    limit = 100
    min_id = last_id if last_id else 0
    
    while True:
        history = await client(GetHistoryRequest(
            peer=target_group,
            offset_id=offset_id,
            offset_date=None,
            add_offset=0,
            limit=limit,
            max_id=0,
            min_id=min_id,
            hash=0
        ))
        if not history.messages:
            break
        for msg in history.messages:
            text = safe_getattr(msg, "message", "") or safe_getattr(safe_getattr(msg, "media", None), "caption", "")
            media_type = "text"
            mime_type = ""
            size_kb = ""

            if hasattr(msg, "media") and msg.media:
                m = msg.media
                if isinstance(m, MessageMediaPhoto):
                    media_type = "photo"
                elif isinstance(m, MessageMediaDocument):
                    doc = m.document
                    mime_type = safe_getattr(doc, "mime_type", "")
                    size = safe_getattr(doc, "size", 0)
                    size_kb = f"{size/1024:.1f} KB"
                    for attr in safe_getattr(doc, "attributes", []):
                        if isinstance(attr, DocumentAttributeVideo): media_type = "video"
                        elif isinstance(attr, DocumentAttributeAudio): media_type = "audio"
                    if "sticker" in mime_type or "webp" in mime_type:
                        media_type = "sticker"
                    else:
                        media_type = "document"

            messages_data.append({
                "id": msg.id,
                "from_id": msg.from_id.user_id if hasattr(msg,"from_id") and msg.from_id else None,
                "date": msg.date.strftime("%Y-%m-%d %H:%M"),
                "text": text.strip().replace("\\n", " ")[:500],
                "text_len": len(text),
                "media_type": media_type,
                "mime": mime_type,
                "size": size_kb
            })
        offset_id = history.messages[-1].id

    await client.disconnect()
    return {
        "participants": json.dumps(participants_data, ensure_ascii=False),
        "messages": json.dumps(messages_data, ensure_ascii=False),
        "new_last_id": max([msg["id"] for msg in messages_data]) if messages_data else last_id
    }

last_id = %s  # Будет заменено на actual value
result = asyncio.run(fetch_data(last_id=last_id))
'

# Авторизация в Google Drive
drive_auth(path = config$GoogleDrive$service_account_json)

# Функция для обработки и загрузки данных
process_and_upload <- function(initial_run = FALSE) {
  last_id <- if (!initial_run) load_last_id() else NULL
  
  # Формируем Python-код с подстановкой last_id
  python_code <- gsub("last_id = %s", 
                      sprintf("last_id = %s", 
                              ifelse(is.null(last_id), "None", last_id)), 
                      python_code_template)
  
  py_run_string(python_code)
  result <- py$result
  
  # Парсим JSON-данные
  participants <- fromJSON(result$participants)
  messages <- fromJSON(result$messages)
  
  # Сохраняем новый last_id
  if (!is.null(result$new_last_id)) {
    save_last_id(result$new_last_id)
  }
  
  # Обрабатываем участников
  participants_processed <- participants %>%
    mutate(
      Имя = ifelse(str_trim(paste(first_name, last_name)) == "", "Нет имени",
                   str_trim(paste(first_name, last_name))),
      Username = ifelse(username == "", "Нет username", paste0("@", username)),
      Тип = ifelse(bot, "Бот", "Человек"),
      Статус = ifelse(premium, "Премиум", "Обычный"),
      Последний_онлайн = ifelse(last_online == "", "Нет данных", last_online)
    ) %>%
    select(ID = id, Имя, Username, Телефон = phone, Статус)
  
  # Обрабатываем сообщения
  if (length(messages) > 0) {
    participant_lookup <- participants %>%
      mutate(
        author_name = ifelse(str_trim(paste(first_name, last_name)) == "", "Нет имени",
                             str_trim(paste(first_name, last_name))),
        author_username = ifelse(username == "", "Нет username", paste0("@", username))
      ) %>%
      select(id, author_name, author_username)
    
    messages_processed <- messages %>%
      mutate(from_id = as.integer(from_id)) %>%
      left_join(participant_lookup, by = c("from_id" = "id")) %>%
      mutate(
        author_name = ifelse(is.na(author_name), "Неизвестный автор", author_name),
        author_username = ifelse(is.na(author_username), "@неизвестно", author_username),
        Дата = date,
        Тип = ifelse(media_type == "text", "Текст", "Медиа"),
        Текст = ifelse(text == "", "(без текста)", text),
        Длина_текста = text_len,
        Тип_медиа = ifelse(media_type == "text", "", media_type),
        MIME = mime,
        Размер = ifelse(size == "", "", size)
      ) %>%
      select(
        ID = id, 
        Дата, 
        Автор = author_name, 
        Username = author_username,
        Тип, 
        Текст
      )
    
    # Загружаем данные на Google Drive
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Функция для загрузки файла
    upload_file <- function(data, prefix) {
      temp_file <- tempfile(fileext = ".csv")
      write_csv(data, temp_file)
      drive_upload(
        media = temp_file,
        path = as_id(config$GoogleDrive$folder_id),
        name = sprintf("%s_%s.csv", prefix, timestamp),
        overwrite = TRUE
      )
      unlink(temp_file)
    }
    
    upload_file(participants_processed, "members_improved")
    upload_file(messages_processed, "chats_improved")
    
    cat(sprintf("Данные за %s успешно загружены!\n", timestamp))
  } else {
    cat("Новых сообщений не найдено.\n")
  }
}

# Первый запуск (полная загрузка)
cat("=== Начальная загрузка истории ===\n")
process_and_upload(initial_run = TRUE)

# Цикл для ежедневного обновления
cat("=== Запуск в режиме ежедневного обновления ===\n")
while (TRUE) {
  cat("Ожидание следующего обновления (24 hours)...\n")
  Sys.sleep(300)  # 24 часа
  cat("=== Начало ежедневного обновления ===\n")
  process_and_upload()
}