# Устанавливаем зависимости
library(reticulate)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(googledrive)

# Читаем конфигурационный файл
config <- read.ini("config.ini")

# Модифицированный Python-код, который возвращает данные как JSON-строки
python_code <- '
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

async def fetch_and_save():
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
    while True:
        history = await client(GetHistoryRequest(
            peer=target_group,
            offset_id=offset_id,
            offset_date=None,
            add_offset=0,
            limit=limit,
            max_id=0,
            min_id=0,
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
        "messages": json.dumps(messages_data, ensure_ascii=False)
    }

result = asyncio.run(fetch_and_save())
'

# Выполняем Python-код и получаем данные
cat("Выполнение Python-клиента Telethon...\n")
py_run_string(python_code)
result <- py$result

# Парсим JSON-данные
participants <- fromJSON(result$participants)
messages <- fromJSON(result$messages)

# Обрабатываем данные
participants <- participants %>%
  mutate(
    Имя = ifelse(str_trim(paste(first_name, last_name)) == "", "Нет имени",
                 str_trim(paste(first_name, last_name))),
    Username = ifelse(username == "", "Нет username", paste0("@", username)),
    Тип = ifelse(bot, "Бот", "Человек"),
    Статус = ifelse(premium, "Премиум", "Обычный"),
    Последний_онлайн = ifelse(last_online == "", "Нет данных", last_online)
  ) %>%
  select(ID = id, Имя, Username, Телефон = phone, Тип, Статус, Был_онлайн = Последний_онлайн)

messages <- messages %>%
  mutate(
    Дата = date,
    Тип = ifelse(media_type == "text", "Текст", "Медиа"),
    Текст = ifelse(text == "", "(без текста)", text),
    Длина_текста = text_len,
    Тип_медиа = ifelse(media_type == "text", "", media_type),
    MIME = mime,
    Размер = ifelse(size == "", "", size)
  ) %>%
  select(ID = id, Дата, Тип, Текст, Длина_текста, Тип_медиа, MIME, Размер)

# Авторизация в Google Drive
drive_auth(path = config$GoogleDrive$credentials_path)

# Функция для загрузки данных из памяти
upload_from_memory <- function(data, filename, folder_id) {
  # Создаем временный файл в памяти
  temp_file <- tempfile(fileext = ".csv")
  write_csv(data, temp_file)
  
  # Загружаем на Google Drive
  drive_upload(
    media = temp_file,
    path = as_id(folder_id),
    name = filename,
    overwrite = TRUE
  )
  
  # Удаляем временный файл
  unlink(temp_file)
}

# Загружаем данные
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
upload_from_memory(participants, sprintf("members_%s.csv", timestamp), config$GoogleDrive$folder_id)
upload_from_memory(messages, sprintf("messages_%s.csv", timestamp), config$GoogleDrive$folder_id)

cat("Данные успешно загружены на Google Drive без сохранения локальных файлов!\n")