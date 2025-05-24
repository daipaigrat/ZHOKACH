library(shiny)
library(ini)
library(reticulate)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(tidyr)

ui <- fluidPage(
  titlePanel("Telegram Chat Parser"),
  sidebarLayout(
    sidebarPanel(
      h4("Настройки подключения"),
      passwordInput("api_id", "API ID", ""),
      passwordInput("api_hash", "API Hash", ""),
      textInput("phone", "Номер телефона", ""),
      passwordInput("password", "Пароль (если есть)", ""),
      textInput("target_chat", "Название чата", ""),
      
      h4("Настройки Google Drive"),
      fileInput("service_account", "JSON ключ сервисного аккаунта", accept = ".json"),
      textInput("folder_id", "ID папки на Drive", ""),
      
      h4("Настройки парсера"),
      numericInput("interval_hours", "Периодичность запуска (часы)", value = 24, min = 1, max = 720),
      
      actionButton("save_config", "Сохранить настройки", class = "btn-primary"),
      hr(),
      actionButton("start_btn", "Запуск", class = "btn-success"),
      actionButton("stop_btn", "Остановить", class = "btn-danger"),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Лог выполнения", 
                 verbatimTextOutput("log_output"),
                 tags$style(HTML("
                   #log_output {
                     height: 400px; 
                     overflow-y: scroll;
                     background-color: #f5f5f5;
                     padding: 10px;
                     border: 1px solid #ddd;
                     border-radius: 4px;
                     font-family: monospace;
                     white-space: pre-wrap;
                   }
                 ")))
      )
    )
  )
)

server <- function(input, output, session) {
  # Реактивные значения
  log_messages <- reactiveVal(character(0))
  is_running <- reactiveVal(FALSE)
  timer <- reactiveVal(NULL)
  last_run_time <- reactiveVal(NULL)
  
  # Функция добавления сообщения в лог
  add_log <- function(message) {
    new_entry <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", message)
    log_messages(c(isolate(log_messages()), new_entry))
  }
  
  # Проверка существования файлов дашбордов
  check_dashboard_file <- function(type = "main") {
    file_name <- ifelse(type == "main", "dashboard.html", "dashboard_period.html")
    if (file.exists(file_name)) {
      includeHTML(file_name)
    } else {
      h4("Нет данных", style = "color: gray; text-align: center; margin-top: 100px;")
    }
  }
  
  
  # Вывод лога
  output$log_output <- renderText({
    paste(log_messages(), collapse = "\n")
  })
  
  # Загрузка сохраненных настроек
  observe({
    if (file.exists("config.ini")) {
      config <- read.ini("config.ini")
      updateTextInput(session, "api_id", value = config$Telegram$api_id %||% "")
      updateTextInput(session, "api_hash", value = config$Telegram$api_hash %||% "")
      updateTextInput(session, "phone", value = config$Telegram$phone %||% "")
      updateTextInput(session, "password", value = config$Telegram$password %||% "")
      updateTextInput(session, "target_chat", value = config$Telegram$target_chat %||% "")
      updateTextInput(session, "folder_id", value = config$GoogleDrive$folder_id %||% "")
      updateNumericInput(session, "interval_hours", value = as.numeric(config$Parser$interval_hours %||% 24))
      add_log("Настройки загружены из config.ini")
    }
  })
  
  # Сохранение конфигурации
  observeEvent(input$save_config, {
    config <- list(
      Telegram = list(
        api_id = input$api_id,
        api_hash = input$api_hash,
        phone = input$phone,
        password = input$password,
        target_chat = input$target_chat
      ),
      GoogleDrive = list(
        service_account_json = ifelse(is.null(input$service_account), 
                                      "", 
                                      input$service_account$datapath),
        folder_id = input$folder_id
      ),
      Parser = list(
        interval_hours = input$interval_hours
      )
    )
    write.ini(config, "config.ini")
    add_log("Настройки сохранены в config.ini")
  })
  
  # Основная функция парсера
  run_parser <- function() {
    tryCatch({
      last_run_time(Sys.time())
      config <- read.ini("config.ini")
      
      if (!file.exists(config$GoogleDrive$service_account_json)) {
        stop("Файл сервисного аккаунта Google не найден")
      }
      
      drive_auth(path = config$GoogleDrive$service_account_json)
      add_log("Успешная авторизация в Google Drive")
      
      code <- '
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

async def fetch_data():
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
        "messages": json.dumps(messages_data, ensure_ascii=False)
    }

result = asyncio.run(fetch_data())
'
    
    add_log("Выполнение клиента ...")
    py_run_string(code)
    result <- py$result
    
    # Парсим JSON-данные
    participants <- fromJSON(result$participants)
    messages <- fromJSON(result$messages)
    
    # Обработка данных участников
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
    
    # Обработка сообщений
    if (length(messages) > 0) {
      participant_lookup <- participants %>%
        mutate(
          author_name = ifelse(str_trim(paste(first_name, last_name)) == "", "Нет имени",
                               str_trim(paste(first_name, last_name))),
          author_username = ifelse(username == "", "Нет username", paste0("@", username))
        ) %>%
        select(id, author_name, author_username)
      
      messages_processed <- messages %>%
        mutate(
          from_id = suppressWarnings(as.integer(from_id)),
          Дата = date,
          Текст = ifelse(text == "", "(без текста)", text),
          Длина_текста = text_len,
          Тип_медиа = ifelse(media_type == "text", "", media_type),
          MIME = mime,
          Размер = ifelse(size == "", "", size)
        ) %>%
        left_join(participant_lookup, by = c("from_id" = "id")) %>%
        mutate(
          Автор = coalesce(author_name, "Неизвестный автор"),
          Username = coalesce(author_username, "@неизвестно"),
          Тип = ifelse(media_type == "text", "Текст", "Медиа")
        ) %>%
        select(
          ID = id, 
          Дата, 
          Автор, 
          Username,
          Тип, 
          Текст
        )
      
      # Загрузка на Google Drive
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      upload_file <- function(data, prefix) {
        temp_file <- tempfile(fileext = ".csv")
        write_csv(data, temp_file)
        drive_upload(
          media = temp_file,
          path = as_id(config$GoogleDrive$folder_id),
          name = sprintf("%s_%s.csv", prefix, ''),
          overwrite = TRUE
        )
        unlink(temp_file)
      }
      upload_file(participants_processed, "members_improved")
      upload_file(messages_processed, "chats_improved")
      
      add_log(sprintf("Данные за %s успешно загружены на Google Drive", timestamp))
    } else {
      add_log("Новых сообщений не найдено")
    }
    
    # После успешного выполнения парсера обновляем дашборды
    output$main_dashboard <- renderUI({
      check_dashboard_file("main")
    })
    
    output$period_dashboard <- renderUI({
      check_dashboard_file("period")
    })
    
    return(TRUE)
    }, error = function(e) {
      add_log(paste("ОШИБКА:", e$message))
      return(FALSE)
    })
  }
  
  # Запуск парсера
  observeEvent(input$start_btn, {
    if (!is_running()) {
      is_running(TRUE)
      add_log("Парсер запущен")
      
      # Первый запуск
      run_parser()
      
      # Запускаем периодическое выполнение
      interval_ms <- isolate(input$interval_hours) * 3600000
      timer(observe({
        if (isolate(is_running())) {
          invalidateLater(interval_ms, session)
          run_parser()
          add_log(paste("Следующий запуск через", isolate(input$interval_hours), "часов"))
        }
      }))
    } else {
      add_log("Парсер уже запущен")
    }
  })
  
  # Остановка парсера
  observeEvent(input$stop_btn, {
    if (is_running()) {
      is_running(FALSE)
      timer(NULL)
      add_log("Парсер остановлен")
    } else {
      add_log("Парсер не был запущен")
    }
  })
  
  # Очистка при завершении
  onStop(function() {
    if (is_running()) {
      is_running(FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)