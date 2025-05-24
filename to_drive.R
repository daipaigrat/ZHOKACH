library(googlesheets4)
library(readr)
library(googledrive)

google_key = "C:/Users/Admin/Desktop/Uni/threathunting/tgadmin-457821_key.json"
folder_id <- "1Xdj3qgYVN0ejUS-aPFJyaOdN17dPludj"

drive_auth(path = google_key)

# 1. Функция для сохранения/перезаписи файла
save_to_drive <- function(data, filename, folder_id) {
  if (!is.data.frame(data)) {
    stop("Данные должны быть в формате data.frame")
  }
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(data, temp_file)
  
  # Проверяем существование файла
  existing_files <- drive_ls(path = as_id(folder_id), pattern = filename)
  
  if (nrow(existing_files) > 0) {
    # Если файл существует - обновляем
    if (nrow(existing_files) > 1) {
      warning("Найдено несколько файлов с таким именем. Используется первый.")
    }
    drive_update(file = as_id(existing_files$id[1]), media = temp_file)
    message("Файл '", filename, "' успешно обновлен")
  } else {
    # Если файла нет - создаем новый
    drive_upload(temp_file, path = as_id(folder_id), name = filename)
    message("Файл '", filename, "' успешно создан")
  }
  
  unlink(temp_file)
}

# 2. Функция для добавления данных в файл
append_to_drive <- function(new_data, filename, folder_id) {
  if (!is.data.frame(new_data)) {
    stop("Данные должны быть в формате data.frame")
  }
  
  temp_file <- tempfile(fileext = ".csv")
  
  # Проверяем существование файла
  existing_files <- drive_ls(path = as_id(folder_id), pattern = filename)
  
  if (nrow(existing_files) == 0) {
    # Если файла нет - создаем новый (аналогично save_to_drive)
    write_csv(new_data, temp_file)
    drive_upload(temp_file, path = as_id(folder_id), name = filename)
    message("Файл '", filename, "' не найден. Создан новый с ", nrow(new_data), " строками")
  } else {
    # Если файл существует
    if (nrow(existing_files) > 1) {
      warning("Найдено несколько файлов с таким именем. Используется первый.")
    }
    
    # Загружаем существующие данные
    drive_download(file = as_id(existing_files$id[1]), path = temp_file, overwrite = TRUE)
    existing_data <- read_csv(temp_file, col_types = cols(.default = "c"))
    
    # Проверяем совместимость структур
    if (!setequal(names(existing_data), names(new_data))) {
      stop("Структура столбцов в новых данных не совпадает с существующим файлом")
    }
    
    # Приводим типы и объединяем
    new_data <- mutate_all(new_data, as.character)
    combined_data <- bind_rows(existing_data, new_data)
    write_csv(combined_data, temp_file)
    
    # Обновляем файл на Drive
    drive_update(file = as_id(existing_files$id[1]), media = temp_file)
    message("Успешно добавлено ", nrow(new_data), " строк в файл '", filename, "'")
  }
  
  unlink(temp_file)
}

# 3. Функция для чтения файла
read_from_drive <- function(filename, folder_id) {
  existing_files <- drive_ls(path = as_id(folder_id), pattern = filename)
  
  if (nrow(existing_files) == 0) {
    stop("Файл '", filename, "' не найден")
  }
  
  if (nrow(existing_files) > 1) {
    warning("Найдено несколько файлов с таким именем. Используется первый.")
  }
  
  temp_file <- tempfile(fileext = ".csv")
  drive_download(file = as_id(existing_files$id[1]), path = temp_file)
  
  #data <- read_csv(temp_file)
  #unlink(temp_file)
  
  return(temp_file)
}

d <- read_from_drive("members_20250524_033653.csv", folder_id)

