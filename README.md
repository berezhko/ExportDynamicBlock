# Экспорт динамических блоков AutoCAD в YAML

![AutoCAD Logo](https://www.autodesk.com/content/dam/autodesk/www/company/autodesk-logo-primary-black.png)

Этот AutoLISP скрипт позволяет экспортировать параметры динамических блоков из чертежа AutoCAD в файл формата YAML.

## Основные возможности

- Экспорт динамических блоков с фильтрацией по типам
- Сохранение в структурированный YAML-файл
- Включение следующих данных:
  - Базовые параметры блока (имя, координаты, слой)
  - Атрибуты блока
  - Динамические свойства
- Замер времени выполнения операции
- Гибкая фильтрация блоков по категориям

## Установка

1. Загрузите файл `.lsp` в AutoCAD:
   ```
   (load "путь/к/файлу.lsp")
   ```
2. Или добавьте в автозагрузку через `acad.lsp`

## Использование

Запустите команду в AutoCAD:
```
EXPORTDYNAMICBLOCKTOYAML
```

Программа предложит выбрать тип блоков для экспорта:
- Схема
- ЗЗИ
- Трасса
- Все

Результат будет сохранен в файл с именем чертежа и расширением `.yaml` в той же папке.

## Формат выходного файла

Пример структуры YAML-файла:
```yaml
- Handle: '1A2B'
  Block Name: 'КЛЕММА1'
  Real Name: 'КЛЕММА1'
  X: '100.0000'
  Y: '50.0000'
  Z: '0.0000'
  Layer: 'Электрооборудование'
  Attribs:
    TAG1: 'Значение1'
    TAG2: 'Значение2'
  Properties:
    Prop1: '100'
    Prop2: 'TRUE'
```

## Настройки

Глобальные переменные:
- `*type-scheme*` - список блоков для экспорта (устанавливается при выборе типа)

Функции фильтрации:
- `is-my-block` - проверяет, должен ли блок быть экспортирован
- `good-property` - определяет, какие свойства пропускать

## Ограничения

- Работает только с динамическими блоками
- Экспортирует только текстовые атрибуты
- Требуется AutoCAD с поддержкой Visual LISP

## Пример использования

1. Откройте чертеж с динамическими блоками
2. Запустите команду
3. Выберите категорию блоков
4. Получите YAML-файл в папке с чертежом

## Разработчики

[Я и DeepSeek]


## Лицензия
[MIT License](LICENSE)

---

Для более подробной информации см. комментарии в исходном коде.
