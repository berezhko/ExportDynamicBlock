;; 
;; Windows-1251
;;

(defun ExportDynamicBlockToyaml (/ filePath)
  ;; Главная функция
  (setq filePath (strcat (getvar "dwgprefix") (vl-filename-base (getvar "DWGNAME")) ".yaml"))
  (if (export-blocks-to-yaml filePath)
      (princ (strcat "\nДанные сохранены в файл: " filePath))
      (princ "\nВ чертеже нет блоков или ошибка экспорта.")
  )
  (princ)
)

(defun c:ExportDynamicBlockToyaml (/ start end)

  ;; Инициализируем глобальную переменную (если еще не существует)
  (if (not *type-scheme*)
    (setq *type-scheme* nil)
  )
  ;; Предлагаем пользователю выбрать вариант
  (initget 1 "1 Схема 2 ЗЗИ 3 Трасса 4 Все") ; 1 - запрещаем пустой ввод
  (setq userChoice (getkword "\nВыберите тип данных для инициализации [Схема/ЗЗИ/Трасса/Все]: "))
  ;; Инициализируем массив в зависимости от выбора
  (cond
    ((= userChoice "Схема")
      (setq *type-scheme*
        '("КЛЕММА1" "КЛЕММА2" "КЛЕММА1_2КАБ" "КЛЕММА2_2КАБ" "КЛЕММА_ВН1" "КЛЕММА_ВН2" "КАБЕЛЬ3" "Устройство" "REF")
      )
	)
    ((= userChoice "ЗЗИ")
      (setq *type-scheme*
        '("КОРОБ" "КОНТАКТ" "БЛОК" "ВЫНОСКА")
      )
	)
    ((= userChoice "Трасса")
      (setq *type-scheme*
        '("КОРОБ" "ШКАФ")
      )
	)
    ((= userChoice "Все")
      (setq *type-scheme*
        '()
      )
	)
    (t (setq *type-scheme* '() ))
  )
  (princ *type-scheme*)

  (setq start (getvar "DATE"))  ; Получаем начальное время
  
  ;; Код, время выполнения которого нужно измерить
  (ExportDynamicBlockToyaml)
  
  (setq end (getvar "DATE"))  ; Получаем конечное время
  (princ (strcat "\nВремя выполнения: " (rtos (* (- end start) 86400) 2 4) " сек."))
  (princ)
)

;; ================================================
;; Глобальные условия программы
;; ================================================
(defun is-my-block (ent)
  (if *type-scheme*
    (member (get-realName ent) *type-scheme*)
	t
  )
)

(defun print-debug (str)
  (if f (princ str))
)

(defun good-property (prop)
  (setq skip-list-property '("Origin"))
  (not (member (get-nameProp prop) skip-list-property))
)

;; ================================================
;; Вспомогательные функции
;; ================================================
(defun export-blocks-to-yaml (yamlPath / ss yamlFile success)
  (setq success nil)
  (if (setq ss (get-all-blocks))
    (progn
      (setq yamlFile (open-yaml-file yamlPath))
      (if (file-ready-for-export yamlFile)
        (progn
          (initialize-export)
          (process-blocks ss yamlFile)
          (finalize-export yamlFile)
          (setq success t)
        )
        (handle-file-error)
      )
    )
  )
  success
)

(defun get-all-blocks ()
  ;; Получаем все блоки в чертеже
  (ssget "_X" '((0 . "INSERT")))
)

(defun open-yaml-file (path)
  ;; Открываем файл для записи с обработкой ошибок
  (if (findfile path)
    (vl-file-delete path)
  )
  (open path "w")
)

(defun file-ready-for-export (fileObj)
  ;; Проверяем доступность файла для записи
  (and fileObj (eq (type fileObj) 'FILE))
)

(defun initialize-export ()
  ;; Инициализация экспорта
  (princ "\nНачало экспорта данных в файл...")
)

(defun export-block (ent yamlFile)
  (print-debug (strcat "\n" (itoa (- (sslength ss) i)) "/" (itoa (sslength ss)) " Обработка динамического блока: " (get-realName ent)))
  (export-block-commonData ent yamlFile)
  (print-debug " Общая часть [Ок],")
  (export-block-attribs ent yamlFile)
  (print-debug " Атрибуты [Ок],")
  (export-block-properties ent yamlFile)
  (print-debug " Свойства [Ок]")
)

(defun process-blocks (ss yamlFile / i ent)
  ;; Обрабатываем все блоки в наборе
  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (if (and (is-dynamic-block ent) (is-my-block ent))
      (export-block ent yamlFile)
      (princ (strcat "\nПропускаем блок: " (get-name ent) "/" (get-realName ent)))
    )
  )
)

;; Основные функции получения данных блока
(defun get-block-insertion-point (ent / blkObj)
 (setq blkObj (vlax-ename->vla-object ent))
 (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint blkObj)))
)

(defun get-block-X (ent)
  (rtos (nth 0 (get-block-insertion-point ent)) 2 4)
)

(defun get-block-Y (ent)
  (rtos (nth 1 (get-block-insertion-point ent)) 2 4)
)

(defun get-block-Z (ent)
  (rtos (nth 2 (get-block-insertion-point ent)) 2 4)
)

(defun get-block-rotation (ent)
  (rtos (vla-get-Rotation (vlax-ename->vla-object ent)) 2 4)
)

(defun get-block-handle (ent)
  (vla-get-Handle (vlax-ename->vla-object ent))
)

(defun get-block-effective-name (ent)
  (vla-get-EffectiveName (vlax-ename->vla-object ent))
)

(defun get-realName (ent)
  (if (vl-string-search "*U" (get-name ent))
    (get-block-effective-name ent)
    (get-name ent)
  )
)

(defun get-name (ent)
  (cdr (assoc 2 (entget ent)))
)

(defun get-block-layer (ent)
  (vla-get-Layer (vlax-ename->vla-object ent))
)

(defun get-nameAttr (att)
  (vla-get-TagString att)
)

(defun get-valueAttr (att)
  (vla-get-TextString att)
)

(defun get-nameProp (prop)
  (vla-get-PropertyName prop)
)

(defun get-valueProp (prop)
  (vl-princ-to-string (vlax-get prop 'Value))
)

(defun get-attValues (ent)
  ;; Собираем значения атрибутов
  (vlax-invoke (vlax-ename->vla-object ent) "GetAttributes")
)

(defun get-propValues (ent)
  ;; Получаем все динамические свойства
  (vlax-invoke (vlax-ename->vla-object ent) "GetDynamicBlockProperties")
)

(defun yaml-line (tag val)
  (strcat "    " tag ": '" val "'")
)

(defun export-block-commonData (ent yamlFile)
  (write-line
    (strcat
      "- Handle: '" (get-block-handle ent) "'\n"
      "  Block Name: '" (get-name ent) "'\n"
      "  Real Name: '" (get-realName ent) "'\n"
      "  X: '" (get-block-X ent) "'\n"
      "  Y: '" (get-block-Y ent) "'\n"
      "  Z: '" (get-block-Z ent) "'\n"
      "  Layer: '" (get-block-layer ent) "'"
    )
  yamlFile
  )
)

(defun export-block-attribs (ent yamlFile)
  (write-line "  Attribs:" yamlFile)
  ;; Экспорт атрибутов блока
  (foreach att (get-attValues ent)
    (write-line (yaml-line (get-nameAttr att) (get-valueAttr att)) yamlFile)
  )
)

(defun export-block-properties (ent yamlFile)
  (write-line "  Properties:" yamlFile)
  ;; Экспорт свойства блока
  (foreach prop (get-propValues ent)
    (if (good-property prop)
      (write-line (yaml-line (get-nameProp prop) (get-valueProp prop)) yamlFile)
    )
  )
)

(defun is-dynamic-block (ent)
  ;; Проверяем, является ли блок динамическим
  (and (vlax-property-available-p (vlax-ename->vla-object ent) "IsDynamicBlock")
       (vlax-get-property (vlax-ename->vla-object ent) "IsDynamicBlock"))
)

(defun finalize-export (yamlFile)
  ;; Завершение экспорта
  (princ "\nЭкспорт данных завершен.")
  (close yamlFile)
)

(defun handle-file-error ()
  ;; Обработка ошибок файла
  (princ "\nОшибка: невозможно создать файл для экспорта!")
)