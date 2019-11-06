Результаты
----------

Файлы `generated_fc_fp2.txt` и `generated_tm_fp2.rkt` содержат
результат пятого задания cоответственно для _FlowChart_ и _TM_.

Файлы `generated_fc_fp3.txt` и `generated_tm_fp3.rkt` содержат
результат шестого задания -- компиляторы для _FlowChart_ и _TM_
 cоответственно.

Структура проекта
-----------------

* `fc_tricky_mix.rkt`
  Содержит mix с трюком и оригинальный mix-алгоритм.
  - Микс с трюком: `fc-mix-new`
  - Его запуск: `(run-mix-new args)`
  - Соответственно, для оригинального: `fc-mix` и `run-mix`. 

* `fc_intrp_in_fc.rkt`
 Интерпретатор _FlowChart_ на _FlowChart_.

* `fc_tricky_fproj.rkt`
  Содержит все проекции.
  - Задание 4
    * Первая проекция: `fc-mix-fp1-new`
    * Вторая проекция: `fc-mix-fp2-tm`
  - Задание 5
    * Первая проекция: `fc-mix-fp1-fc-new`
    * Вторая проекция: `fc-mix-fp2-fc-new`
  - Задание 6
    * Третья проекция: `fc3`
    * Остальные проекции ниже по коду.

* `generated_fc3.rkt`
  Предподсчитанный `fc3`. Используется для тестов.

* `fc_intrp.rkt`
  Содержит интерпретатор _FlowChart_.
  - Запуск интерпретатора: `(intrp program input)`

* `tm_intrp.rkt`
  Содержит интерпретатор _Turing Machine_.
  - Запуск интерпретатора: `(intrp tm-intrp (list prog init-tape))`
  - Более удобный запуск интерпретатора: `(run-tm prog init-tape)`
  - Более удобный запуск интерпретатора
    c более понятным отображением результата: `(run-tm-display prog init-tape)`

* `fc_mix.rkt`
  Содержит реализацию mix-алгоритма с сжатием переходов.
  - Запуск: `(run-mix args)`, где `args` -- это список
    вида `(program div vs0)`.
  - Pretty-print: `(pretty-print fc-program)`.

* `fc_fproj.rkt`
  Содержит "проекции Футамуры".
  - Запустить вычисления первой проекции Футамуры для интерпретатора TM: `(fc-mix-fp1 div vs0)`.
  - Запустить тест для TM-программы из заданий `fc-mix-fp1-test`.
  - Запустить тест для TM-программы из заданий с pretty-print'ом `fc-mix-fp1-test-pp`.
