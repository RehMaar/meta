* fc_intrp.rkt
  Содержит интерпретатор _FlowChart_.
  - Запуск интерпретатора: `(intrp program input)`

* tm_intrp.rkt
  Содержит интерпретатор _Turing Machine_.
  - Запуск интерпретатора: `(intrp tm-intrp (list prog init-tape))`
  - Более удобный запуск интерпретатора: `(run-tm prog init-tape)`
  - Более удобный запуск интерпретатора
    c более понятным отображением результата: `(run-tm-display prog init-tape)`

* fc_mix0.rkt
  Содержит реализацию mix-алгоритма с сжатием переходов.
  - Запуск: `(run-mix0 args)`, где `args` -- это список
    вида `(program div vs0)`.
  - Pretty-print: `(pretty-print fc-program)`.

* fc_fproj.rkt
  Содержит "проекции Футамуры".
  - Запустить вычисления первой проекции Футамуры для интерпретатора TM: `(fc-mix-fp1 div vs0)`.
  - Запустить тест для TM-программы из заданий `fc-mix-fp1-test`.
  - Запустить тест для TM-программы из заданий с pretty-print'ом `fc-mix-fp1-test-pp`.
