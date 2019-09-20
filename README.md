* fc_intrp.rkt
  Содержит интерпретатор _FlowChart_.
  - Запуск интерпретатора: `(intrp programm input)`

* tm_intrp.rkt
  Содержит интерпретатор _Turing Machine_.
  - Запуск интерпретатора: `(intrp tm-intrp (list prog init-tape))`
  - Более удобный запуск интерпретатора: `(run-tm prog init-tape)`
  - Более удобный запуск интерпретатора
    c более понятным отображением результата: `(run-tm-display prog init-tape)`
