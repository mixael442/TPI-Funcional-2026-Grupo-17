
;FUNCIONES AUXILIARES
;DESCRIPCION: extrae los valores necesarios del archivo config.json
(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-json)

(defun leer-config ()
  (with-open-file (archivo "config.json"
                     :direction :input)
    (json:decode-json archivo)))

(defun obtener-color (clave)
  (cdr (ASSOC clave (leer-config)));par que no devuelva el par, y ahorro una banda de cdrs jsjs
)

; ========================================================
;REQUERIMIENTO 1
; ========================================================
; FUNCIÓN: transicion
; NATURALEZA: Pura
; DESCRIPCIÓN: Simula el cambio de estado de un semáforo. 
;              Valida que el color actual y el color destino sean válidos. 
;              Si alguno es incorrecto, devuelve un mensaje de error.
; IMPACTO: No destructiva
; ========================================================
(defun transicion (color-actual cambiar-a)

  (cond
    ;; Validación del estado actual
    ((not (or (eq color-actual 'en-rojo)
              (eq color-actual 'en-amarillo)
              (eq color-actual 'en-verde)))

     (list 'error "color-actual-invalido"))

    ;; Validación del estado destino
    ((not (or (eq cambiar-a 'rojo)
              (eq cambiar-a 'amarillo)
              (eq cambiar-a 'verde)))

     (list 'error "color-destino-invalido"))
    ;; Transiciones válidas
    ((eq cambiar-a 'rojo)
     (list color-actual "cambiar-a-rojo"))

    ((eq cambiar-a 'amarillo)
     (list color-actual "cambiar-a-amarillo"))

    ((eq cambiar-a 'verde)
     (list color-actual "cambiar-a-verde"))

    (t
     (list 'error "transicion-no-valida"))))

; ========================================================
;REQUERIMIENTO 2
;; ========================================================
;; FUNCIÓN: calcular-timer
;; NATURALEZA: Pura (misma entrada, misma salida)
;; ESTRATEGIA: Invoca otra funcion pasando los valores como parametro
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-timer (tiempo)
  (if (or (not (integerp tiempo)) (not (plusp tiempo)))

      "tiempo ingresado incorrecto"

      (let ((rValor (obtener-color :rojo))
            (vValor (obtener-color :verde))
            (aValor (obtener-color :amarillo))
            (iValor (obtener-color :intermitente)))

        (calcular-rem
         tiempo
         rValor
         vValor
         aValor
         iValor))))

;; ==========================================================
;; FUNCIÓN: calcular-rem
;; NATURALEZA: Pura
;; ESTRATEGIA: Composición de Funciones + operacion aritmetica
;; IMPACTO: No destructiva
;; ==========================================================
(defun calcular-rem (tiempo rojo verde amarillo intermitente)
  (compararRem
    (rem tiempo
         (+ rojo verde amarillo
            (* 3 intermitente)))

    rojo
    verde
    amarillo
    intermitente))

;; ==========================================================
;; FUNCIÓN: compararRem
;; NATURALEZA: Pura
;; ESTRATEGIA: Función Condicional
;; IMPACTO: No destructiva
;; ==========================================================
(defun compararRem (resto rojo verde amarillo intermitente)

  (cond

    ((< resto rojo)
     'en-rojo)

    ((< resto (+ rojo intermitente))
     'rojo-intermitente)

    ((< resto (+ rojo intermitente verde))
     'en-verde)

    ((< resto (+ rojo intermitente verde intermitente))
     'verde-intermitente)

    ((< resto (+ rojo intermitente verde intermitente amarillo))
     'en-amarillo)

    ((< resto (+ rojo
                 intermitente
                 verde
                 intermitente
                 amarillo
                 intermitente))
     'amarillo-intermitente)

    (t
     "error")))

;AUDITORÍA - REQUERIMIENTO 3

;FUNCION: registrar-evento
;NATURALEZA: Pura
;DESCRIPCIÓN:
;Agrega un nuevo evento al historial y devuelve
;un historial actualizado.
;IMPACTO: no destructiva
(defun registrar-evento
       (timestamp estado-anterior estado-nuevo historial)

  (cons
   (list timestamp estado-anterior estado-nuevo)
   historial))


;FUNCION: mostrar-evento
;NATURALEZA: Impura
;DESCRIPCIÓN:
;Muestra un único evento por pantalla.
;IMPACTO: no destructiva
(defun mostrar-evento (evento)

  (format t
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          (car evento)
          (cadr evento)
          (caddr evento)))


;FUNCION: mostrar-historial
;NATURALEZA: Impura
;DESCRIPCIÓN:
;Recorre recursivamente el historial y muestra cada evento.
;IMPACTO: no destructiva
(defun mostrar-historial (historial)

  (cond
    ((null historial) nil)

    (t
     (mostrar-evento (car historial))
     (mostrar-historial (cdr historial)))))


;FUNCION: evento-a-texto
;NATURALEZA: Pura
;DESCRIPCIÓN:
;Convierte un evento en una cadena de texto.
;IMPACTO: no destructiva
(defun evento-a-texto (evento)

  (format nil
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          (car evento)
          (cadr evento)
          (caddr evento)))


;FUNCION: historial-a-texto
;NATURALEZA: Pura
;DESCRIPCIÓN:
;Convierte todo el historial en una cadena de texto.
;IMPACTO: no destructiva
(defun historial-a-texto (historial)

  (cond
    ((null historial) "")

    (t
     (concatenate
      'string
      (evento-a-texto (car historial))
      (historial-a-texto (cdr historial))))))


;FUNCION: guardar-informe
;NATURALEZA: Impura
;DESCRIPCIÓN:
;Genera un archivo de texto con el informe de auditoría.
;IMPACTO:  no destructiva
(defun guardar-informe (historial)

  (with-open-file
      (archivo
       "informe-ejecucion-semaforo.txt"
       :direction :output
       :if-exists :append; o supersede si se decea eliminar el inf ant
       :if-does-not-exist :create)
       (terpri archivo);para que no se pengen los archivos angau

    (write-string
     (informe historial)
     archivo)))


;FUNCION: informe
;NATURALEZA: Pura
;DESCRIPCIÓN:
;Genera el informe completo como una cadena de texto.
;IMPACTO: no destructiva
(defun informe (historial)

  (concatenate
   'string
   "Informe de Ejecución del Sistema Semafórico

"
   (historial-a-texto (reverse historial))
   "
- Fin del Informe -"))

;; ========================================================
;; REQUERIMIENTO 4
;; ========================================================
;; FUNCIÓN: duracion
;; NATURALEZA: Pura (no produce efectos secundarios)
;; ESTRATEGIA: Operacion aritmética (suma)
;; IMPACTO: No destructiva 
;; ========================================================
(defun duracion()

  (let ((rojo (obtener-color :rojo))
        (verde (obtener-color :verde))
        (amarillo (obtener-color :amarillo))
        (intermitente (obtener-color :intermitente)))

    (+ rojo
       intermitente
       verde
       intermitente
       amarillo
       intermitente)));fin 
;; ========================================================
;; FUNCIÓN: recomendacion-ciclo 
;; NATURALEZA: Pura (misma entrada, misma salida)
;; ESTRATEGIA: Condicional (evalua distintos casos para una entrada)
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
    (cond ((not (numberp duracion)) (error "Tipo de dato incorrecto. Debe ser numérico"))
          ((not (plusp duracion)) (error "El numero debe ser mayor o igual a 0 (positivo)")) ;tambien puede ser (< ciclo-seg 0) 
          (t 
             (cond ((> duracion 150) "Ciclo demasiado largo")
                   ((< duracion 35) "Ciclo demasiado corto")
                   (t "Ciclo optimo")
             )));fin cond
);fin

;; ========================================================
;;;; REQUERIMIENTO 5: PLANIFICACIÓN TEMPORAL
;;;; FUNCIÓN: cantidad-ciclos
;;;; Entrada: minutos
;;;; Salida: número de ciclos completos en ese período
;;;; ============================================================
(defun cantidad-ciclos (minutos)

  (if (not (integerp minutos))

      "ingresar un numero entero"

      (floor
       (* minutos 60)

       (+  (obtener-color :rojo)
           (obtener-color :verde)
           (obtener-color :amarillo)
          (* 3 (obtener-color :intermitente))))))

;TIMER - REQUERIMIENTO 6
;FUNCIÓN: porcentaje-color
; NATURALEZA: Pura.
; ESTRATEGIA: Composición funcional y operaciones aritméticas.
; IMPACTO: No destructiva
(defun porcentaje-color ()
    (let* ((rojo (+ (obtener-color :rojo)
                  (obtener-color :intermitente)))
         (verde (+ (obtener-color :verde)
                   (obtener-color :intermitente)))
         (amarillo (+  (obtener-color :amarillo)
                       (obtener-color :intermitente)))
         (ciclo (+ rojo verde amarillo))
         (ciclos (floor 3600 ciclo))
         (resto (rem 3600 ciclo)))
          
          (list

     (cons 'rojo
           (calcular
            rojo
            (calcular-rest resto rojo verde amarillo 'rojo)
            ciclos))
     (cons 'verde
           (calcular
            verde
            (calcular-rest resto rojo verde amarillo 'verde)
            ciclos))
     (cons 'amarillo
           (calcular
            amarillo
            (calcular-rest resto rojo verde amarillo 'amarillo)
            ciclos))
     )))

; FUNCIÓN: calcular-rest
; NATURALEZA: Pura
; ESTRATEGIA: Condicional mediante evaluación de casos.
; IMPACTO: No destructiva
(defun calcular-rest (resto rojo verde amarillo actual)
    (cond
        ((equal actual 'rojo)(min resto rojo))
        ((equal actual 'verde)(max (min (- resto rojo) verde) 0))
        ((equal actual 'amarillo)(max (min (- resto rojo verde) amarillo) 0))
        (t 0)
    )
)
; FUNCIÓN: calcular
; NATURALEZA: Pura
; ESTRATEGIA: Operación aritmética.
; IMPACTO: No destructiva
(defun calcular (color resto ciclos)
    (* 100.0 (/ (+ (* color ciclos) resto) 3600))
)


(defun mostrar-menu ()

  (format t "~%")
  (format t " SISTEMA DE SEMAFOROS INTELIGENTES~%")
  (format t "1 - Probar transicion~%")
  (format t "2 - Consultar timer~%")
  (format t "3 - Mostrar historial~%")
  (format t "4 - Duracion de ciclo~%")
  (format t "5 - Recomendacion de ciclo~%")
  (format t "6 - Ciclos por tiempo~%")
  (format t "0 - Salir~%")
  (format t "Seleccione una opcion: ")
  (terpri)
  (read))

(defun ejecutar-menu ()
  (let ((opcion (mostrar-menu)))
    (cond
      ; SALIR
      ((= opcion 0)
       (format t "~%Finalizando programa...~%"))
; REQ 1
      ((= opcion 1)
       (format t "~%Color actual: ")(finish-output)
       (let ((actual (read)))
         (format t "Cambiar a: ")(finish-output)
         (let ((nuevo (read)))
           (format t "~A~%"
                   (transicion actual nuevo))))
       (ejecutar-menu))
; REQ 2
      ((= opcion 2)
       (format t "~%Tiempo Actual en Unix (ej. 1781450000): ")
       (let ((tiempo (read)))
         (format t "Semaforo activo: ~A~%"
                 (calcular-timer tiempo)))
       (ejecutar-menu))
; REQ 3
      ((= opcion 3)
       (format t "~%Ingrese variable historial: ")
       (let ((historial (read)))
         (mostrar-historial historial))
       (ejecutar-menu))
; REQ 4
      ((= opcion 4)
       (format t "~%Duracion ciclo: ~A segundos~%~A~%"
               (duracion)
               (recomendacion-ciclo (duracion)))
       (format t "~%Presione ENTER para continuar...")
       (read-line)
       (ejecutar-menu))
 ;REQ 5
      ((= opcion 5)
       (format t "~%Minutos: ")
       (let ((minuto (read)))
        (format t "~%Cantidad de ciclos en ~A minutos: ~A~%"
                    minuto
                  (cantidad-ciclos minuto)))
       (ejecutar-menu))
; REQ 6
      ((= opcion 6)
       (format t "~%Porcentajes de cada color: ~A~%" (porcentaje-color))
       (format t "~%Presione ENTER para continuar...")
       (read-line)
       (ejecutar-menu))
      ;; ERROR
      (t
       (format t "~%Opcion invalida~%")
       (ejecutar-menu))
    )
  )
)
(ejecutar-menu)


