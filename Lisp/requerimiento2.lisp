(load "funcionesAux.lisp")


;; ========================================================
;; FUNCIÓN: calcular-timer
;; NATURALEZA: Pura (misma entrada, misma salida)
;; ESTRATEGIA: Invoca otra funcion pasando los valores como parametro
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-timer (tiempo)
  (if (not (integerp tiempo))

      "tiempo ingresado incorrecto"

      (let ((rValor (cdr (obtener-color :rojo)))
            (vValor (cdr (obtener-color :verde)))
            (aValor (cdr (obtener-color :amarillo)))
            (iValor (cdr (obtener-color :intermitente))))

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


;;CASOS DE PRUEBA

;validos:

;invalidos:
(calcular-timer 10.5) ;resultado esperado "tiempo ingresado incorrecto"

(calcular-timer "120") ;resultado esperado "tiempo ingresado incorrecto"

(calcular-timer '(120)) ;resultado esperado "tiempo ingresado incorrecto"
