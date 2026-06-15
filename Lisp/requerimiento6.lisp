(load "funcionesAux.lisp")


; FUNCIÓN: porcentaje-color
; NATURALEZA: Pura.
; ESTRATEGIA: Composición funcional y operaciones aritméticas.
; IMPACTO: No destructiva

(defun porcentaje-color ()
    (let* ((rojo (+ (cdr (obtenerColor :rojo))
                  (cdr (obtenerColor :intermitente))))
         (verde (+ (cdr (obtenerColor :verde))
                   (cdr (obtenerColor :intermitente))))
         (amarillo (+ (cdr (obtenerColor :amarillo))
                      (cdr (obtenerColor :intermitente))))
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

;CASOS DE PRUEBA
;no necesita casos de prueba debido a que no se ingresan datos