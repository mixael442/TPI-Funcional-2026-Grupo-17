;;;; ============================================================
;;;; REQUERIMIENTO 5: PLANIFICACIÓN TEMPORAL
;;;; FUNCIÓN: cantidad-ciclos
;;;; Entrada: minutos
;;;; Salida: número de ciclos completos en ese período
;;;; ============================================================
(load "funcionesAux.lisp")

(defun cantidad-ciclos (minutos)

  (if (not (integerp minutos))

      "ingresar un numero valido"

      (floor
       (* minutos 60)

       (+ (cdr (obtener-color :rojo))
          (cdr (obtener-color :verde))
          (cdr (obtener-color :amarillo))
          (* 3 (cdr (obtener-color :intermitente)))))))

;;CASOS DE PRUEBA

;validos:
(cantidad-ciclos 15) ;resultado esperado 4
(cantidad-ciclos 20) ;resultado esperado 5
(cantidad-ciclos 60) ;resultado esperado 16

;invalidos:
(cantidad-ciclos 10.5) ;resultado esperado "ingresar un numero entero"
(cantidad-ciclos "30") ;resultado esperado "ingresar un numero entero"
(cantidad-ciclos '(30)) ;resultado esperado "ingresar un numero entero"
