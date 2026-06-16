-- ==========================================================
-- FUNCIÓN: calcuTimer
-- NATURALEZA: Pura
-- ESTRATEGIA: Composición de Funciones, Delega el cálculo a calcuTiempo
-- IMPACTO: No Destructiva
-- ==========================================================
calcuTimer :: Int -> String
calcuTimer tiempo = calcuTiempo tiempo 90 6 120 3

-- ==========================================================
-- FUNCIÓN: calcuTiempo
-- NATURALEZA: Pura
-- ESTRATEGIA: Composición de Funciones + operaciones aritmeticas
-- IMPACTO: No Destructiva
-- ==========================================================
calcuTiempo :: Int -> Int -> Int -> Int -> Int -> [Char]
calcuTiempo tiempo rojo amarillo verde interm = compararRem (mod tiempo (rojo + amarillo + verde + (3 * interm))) rojo verde amarillo interm

-- ==========================================================
-- FUNCIÓN: compararRem
-- NATURALEZA: Pura
-- ESTRATEGIA: Función condicional mediante guards
-- IMPACTO: No Destructiva
-- ==========================================================
compararRem :: Int -> Int -> Int -> Int -> Int -> String
compararRem resto rojo verde amarillo interm
  | resto < rojo = "en-rojo"
  | resto < (rojo + interm) = "en-rojo-intermitente"
  | resto < (rojo + interm + verde) = "en-verde"
  | resto < (rojo + interm + verde + interm) = "en-verde-intermitente"
  | resto < (rojo + interm + verde + interm + amarillo) = "en-amarillo"
  | resto < (rojo + interm + verde + interm + amarillo + interm) = "en-amarillo-intermitente"
  | otherwise = "error"

transicion :: String -> String -> [String]
transicion colorActual cambiarA
  -- Validación del color actual
  | colorActual /= "en-rojo"
      && colorActual /= "en-amarillo"
      && colorActual /= "en-verde"
      && colorActual /= "en-rojo-intermitente"
      && colorActual /= "en-amarillo-intermitente"
      && colorActual /= "en-verde-intermitente" =
      ["ERROR: color actual invalido"]
  -- Validación del color destino

  | cambiarA /= "en-rojo"
      && cambiarA /= "en-amarillo"
      && cambiarA /= "en-verde"
      && cambiarA /= "en-rojo-intermitente"
      && cambiarA /= "en-amarillo-intermitente"
      && cambiarA /= "en-verde-intermitente" =
      ["ERROR: color destino invalido"]
  -- Transiciones intermitentes

  | colorActual == "en-rojo"
      && cambiarA == "en-rojo-intermitente" =
      [colorActual, "activar-rojo-intermitente"]
  | colorActual == "en-rojo-intermitente"
      && cambiarA == "en-verde" =
      [colorActual, "cambiar-a-verde"]
  | colorActual == "en-verde"
      && cambiarA == "en-verde-intermitente" =
      [colorActual, "activar-verde-intermitente"]
  | colorActual == "en-verde-intermitente"
      && cambiarA == "en-amarillo" =
      [colorActual, "cambiar-a-amarillo"]
  | colorActual == "en-amarillo"
      && cambiarA == "en-amarillo-intermitente" =
      [colorActual, "activar-amarillo-intermitente"]
  | colorActual == "en-amarillo-intermitente"
      && cambiarA == "en-rojo" =
      [colorActual, "cambiar-a-rojo"]
  -- Transiciones normales

  | cambiarA == "en-rojo" =
      [colorActual, "cambiar-a-rojo"]
  | cambiarA == "en-amarillo" =
      [colorActual, "cambiar-a-amarillo"]
  | cambiarA == "en-verde" =
      [colorActual, "cambiar-a-verde"]
  | otherwise =
      [colorActual, "accion-por-defecto"]
