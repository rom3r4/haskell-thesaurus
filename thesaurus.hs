module Main (Main.main) where

import Data.Maybe
import Graphics.UI.Gtk
import Char
import IO
import Graphics.UI.Gtk.Mogul
import Graphics.UI.Gtk.Glade


import Maybe (isJust, fromJust)
import Monad (when)
import List  (unfoldr, intersperse)
import System (getArgs)
import Data.IORef



-- ///////////////////////////////////////////////////////////////////////////
-- ///////////////////////////////////////////////////////////////////////////
-- // INTERFAZ: MAIN**********************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////



main :: IO ()
main = do
  initGUI


  almacen <- textBufferNew Nothing
  almacen2 <- textBufferNew Nothing
  
  

  
-- ///////////////////////////////////////////////////////////////////////////
-- // INICIO DE INTERFAZ******************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////



  dialogXmlM <- xmlNew "teshaurus.glade"
  let dialogXml = case dialogXmlM of
        (Just dialogXml) -> dialogXml
        Nothing -> error $ "can't find the glade file \"Teshaurus.glade\""
                        ++ "in the current directory"

  -- get a handle on a various objects from the glade file
  mainWindow <- xmlGetWidget dialogXml castToWindow "mainWindow"

  mainWindow `onDestroy` mainQuit

  mainView <- xmlGetWidget dialogXml castToTreeView "mainView"

  almacen_texto <- xmlGetWidget dialogXml castToTextView "almacen_texto"
  titleLabel <- xmlGetWidget dialogXml castToLabel "titleLabel"
  commandLabel <- xmlGetWidget dialogXml castToLabel "commandLabel"
  
  statusbar <- xmlGetWidget dialogXml castToStatusbar "statusbar"
  ctx <- statusbarGetContextId statusbar "state"
  statusbarPush statusbar ctx "Programa inicializado correctamente."
  

  
  
  
-- ///////////////////////////////////////////////////////////////////////////
-- // AGREGAR DICCIONARIO*****************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////

--añade un diccionario de palabras al diccionario actual 


  openMenuItem <- xmlGetWidget dialogXml castToMenuItem "openMenuItem"
  openMenuItem `onActivateLeaf` do
	dialog <- fileChooserDialogNew
			(Just "Abrir Diccionario... ")
			(Just mainWindow)
			FileChooserActionOpen
			[("gtk-cancel", ResponseCancel)
			,("gtk-open", ResponseAccept)]
	widgetShow dialog
	response <- dialogRun dialog
	widgetHide dialog
	case response of
	    ResponseAccept -> 
			do
				Just direccionAdd <- fileChooserGetFilename dialog
				diccionario1 <- carga_diccionario almacen2
				archivoAdd <- openFile direccionAdd ReadMode
				tex <- labelGetText commandLabel
				--putStrLn tex
				commandLabel`labelSetLabel` ("'"++direccionAdd++"'"++ valor_n tex)
				diccionario2 <- agregar_dicc archivoAdd []
				hClose archivoAdd
				escribe_almacen (diccionarios (juntar_diccs (carg_dicc diccionario1) (carg_dicc diccionario2))) almacen2
				widgetDestroy dialog	
				dia2 <- dialogNew
				windowSetPosition dia2 WinPosCenter
				windowSetDefaultSize dia2 250 150
				windowSetTitle dia2 "Diccionario agregado"
				dialogAddButton dia2 "Aceptar" ResponseYes
				label <- labelNew (Just "Diccionario correctamente agregado")
				contain <- dialogGetUpper dia2
				boxPackStartDefaults contain label
				widgetShowAll dia2
				res <- dialogRun dia2
				statusbarPush statusbar ctx "Dicionario agregado correctamente."
				widgetDestroy dia2
				return ()
	    _ -> 
			do
				statusbarPush statusbar ctx "Agregacion de diccionario cancelada."	
				widgetDestroy dialog
				return ()
				
				
-- ///////////////////////////////////////////////////////////////////////////
-- // ELIMINAR DICCIONARIO*****************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////

--Reseteamos el diccionario actual acumulado, de forma que la siguiente operacion
--de mostrar diccionario actual mostrará el diccionario vacío

  deleteMenuItem <- xmlGetWidget dialogXml castToMenuItem "deleteMenuItem"
  deleteMenuItem `onActivateLeaf` do
  	vExit <- windowNew
	windowSetPosition vExit WinPosCenter
	texto <- labelNewWithMnemonic "\n¿ Esta seguro que desea eliminar \nel diccionario actual ?\n"
	botonSi <- buttonNew
	set botonSi [ buttonLabel := "Si" ]
	onClicked botonSi $ do
		commandLabel`labelSetLabel` ("<< No Introducido >>")
		statusbarPush statusbar ctx "Dicionario eliminado correctamente."
		escribe_almacen (diccionarios (juntar_diccs (carg_dicc []) (carg_dicc []))) almacen2
		widgetDestroy vExit
	botonNo <- buttonNew
	set botonNo [ buttonLabel := "No" ]
	onClicked botonNo $ do
		statusbarPush statusbar ctx "Cancelado eliminar diccionario."
		widgetDestroy vExit
	contenedorExit <- hButtonBoxNew
	boxPackStartDefaults contenedorExit botonSi
	boxPackStartDefaults contenedorExit botonNo
	divExit <- vPanedNew
	panedSetPosition divExit 5
	panedPack1 divExit texto False False
	panedPack2 divExit contenedorExit False False
	set vExit [ containerChild := divExit, windowTitle := "Eliminar diccionario" ]
	windowSetDefaultSize vExit 200 50
	widgetShowAll vExit

-- ///////////////////////////////////////////////////////////////////////////
-- // DEFINIR PALABRA*********************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////

--muestra un dialogo en el cual se pide introducir la palabra que se quiere analizar

  definir_palabra <- xmlGetWidget dialogXml castToMenuItem "definir_palabra"
  definir_palabra `onActivateLeaf` do
		dia <- dialogNew
		windowSetTitle dia "Definir Palabra"
		dialogAddButton dia "Aceptar" ResponseYes
		dialogAddButton dia "Cancelar"  ResponseNo
		label <- labelNew (Just "Introduzca la palabra que \nse quiere verificar:")
		input <- entryNew
		contain <- dialogGetUpper dia
		boxPackStartDefaults contain label
		boxPackStartDefaults contain input
		windowSetPosition dia WinPosCenter
		widgetShowAll dia
		res <- dialogRun dia
		name <- entryGetText input
		widgetDestroy dia
		case res of 
			ResponseYes -> do
				titleLabel`labelSetLabel` (name)
				textBufferSetText almacen name
				statusbarPush statusbar ctx "Palabra definida correctamente." 
				return ()
			_ -> do
				statusbarPush statusbar ctx "Definicion de palabra cancelada."	
				return ()

	
	
	
-- ///////////////////////////////////////////////////////////////////////////
-- // AGREGAR PALABRA*********************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////


--para agregar directamente una palabra al diccionario actual de palabras

  agregar_palabra <- xmlGetWidget dialogXml castToMenuItem "agregar_palabra"
  agregar_palabra `onActivateLeaf` do
		dia <- dialogNew
		windowSetTitle dia "Agregar Palabra"
		dialogAddButton dia "Aceptar" ResponseYes
		dialogAddButton dia "Cancelar"  ResponseNo
		label <- labelNew (Just "Introduzca la palabra que se \nquiere agregar al diccionario:")
		input <- entryNew
		contain <- dialogGetUpper dia
		boxPackStartDefaults contain label
		boxPackStartDefaults contain input
		windowSetPosition dia WinPosCenter
		widgetShowAll dia
		res <- dialogRun dia
		name <- entryGetText input
		widgetDestroy dia
		case res of 
			ResponseYes -> do
				diccionario <- agrega_pal_dicc almacen2 name
				escribe_almacen diccionario almacen2
				tex <- labelGetText commandLabel
				commandLabel`labelSetLabel` ("'Agregada Palabra'"++ valor_n tex)
				statusbarPush statusbar ctx "Palabra agregada correctamente." 
				return ()
			_ -> do
				statusbarPush statusbar ctx "Cancelada la agregacion de palabra."	
				return ()

				
				
	
-- ///////////////////////////////////////////////////////////////////////////
-- // MOSTRAR DICCIONARIO*****************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////

--muestra el diccionario actual, con todas las palabras o diccionarios de palabras 
--que se hayan agregado hasta el momento


  mostrar_diccionario <- xmlGetWidget dialogXml castToMenuItem "mostrar_diccionario"
  mostrar_diccionario `onActivateLeaf` do
	dia2 <- dialogNew
	windowSetPosition dia2 WinPosCenter
	windowSetDefaultSize dia2 400 330
	windowSetTitle dia2 "Diccionarios Agregados"
	dialogAddButton dia2 "Aceptar" ResponseYes
	vScroll <- scrolledWindowNew Nothing Nothing
	almacen_texto <- textViewNewWithBuffer almacen2
	textViewSetEditable almacen_texto False
	textViewSetLeftMargin almacen_texto 5
	textViewSetRightMargin almacen_texto 5
	textViewSetIndent almacen_texto 5
	textViewSetCursorVisible almacen_texto False
 	set vScroll [ containerChild := almacen_texto ]
	--containerAdd vScroll almacen_texto
	contain <- dialogGetUpper dia2
	boxPackStartDefaults contain vScroll
	widgetShowAll dia2
	res <- dialogRun dia2
	statusbarPush statusbar ctx "Mostrado dicionario actual."
	widgetDestroy dia2	
	
	
	
-- ///////////////////////////////////////////////////////////////////////////
-- // SALIR*******************************************************************
-- //	
-- ///////////////////////////////////////////////////////////////////////////


--para abandonar al interfaz, mostrando previamente el mensaje de confirmación de
--que se desea abandonar el interfaz
	
  quitMenuItem <- xmlGetWidget dialogXml castToMenuItem "quitMenuItem"
  quitMenuItem `onActivateLeaf` do
  	vExit <- windowNew
	windowSetPosition vExit WinPosCenter
	texto <- labelNewWithMnemonic "\n¿ Esta seguro que desea \nsalir del programa ?\n"
	botonSi <- buttonNew
	set botonSi [ buttonLabel := "Si" ]
	onClicked botonSi $ do
		mainQuit
	botonNo <- buttonNew
	set botonNo [ buttonLabel := "No" ]
	onClicked botonNo $ do
		statusbarPush statusbar ctx "Cancelado salir del programa."
		widgetDestroy vExit
	contenedorExit <- hButtonBoxNew
	boxPackStartDefaults contenedorExit botonSi
	boxPackStartDefaults contenedorExit botonNo
	divExit <- vPanedNew
	panedSetPosition divExit 5
	panedPack1 divExit texto False False
	panedPack2 divExit contenedorExit False False
	set vExit [ containerChild := divExit, windowTitle := "Salir del programa" ]
	windowSetDefaultSize vExit 200 50
	widgetShowAll vExit

	
	
	
-- ///////////////////////////////////////////////////////////////////////////
-- // SALVAR DICCIONARIO ACTUAL***********************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////

--guarda el diccionario actual


  salvar_actual <- xmlGetWidget dialogXml castToMenuItem "salvar_actual"
  salvar_actual `onActivateLeaf` do
	diccionario <- carga_diccionario almacen2
  	dialog <- fileChooserDialogNew
			(Just "Salvar el Diccionario Actual... ")
			(Just mainWindow)
			FileChooserActionSave
			[("gtk-cancel", ResponseCancel)
			,("gtk-save", ResponseAccept)]
	widgetShow dialog
	response <- dialogRun dialog
	widgetHide dialog
	case response of
	    ResponseAccept -> 
			do
				Just direccion <- fileChooserGetFilename dialog
				archivoStore <- openFile direccion WriteMode
				escribir_dicc archivoStore diccionario
				hClose archivoStore
				statusbarPush statusbar ctx "Dicionario guardado correctamente."
				widgetDestroy dialog
				return ()
	    _ -> 
			do
				statusbarPush statusbar ctx "Cancelado salvar diccionario."
				widgetDestroy dialog
				return ()
				
				

-- ///////////////////////////////////////////////////////////////////////////
-- // ACERCA******************************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////
  
--muestra ventana con la información relativa al proyecto, breve explicación de en 
--qué consiste y datos de autores
  
  
  aboutMenuItem <- xmlGetWidget dialogXml castToMenuItem "aboutMenuItem"
  aboutMenuItem `onActivateLeaf` show_about_dialog mainWindow

  
  
-- ///////////////////////////////////////////////////////////////////////////
-- // COMPROBAR ORTOGRAFIA****************************************************
-- //
-- ///////////////////////////////////////////////////////////////////////////


--a partir de una palabra previamente definida, analiza su validez respecto al diccionario
--actual aplicando todas las reglas de ortografía implementadas en el proyecto.
  
  comprobar_ortografia <- xmlGetWidget dialogXml castToMenuItem "comprobar_ortografia"
  comprobar_ortografia `onActivateLeaf` do 
	diccionario <- carga_diccionario almacen2
	texto <- carga_almacen almacen
	tex <- labelGetText titleLabel
	es_vacio_n tex
	statusbarPush statusbar ctx "Comprobacion de ortografia finalizada."
	analizar_ortografia (divide_texto texto) (carg_dicc diccionario) [] almacen2 almacen
	

  textViewSetBuffer almacen_texto almacen
  windowSetPosition mainWindow WinPosCenter
  widgetShowAll mainWindow
    
  
  mainGUI



-- ///////////////////////////////////////////////////////////////////////////
-- ///////////////////////////////////////////////////////////////////////////
-- // TIPOS Y LLAMADAS A FUNCIONES DE LA GRAMATICA****************************
-- //
-- ///////////////////////////////////////////////////////////////////////////




data TTrie a = Constructor(Char, [TTrie a]) deriving (Ord,Eq,Show)
type TDic = TTrie Char
type TPal = [Char]



vacio:: TDic
vacio = Constructor(' ',[])


--
nuevo_dicc:: TDic
nuevo_dicc = Constructor('.', [])

inversa:: [Char] -> [Char]
inversa lista = reverse lista

diccionarios:: TDic -> [TPal]
diccionarios (Constructor('.',[])) = []
diccionarios (Constructor(raiz,rest_ent)) =
	obtener_palabras rest_ent

	

carg_dicc:: [TPal] -> TDic
carg_dicc [] = nuevo_dicc
carg_dicc listaPalabras = 
	cargar_dicc_aux nuevo_dicc listaPalabras

	

cargar_dicc_aux:: TDic -> [TPal] -> TDic
cargar_dicc_aux diccio [] = diccio
cargar_dicc_aux diccio (p:resto_pal) = cargar_dicc_aux (anadir_palabra diccio p) resto_pal


--une dos diccionarios
juntar_diccs::TDic->TDic->TDic
juntar_diccs dBase dNuevo =
	cargar_dicc_aux dBase (diccionarios dNuevo)

	
--añade una palabra a un dicionario	
anadir_palabra:: TDic -> TPal -> TDic
anadir_palabra diccio [] = diccio
anadir_palabra (Constructor(raiz,restoConstructor)) palabra =
	Constructor (raiz, anadir_palabra_lista restoConstructor (a_minus_sin_acen palabra))

	
--añade una palabra al diccionario actual, que contine todos los diccionarios agregados
anadir_palabra_lista:: [TDic] -> TPal -> [TDic]
anadir_palabra_lista [] pal = [(crear_entrada vacio pal)]
anadir_palabra_lista (diccios@(Constructor(' ',_):rConstructor)) [] =  diccios
anadir_palabra_lista (diccios@(Constructor(pEnt,_):rConstructor)) [] = vacio:diccios
anadir_palabra_lista (Constructor(pEntrada,resto_entrada):rConstructor) (pal@(letra:resto_pal)) =
	if (pEntrada == letra) then
		(Constructor(pEntrada, anadir_palabra_lista resto_entrada resto_pal):rConstructor)
	else 
		if (pEntrada > letra) then
            	([(Constructor(letra, anadir_palabra_lista [] resto_pal))] 				++((Constructor(pEntrada,resto_entrada)):rConstructor))
 
		else 
			((Constructor(pEntrada,resto_entrada)):(anadir_palabra_lista rConstructor (pal)))

--comprueba si una palabra pertenece al diccionario
pertenece_pal:: TDic -> TPal -> Bool
pertenece_pal (Constructor(_,Constructor(_,[]):r)) [] = True
pertenece_pal (Constructor(_,[])) _ = False
pertenece_pal diccio [] = False
pertenece_pal (Constructor(raiz,((Constructor(pEntrada,resto_entrada)):rConstructor)))(palabra@(letra:resto_pal)) =
	if (pEntrada == letra) then 
		pertenece_pal (Constructor(pEntrada,resto_entrada)) resto_pal
	else 
		if (pEntrada < letra) then 
			pertenece_pal (Constructor(raiz, rConstructor)) palabra
		else False
		
		
cons_palabra :: [TPal] -> TDic -> [TPal]
cons_palabra l_palabras ((Constructor(' ',[]))) = l_palabras
cons_palabra l_palabras (Constructor(car,resto)) = concat([(cons_palabra lista y) | y <- resto]) 
      where lista = (insertar_letra car l_palabras)

		
--convierte el diccionario en una lista de palabras		
obtener_palabras :: [TDic] -> [TPal]
obtener_palabras [(Constructor(' ',[]))] = []
obtener_palabras diccionarios = concat ([cons_palabra [[]] x | x <- diccionarios])



insertar_letra :: Char -> [TPal] -> [TPal]
insertar_letra x ll = map (++ [x]) ll



crear_entrada:: TDic -> TPal -> TDic
crear_entrada diccio [] = diccio
crear_entrada (Constructor(' ',[])) ((letra:resto_pal)) =
	Constructor(letra, [crear_entrada vacio ((resto_pal))])

	
	

--aplica todos los patrones a la palabra	
analizar_palabra:: TDic -> TPal -> (Bool,[TPal])
analizar_palabra diccionario palabra = 
	if(pertenece_pal diccionario (a_minus_sin_acen(palabra))) then
		(True,[])
	else 
		if (es_falta_comun diccionario palabra) then
        	(False, (pals_no_repes [] (concat (map (\x -> sugerir diccionario x) 
			(errores_comunes (a_minus_sin_acen palabra))))))

      	else 
			if (es_verbo_plural diccionario palabra) then
				(True,[])
      
			else
				(False, (pals_no_repes [] (concat (map (\x -> sugerir diccionario x) 
					(patrones(errores_comunes (a_minus_sin_acen palabra)))))))

			 
			 
es_falta_comun:: TDic -> TPal -> Bool
es_falta_comun diccionario palabra = (filter (\y -> y==True) 
				(map (\x -> pertenece_pal diccionario x) 
					(errores_comunes(a_minus_sin_acen palabra)))) /= []

--lista las palabras no repetidas
pals_no_repes:: [TPal] -> [TPal] -> [TPal]
pals_no_repes acum [] = acum
pals_no_repes acum (p:r) = 
			if (elem p acum) then 
				pals_no_repes acum r
			else
				pals_no_repes (acum++[p]) r
					
--comprueba si el verbo esta en plural					
es_verbo_plural:: TDic -> TPal -> Bool
es_verbo_plural diccionario palabra = (filter (\y -> y==True) 
				(map (\x -> pertenece_pal diccionario x)
					(verbo_plural[(a_minus_sin_acen palabra)]))) /= []


 
 
comienza_por:: TPal -> TPal -> Bool
comienza_por _ [] = True
comienza_por [] _ = False
comienza_por (pPal:resto_pal) (pPre:rPre) = 
	if (pPal == pPre) && (comienza_por resto_pal rPre) then 
		True
	else
		False

 
--dado un diccionario y una palabra, devuelve la lista de las palabra
--que tienen la misma raiz que palabra. 
sugerir:: TDic -> TPal -> [TPal]
sugerir (Constructor('.',[])) _ = []
sugerir _ [] = []
sugerir diccio palabra = 
	if((pals_raiz_comun diccio palabra)/= []) then
		map (\x-> palabra++x) (pals_raiz_comun diccio palabra)
	else []

	

pals_raiz_comun:: TDic -> TPal -> [TPal]
pals_raiz_comun diccio [] = 
	diccionarios diccio
pals_raiz_comun diccio pal = 
	pals_raiz_comunAux diccio pal



pals_raiz_comunAux:: TDic -> TPal -> [TPal]
pals_raiz_comunAux diccio [] = if (diccionarios diccio /= []) then diccionarios diccio
	else [""]
pals_raiz_comunAux (Constructor(_,[])) _ = []
pals_raiz_comunAux (Constructor(_,((Constructor(pEntrada,resto_entrada)):rConstructor))) (raiz@(letra:rRaiz)) =
	if (pEntrada == letra) then 
		pals_raiz_comunAux (Constructor('.',resto_entrada)) rRaiz
	else 
		if (pEntrada < letra) then
			pals_raiz_comunAux (Constructor('.', rConstructor)) raiz
		else 
			[]


--errores comunes es una función que dada una palabra devuelve una lista de palabras derivadas, a las que se 
--ha podido referir el usuario
--si ha escrito mal la palabra (con errores de ortografía)	
--el usuraio puede haber introducido la palabra con algun tipo de error comun de ortografía como poner
--con b una palabra que es con v o con j una palabra que empieza por g, k por q, etc...

errores_comunes:: TPal -> [TPal]
errores_comunes [] = []
errores_comunes (p:[]) = [[p]]
errores_comunes (palabra@(p:resto_pal@(s:resto)))=
	if ((comienza_por palabra "b")||(comienza_por palabra "v")) then
		[('b':resto_pal), ('v':resto_pal)]
	else if (comienza_por palabra "h") then
		[palabra, resto_pal]
	else if ((comienza_por palabra "a")||(comienza_por palabra"e")||(comienza_por palabra "i")
	||(comienza_por palabra "o")||(comienza_por palabra "u")) then
		[palabra,('h':palabra)]
	else if ((comienza_por palabra "g")||(comienza_por palabra "j")) then
		[('g':resto_pal),('j':resto_pal)]
	else if ((comienza_por palabra "qe")||(comienza_por palabra "qi")) then
		[palabra,("qu"++resto_pal), ('k':resto_pal)]
	else if ((comienza_por palabra "ze")||(comienza_por palabra "zi")) then
		[palabra,('c':resto_pal)]
	else if ((comienza_por palabra "ka")||(comienza_por palabra "ko")||(comienza_por palabra "ku")) then
		[palabra, ('c':resto_pal), ('q':resto_pal)]
    else if ((comienza_por palabra "ke")||(comienza_por palabra "ki")) then
		[palabra, ('q':resto_pal)]
	else if ((comienza_por palabra "q")) then
		[palabra, ('c':resto_pal), ('k':resto_pal)]
	else if ((comienza_por palabra "ca")||(comienza_por palabra "co")||(comienza_por palabra "cu")) then
		[palabra, ('k':resto_pal), ('q':resto_pal)]
    else if ((comienza_por palabra "ce")||(comienza_por palabra "ci")) then
		[palabra, ('z':resto_pal)]
	else
		[(palabra)]


--pasa una palabra a minusculas eliminando los posibles acentos que tenga	
a_minus_sin_acen:: TPal -> TPal
a_minus_sin_acen cad = map (\x -> quitar_acentos (toLower x)) cad


  

extraer_raiz:: TPal -> TPal -> TPal
extraer_raiz [] _ = []
extraer_raiz pal [] = inversa(pal)
extraer_raiz (pPal:resto_pal) (pPref:resto_pref) = 
   if (pPal/=pPref) then []
   else extraer_raiz resto_pal resto_pref

   

verbo_plural:: [TPal] -> [TPal]
verbo_plural palabras = filter (\y -> y /= []) (concat (map (\x -> transformar_ver_pl x) palabras))



transformar_ver_pl:: TPal -> [TPal]
transformar_ver_pl pal = concat [plural pal, tiempos_verbales pal]



patrones:: [TPal] -> [TPal]
patrones palabras = filter (\y -> y /= []) (concat (map (\x -> modificar_patrones x) palabras))



modificar_patrones:: TPal -> [TPal]
modificar_patrones pal = concat [femenino pal, plural pal, adverbio pal, aumentativo pal,
			diminutivo pal, relativo pal, tiempos_verbales pal]

			


			
adverbio:: TPal -> [TPal]
adverbio pal = if (extraer_raiz (inversa pal) (inversa "mente") == []) then []
	else
		[extraer_raiz (inversa pal) (inversa "mente")]

		
		


quitar_acentos:: Char -> Char
quitar_acentos car =
    case car of
		'ú' ->  'u'	
		'é' ->  'e'
		'á' ->  'a'
		'ó' ->  'o'
		'í' ->  'i'
		_ ->  car 

plural:: TPal -> [TPal]
plural pal = [extraer_raiz (inversa(pal)) "s"]   



tiempos_verbales:: TPal -> [TPal]
tiempos_verbales pal = concat [(conjugacion "ar" pal), (conjugacion "er" pal), (conjugacion "ir" pal)]



conjugacion:: TPal -> TPal -> [TPal]
conjugacion morfema pal = concat [(presente morfema pal), (pasado morfema pal), (futuro morfema pal), (imperativo morfema pal),
			       (gerundio morfema pal), (participio morfema pal)]

				   
				   
presente:: TPal -> TPal -> [TPal]
presente morfema pal =
   case morfema of
		"ar" ->  map (\x -> inversa("ra"++(inversa(x)))) 
		     	(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z)))) 
					["o", "as", "a", "amos", "ais", "an"]))
		"er" ->  map (\x -> inversa("re"++(inversa(x)))) 
				(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["o", "es", "e", "emos", "eis", "en"]))
		"ir" ->  map (\x -> inversa("ri"++(inversa(x)))) 
				(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["o", "es", "e", "imos", "is", "en"]))
		_ -> []


   
   
pasado:: TPal -> TPal -> [TPal]
pasado morfema pal =
	case morfema of
		"ar" -> map (\x -> inversa("ra"++(inversa(x)))) 
			   (filter (\y -> y /= [])
			    (map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["e", "uve", "aste", "asteis", "o", "amos", 
					 "aron", "aba", "abas", "abamos", "abais", "aban"]))
		"er" -> map (\x -> inversa("re"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["i", "iste", "io", "imos", "isteis", 
					 "ieron", "ia", "ias", "iamos", "iais", "ian"]))
		"ir" -> map (\x -> inversa("ri"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["i", "iste", "io", "imos", "isteis", 
					 "ieron", "ia", "ias", "iamos", "iais", "ian"]))
		_ -> []

   
   
   
gerundio:: TPal -> TPal -> [TPal]
gerundio morfema pal =
	case morfema of
		"ar" -> map (\x -> inversa("ra"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["ando"]))
		"er" -> map (\x -> inversa("re"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["iendo"]))
		"ir" -> map (\x -> inversa("ri"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["iendo", "yendo"]))
		_ -> []
   
   
   
   
futuro:: TPal -> TPal -> [TPal]
futuro morfema pal =
	case morfema of
		"ar" -> map (\x -> inversa("ra"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["are", "aras", "ara", "aremos", "areis", "aran", "aria", 
					 "arias", "ariamos", "ariais", "arian"]))
		"er" -> map (\x -> inversa("ra"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["ere", "eras", "era", "eremos", "ereis", "eran",
				 	 "eria", "erias", "eriamos", "eriais", "erian"]))
		"ir" -> map (\x -> inversa("ra"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["ire", "iras", "ira", "iremos", "ireis", "iran",
					 "iria", "irias", "iriamos", "iriais", "irian"]))
		_ -> []


   
   
participio:: TPal -> TPal -> [TPal]
participio morfema pal =
	case morfema of
		"ar" -> map (\x -> inversa("ra"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["ado"]))
		"er" -> map (\x -> inversa("re"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["ido"]))
		"ir" -> map (\x -> inversa("ri"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["ido"]))
		_ -> []
   
  
  
  
 
imperativo:: TPal -> TPal -> [TPal]
imperativo morfema pal =
	case morfema of
		"ar" -> map (\x -> inversa("ra"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["a", "e", "emos", "ad", "en"]))
		"er" -> map (\x -> inversa("re"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["a", "e", "amos", "ed", "an"]))
		"ir" -> map (\x -> inversa("ri"++(inversa(x)))) 
			(filter (\y -> y /= [])
				(map (\z -> (extraer_raiz (inversa(pal)) (inversa(z))))
					["a", "e", "amos", "id", "an"]))
		_ -> []
   
   
   
femenino:: TPal -> [TPal]
femenino pal =  
	if ((inversa(extraer_raiz (inversa(pal)) "a"))==[]) then []
	else [inversa('o':(inversa(extraer_raiz (inversa(pal)) "a"))),
            inversa('e':(inversa(extraer_raiz (inversa(pal)) "a")))]  
  
aumentativo:: TPal -> [TPal]
aumentativo pal =
	if((extraer_raiz (inversa pal) (inversa "on")) /= []) then
		[extraer_raiz (inversa pal) (inversa "on")] 
	else if ((extraer_raiz (inversa pal) (inversa "azo")) /= []) then
		[extraer_raiz (inversa pal) (inversa "azo")] 
	else if((extraer_raiz (inversa pal) (inversa "ona")) /= []) then
		[extraer_raiz (inversa pal) (inversa "ona")] 
	else if ((extraer_raiz (inversa pal) (inversa "aza")) /= [])then
		[extraer_raiz (inversa pal) (inversa "aza")] 
	else
		[]

	
	
diminutivo:: TPal -> [TPal]
diminutivo pal =
	if((extraer_raiz (inversa pal) (inversa "in")) /= []) then 
		[inversa('o':inversa ((extraer_raiz (inversa pal) (inversa "in"))))] 
	else if ((extraer_raiz (inversa pal) (inversa "ina")) /= [])then
		[inversa('o':inversa ((extraer_raiz (inversa pal) (inversa "ina"))))] 
	else if ((extraer_raiz (inversa pal) (inversa "ito")) /= []) then 
		[inversa('o':inversa ((extraer_raiz (inversa pal) (inversa "ito"))))] 
	else if ((extraer_raiz (inversa pal) (inversa "ita")) /= [])then
		[inversa('a':inversa ((extraer_raiz (inversa pal) (inversa "ita"))))]
	else if((extraer_raiz (inversa pal) (inversa "illo")) /= [])then
		[inversa('o':inversa ((extraer_raiz (inversa pal) (inversa "illo"))))]
	else if ((extraer_raiz (inversa pal) (inversa "illa")) /= [])then
		[inversa('a':inversa ((extraer_raiz (inversa pal) (inversa "illa"))))]
	else
		[]

	
	
relativo:: TPal -> [TPal]
relativo pal = [extraer_raiz (inversa(pal)) (inversa("al"))]


  
-- ///////////////////////////////////////////////////////////////////////////
-- ///////////////////////////////////////////////////////////////////////////
-- // LLAMADAS AUXILIARES
-- //
-- ///////////////////////////////////////////////////////////////////////////




es_vacio_n :: [Char] -> IO ()
es_vacio_n [] = return ()
es_vacio_n (p:r) =  
		if (p /= '<') then 
		    return ()
	    else
			do
				dia2 <- dialogNew
				windowSetPosition dia2 WinPosCenter
				windowSetDefaultSize dia2 250 150
				windowSetTitle dia2 "Comprobar Ortografia"
				dialogAddButton dia2 "Aceptar" ResponseYes
				label <- labelNew (Just "Debe definir una palabra antes de \ncomprobar la ortografia")
				contain <- dialogGetUpper dia2
				boxPackStartDefaults contain label
				widgetShowAll dia2
				res <- dialogRun dia2
				widgetDestroy dia2
			
		

		
agrega_pal_dicc :: TextBuffer -> String -> IO [String]
agrega_pal_dicc almacen pal = 

	do
		texto <- carga_almacen almacen
		return ([pal]++(obtiene_palabras texto))


		
		
valor_n :: [Char] -> [Char]
valor_n [] = []
valor_n (p:r) =  
		if (p == '<') then 
			[]
		else
			',':p:r

			
analizar_ortografia :: [String] -> TDic -> String -> TextBuffer -> TextBuffer -> IO()
analizar_ortografia [] diccionario texto almacen2 almacen = do
	textBufferSetText almacen texto
	escribe_almacen (diccionarios diccionario) almacen2

	
	
analizar_ortografia palabras diccionario texto almacen2 almacen
	| (not (es_car_alfabetico (head primera))) = do
		analizar_ortografia (tail palabras) diccionario (texto++primera) almacen2 almacen
	| otherwise = do 
		analizar_aux (analizar_palabra diccionario primera) primera diccionario almacen2 almacen palabras texto
	where primera = (head palabras)

	
	
analizar_aux :: (Bool, [String]) -> String -> TDic -> TextBuffer -> TextBuffer -> [String] -> String -> IO()
analizar_aux (False, []) pal diccionario almacen2 almacen palabras texto =
	do
		dia <- dialogNew
		windowSetTitle dia "Agregar Palabra"
		dialogAddButton dia "Si" ResponseYes
		dialogAddButton dia "No"  ResponseNo
		label <- labelNewWithMnemonic ("\nNo se ha encontrado en el diccionario actual \nninguna correspondencia para la palabra \n'"++pal++"' y tampoco palabras similares. \n\n¿Desea agregar esta palabra al\n diccionario actual?\n")
		contain <- dialogGetUpper dia
		boxPackStartDefaults contain label
		windowSetPosition dia WinPosCenter
		widgetShowAll dia
		res <- dialogRun dia
		widgetDestroy dia
		case res of 
			ResponseNo -> do	
				analizar_ortografia (tail palabras) diccionario (texto++pal) almacen2 almacen
				widgetDestroy dia
				return ()
			ResponseYes -> do
				analizar_ortografia (tail palabras) (anadir_palabra diccionario pal) (texto++pal) almacen2 almacen
				widgetDestroy dia
				return ()
			_ -> do
				return ()
	
	
	
analizar_aux (False, alternativa:resto) pal diccionario almacen2 almacen palabras texto = 
	do	
	vSugerencias <- windowNew
	windowSetPosition vSugerencias WinPosCenter
	windowSetDefaultSize vSugerencias 300 150
	comentario <- labelNewWithMnemonic ("\n La palabra '"++pal++"' no se ha \nencontrado en el diccionario,  pero \nse ha encontrado otra parecida que \npodria ser su derivada. \n\n\n ¿Desea Agregar la palabra \nseleccionada?\n")

	opciones <- comboBoxNewText
	widgetSetSizeRequest opciones 0 0

	agregar_sugerencias opciones (alternativa:resto)
	frame <- separatorMenuItemNew
	frame <- frameNew
	frameSetLabel frame "Posibles Palabras "
	containerAdd frame opciones

	
	botonAgregar <- buttonNew
	set botonAgregar [ buttonLabel := "Si" ]
	onClicked botonAgregar $ do
		analizar_ortografia (tail palabras) (anadir_palabra diccionario pal) (texto++pal) almacen2 almacen
		widgetDestroy vSugerencias
	botonOmitir <- buttonNew
	set botonOmitir [ buttonLabel := "No" ]
	onClicked botonOmitir $ do
		analizar_ortografia (tail palabras) diccionario (texto++pal) almacen2 almacen
		widgetDestroy vSugerencias
 	contenedor <- vButtonBoxNew
	boxPackStartDefaults contenedor botonAgregar
	boxPackStartDefaults contenedor botonOmitir
	buttonBoxSetLayout contenedor ButtonboxSpread
  	division2 <- hPanedNew
  	panedSetPosition division2 250
  	panedPack1 division2 frame False False
  	panedPack2 division2 contenedor False False
  	division1 <- vPanedNew
  	panedSetPosition division1 35
  	panedPack1 division1 comentario False False
  	panedPack2 division1 division2 False False
	set vSugerencias [ containerChild := division1, windowTitle := "Se encontro una palabra parecida" ]
	widgetShowAll vSugerencias

	
	
analizar_aux (True, _) pal diccionario almacen2 almacen palabras texto = 
	
	do

	dia2 <- dialogNew
	windowSetPosition dia2 WinPosCenter
	windowSetDefaultSize dia2 250 150
	windowSetTitle dia2 "Comprobacion de ortografia"
	dialogAddButton dia2 "Aceptar" ResponseYes
	label <- labelNew (Just "La palabra pertenece al lenguaje")
	contain <- dialogGetUpper dia2
	boxPackStartDefaults contain label
	widgetShowAll dia2
	res <- dialogRun dia2
	widgetDestroy dia2
		
	analizar_ortografia (tail palabras) diccionario (texto++pal) almacen2 almacen


	
analiza ::  ((Bool,[TPal])) -> IO ()
analiza (True,_) =
    do
		dia2 <- dialogNew
		windowSetPosition dia2 WinPosCenter
		windowSetDefaultSize dia2 250 150
		windowSetTitle dia2 "Diccionario agregado"
		dialogAddButton dia2 "Aceptar" ResponseYes
		label <- labelNew (Just "La palabra existe")
		contain <- dialogGetUpper dia2
		boxPackStartDefaults contain label
		widgetShowAll dia2
		res <- dialogRun dia2
		widgetDestroy dia2

		
		
analiza (False,_) =
    do
		dia2 <- dialogNew
		windowSetPosition dia2 WinPosCenter
		windowSetDefaultSize dia2 250 150
		windowSetTitle dia2 "Diccionario agregado"
		dialogAddButton dia2 "Aceptar" ResponseYes
		label <- labelNew (Just "La palabra no existe")
		contain <- dialogGetUpper dia2
		boxPackStartDefaults contain label
		widgetShowAll dia2
		res <- dialogRun dia2
		widgetDestroy dia2

		
		
obtiene_palabras :: String -> [String]
obtiene_palabras [] = []
obtiene_palabras texto = 

	if (noes_car_alfabetico (head texto)) then 
		(obtiene_palabras (dropWhile noes_car_alfabetico texto))
	else 
		[(takeWhile es_car_alfabetico texto)]++(obtiene_palabras (dropWhile es_car_alfabetico texto))

		
		
escribir_dicc :: Handle -> [String] -> IO ()
escribir_dicc manejador (p:[]) =
	do
		hPutStrLn manejador p

escribir_dicc manejador (p:ps) =
	do
		hPutStrLn manejador p
		escribir_dicc manejador ps

		
divide_texto :: String -> [String]
divide_texto [] = []
divide_texto texto = 

	if (es_car_alfabetico (head texto)) then 
	    [(takeWhile es_car_alfabetico texto)]++(divide_texto (dropWhile es_car_alfabetico texto))
	else
		[(takeWhile noes_car_alfabetico texto)]++(divide_texto (dropWhile noes_car_alfabetico texto))
	 



		
escribe_almacen :: [String] -> TextBuffer -> IO()
escribe_almacen (p:[]) almacen =

	do
		textBufferSetText almacen p		

escribe_almacen lista almacen = 

	do
		textBufferSetText almacen (concat (inserta_ret_carro lista))

		
		
inserta_ret_carro :: [String] -> [String]
inserta_ret_carro [] = []
inserta_ret_carro (pal:[]) = [pal]
inserta_ret_carro (pal:resto) = [pal]++["\n"]++(inserta_ret_carro resto)

		
carga_almacen :: TextBuffer -> IO String
carga_almacen almacen = 

	do
		comienzo <- textBufferGetIterAtOffset almacen 0
		chars <- textBufferGetCharCount almacen
		final <- textBufferGetIterAtOffset almacen chars
		texto <- (textBufferGetText almacen comienzo final False)
		return texto





	 
agregar_dicc :: Handle -> [String] -> IO [String]
agregar_dicc manejador diccionario =
	do 
	        final <- hIsEOF manejador
		case final of
			True -> return diccionario
			False -> do
				 palabra <- hGetLine manejador
				 case elem palabra diccionario of
					True -> agregar_dicc manejador diccionario
					False -> agregar_dicc manejador (diccionario++[palabra])
		
	

	
es_car_alfabetico :: Char -> Bool
es_car_alfabetico c = 

			if (((c>='A') && (c<='Z')) || ((c>='a') && (c<='z')) || (fromEnum c == 209) || (fromEnum c == 241) || (c=='ç') || (c=='Á') || (c=='É') || (c=='Í') || (c=='Ó') || (c=='Ú')|| (c=='á') || (c=='é') || (c=='í') || (c=='ó') || (c=='ú')) then
				True
			else
				False

			
			
noes_car_alfabetico :: Char -> Bool
noes_car_alfabetico c = not (es_car_alfabetico c)




		
agregar_sugerencias :: ComboBox -> [String] -> IO()
agregar_sugerencias opciones (alternativa:[]) = comboBoxAppendText opciones alternativa
agregar_sugerencias opciones (alternativa:resto) =
	
	do
		comboBoxAppendText opciones alternativa
		agregar_sugerencias opciones resto

carga_diccionario :: TextBuffer -> IO [String]
carga_diccionario almacen = 

	do
		texto <- carga_almacen almacen
		return (obtiene_palabras texto)
		

		
open_doc_dialog :: IO (Maybe String)
open_doc_dialog =
    do	 dia <- dialogNew                                -- Create a dialog box
         windowSetTitle dia "Open an existing document"
	 dialogAddButton dia "Open" ResponseYes          -- Add buttons and response codes
	 dialogAddButton dia "Cancel"  ResponseNo
	 label <- labelNew (Just "Open document:")       -- Create a label
	 input <- entryNew                               -- Create a text field
	 contain <- dialogGetUpper dia                   -- Get the upper container of the dialog box
	 boxPackStartDefaults contain label              -- Add the widgets to the container
         boxPackStartDefaults contain input
         widgetShowAll dia                               -- Show the dialog
         res <- dialogRun dia                            -- Run it (blocking all else)
         widgetDestroy dia                               -- Destroy the dialog box
	 case res of
	   ResponseYes -> do  name <- entryGetText input
	   		      return (Just name)

	   ResponseNo  -> do  return Nothing

	   
	   -- display a standard file open dialog

	   
	   
open_file_dialog :: Window -> IO ()
open_file_dialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just "Open Profile... ")
              (Just parentWindow)
	      FileChooserActionOpen
	      [("gtk-cancel", ResponseCancel)
	      ,("gtk-open", ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog

	
show_about_dialog :: Window -> IO ()
show_about_dialog parent = do
  -- create the about dialog
  aboutDialog <- aboutDialogNew
  
  -- set some attributes
  set aboutDialog [
      aboutDialogName      := "Teshaurus",
      aboutDialogVersion   := "1.0",
      aboutDialogCopyright := "",
      aboutDialogComments  := "Se dispone de un diccionario de palabras del castellano (implementado con un trie) simples y ante una palabra nueva debe determinar si posiblemente pertenece al castellano o no. En el diccionario no aparecen ni los plurares, ni los cambios de genero ni los participios, etc. Las reglas del castellano para tales casos pueden simplificarse. Se incluirá en un programa que permita componer texto y que si no encuentra en el diccionario una de las palabras lo haga saber así como proponga palabras correctas similares (esto es, lo que hace cualquier comprobador de ortografía de un procesador de textos).",
      aboutDialogWebsite   := "http://www.haskell.org/haskellwiki/Haskell"
    ]
  
  -- make the about dialog appear above the main window
  windowSetTransientFor aboutDialog parent
  
  -- make the dialog non-modal. When the user closes the dialog destroy it.
  aboutDialog `afterResponse` (\_ -> widgetDestroy aboutDialog)
  widgetShow aboutDialog
  
