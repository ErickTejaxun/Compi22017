import ply.lex as lex

#palabras reservadas
reserved = {
	'suma' : 'FSUMA',
	'resta': 'FRESTA',
	'multiplicacion': 'FMULTI',
	'division' : 'FDIV',
	'text': 'TEXT',
	'no': 'NO',
	'nulo': 'NULO',
	'date': 'DATE',
	'bool': 'BOOL',
	'crear': 'CREAR',
	'tabla': 'TABLA',
	'base_datos':'BASEDATOS',
	'objeto':'OBJETO',
	'autoincremental': 'AUTOINC',
	'llave_primaria': 'PRIMARIA',
	'llave_foranea': 'FORANEA',
	'unico': 'UNICO',
	'procedimiento': 'PROCEDIMIENTO',
	'actualizar': 'ACTUALIZAR',
	'donde': 'DONDE',
	'valores': 'VALORES',
	'retorno': 'RETORNO',
	'usuario': 'USUARIO',
	'usar' : 'USAR',
	'alterar': 'ALTERAR',
	'agregar': 'AGREGAR',
	'quitar': 'QUITAR',
	'objeto-usql': 'OBJETOUSQL',
	'eliminar': 'ELIMINAR',
	'user': 'USUARIOB',
	'altero': 'ALTERARU',
	'en': 'EN',
	'borrar': 'BORRAR',
	'seleccionar': 'SELECCIONAR',
	'ordenar_por': 'ORDENAR',
	'asc': 'ASCENDENTE',
	'desc': 'DESCENDENTE',
#DCL
	'otorgar': 'OTORGAR',
	'permisos': 'PERMISOS',
	'denegar': 'DENEGAR',
#SSL
	'declara': 'DECLARA',
	'si': 'IF',
	'sino': 'ELSE',
	'selecciona': 'SWITCH',
	'caso':	'CASE',
	'defecto': 'DEFECTO',
	'para': 'FOR',
	'mientras': 'MIENTRAS',
	'detener': 'BREAK',
	'imprimir': 'IMPRIMIR',
	'fecha()': 'FFECHA',
	'fecha_hora()': 'FFECHAHORA',
	'contar': 'CONTAR',
	'de': 'DE',
#BACKUPS
	'backup': 'BACKUP',
	'usqldump': 'BACKUPPARCIAL',
	'completo': 'BACKUPCOMPLETO',
	'restaurar': 'RESTAURAR'
}

tokens = [	
	'INTEGER',
	'DOUBLE',	
	'DATETIME',	
	'OR',
	'PLUS',
	'MINUS',
	'AND',
	'MULTI'
	'DIV',
	'POWER',			
	'FPOWER',
	'ID',
	'IGUAL',
	'DIFERENTE',
	'MENORQUE',
	'MAYORQUE',
	'MENORIGUAL',
	'MAYORIGUAL',
	'NOT',
	'PARA',
	'PARC',
	'PUNTOCOMA',	
	'VARIABLE',
	'COMA',
	'LLAVEA',
	'LLAVEC',
	'ASIGNACION',
	'TEXTOESP',
	'PUNTO',
	'MULTI',
	'INCREMENTAR',
	'DECREMENTAR',
	'ABRIRSELECCIONAR',
	'CERRARSELECCIONAR',
	'PATHLINUX',
	'PATHWINDOWS',
	'COMENTARIOS',
	'COMENTARIOM',
	'ENTERO'
] + list(reserved.values())

#Expresiones regulares para cadenas/caracteres basicos.
t_ABRIRSELECCIONAR = r'<<'
t_CERRARSELECCIONAR = r'>>'
t_INCREMENTAR = r'\++'
t_DECREMENTAR = r'--'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_POWER   = r'\^'
t_OR	  = r'\|\|'
t_AND	  = r'&&'
t_MULTI   = r'\*'
t_NOT	  = r'!'
t_IGUAL   = r'=='
t_DIFERENTE = r'!='
t_MENORQUE = r'<'
t_MAYORQUE = r'>'
t_MENORIGUAL = r'<='
t_MAYORIGUAL = r'>='
t_PARA    = r'\('
t_PARC    = r'\)'
t_LLAVEA = r'{'
t_LLAVEC = r'}'
t_PUNTOCOMA = r';'
t_ASIGNACION = r'='
t_PUNTO = r'\.'
t_COMA = r'\,'
t_ignore  = ' \t'




#Expresion regular de valores de entrada

def t_newline(t):
	r'\n+'
	t.lexer.lineno += len(t.value)

def t_TEXT(t):
	r'[a-zA-Z_][a-zA-Z_0-9]*'
	#t.value = reserved.get(t.values,'ID')
	#t.value = (t.value, symbol_lookup(t.value))
	return t

def t_ENTERO(t):
	r'\d+'
	t.value = int(t.value)
	return t

def t_TEXTOESP(t):
	r'\[[a-zA-Z_][a-zA-Z_0-9]*| \t]+\"'
	#t.value = (t.value, symbol_lookup(t.value))
	return t			  

def t_VARIABLE(t):
	r'@[a-zA-Z_][a-zA-Z_0-9]*'
	#t.value = (t.value, symbol_lookup(t.value))
	return t

def t_PATHLINUX(t):
	r'/([a-zA-Z_][a-zA-Z_0-9]*| \t)+'
	#t.value = (t.value, symbol_lookup(t.value))
	return 
			   
def t_PATHWINDOWS(t):
	r'[a-zA-Z_][/\+[[a-zA-Z_][a-zA-Z_0-9]]* | \t]+'
        #t.value = (t.value, symbol_lookup(t.value))
	return t			   

def t_COMENTARIOS(t):
	r'\#[a-zA-Z_][a-zA-Z_0-9]*| \t]\#'			  
	pass

def t_COMENTARIOM(t):
	r'\#*[[a-zA-Z_][a-zA-Z_0-9]*| \t|\n]*\#'			  
	pass				  

#Reglas de manejo de errores.
				  
def t_error(t):
	print("Error lexico, caracter '%s' no valido" % t.value[0])
	t.lexer.skip(1)
				  
#prueba de inicio
data= ''' 
CREAR BASE_DATOS Ejemplo1;
USAR Ejemplo1;
CREAR OBJETO Direccion(INTEGER avenida, INTEGER calle, TEXT
nombre, TEXT descripcion);
CREAR TABLA Estudiante (
INTEGER id Llave_Primaria Autoincrementable,
TEXT Nombre No Nulo,
DATE fh_nac No Nulo,
BOOL trabaja,
Direccion direccion);
CREAR TABLA Curso(
INTEGER id Llave_Primaria Autoincrementable,
TEXT Nombre No Nulo,
INTEGER creditos No Nulo);
CREAR TABLA Asignacion(
INTEGER id Llave_Primaria Autoincrementable,
DATETIME fh_Asignacion No Nulo,
INTEGER id_estudiante Llave_Foranea Estudiante,
INTEGER id_curso Llave_Foranea Curso);
CREAR ROCEDIMIENTO Asignar (INTEGER @id_e, INTEGER @id_c){
 DECLARAR @estudiante INTEGER= CONTAR(<<SELECCIONAR * DE
estudiante DONDE id == @id_e>>);
 DECLARAR @curso INTEGER= CONTAR(<<SELECCIONAR * DE curso DONDE
id == @id_c>>);
 SI (@curso > 0){
 SI(@estudiante > 0){
 INSERTAR EN TABLA asignacion(id_estudiante, id_curso)
VALORES (@id_e, @id_c);
 }SINO{
 IMPRIMIR("El usuario ingresado no existe");
 }
 }SINO{
 IMPRIMIR("El curso ingresado no existe");
 }
}
CREARFUNCION crearDireccion (INTEGER @calle, INTEGER @avenida,
TEXT @nombre, TEXT @descripcion) Direccion{
 DECLARAR @direc DIRECCION;
 @direc.calle = @calle;
 @direc.avenida = @avenida;
 @direc.nombre = @nombre;
 @direc.descripcion = @descripcion;
 RETORNO @direc;
}
'''

#Constructor del analizador.
lexer = lex.lex()
lexer.input(data)
				  
tok = lexer.token()

for tok in lexer:
	print(tok)
