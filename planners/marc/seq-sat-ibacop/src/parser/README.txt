Hay dos scripts principales:


* parse.py:
	- Elimina los action costs del problema y dominio
	- Los almacena en ficheros auxiliares
	- Compila los efectos condicionales 
	- Restaura los action costs

* clean_plans.py:
	- Limpia los planes generados para que sean v‡lidos para el dominio original
	- Valida los planes parseados, si no son v‡lidos se eliminan