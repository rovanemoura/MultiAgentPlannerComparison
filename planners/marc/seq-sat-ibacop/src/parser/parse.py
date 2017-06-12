#!/usr/bin/python2.7

import os
import sys


def remove_all_generated_files():
    name = "rm -f " + domain_wac + " " + problem_wac + " " + action_costs_domain_file + " " + action_costs_problem_file + " " + domain_wce + " " + problem_wce + " " + domain_ready + " " + problem_ready
    data = os.system(name)
    sys.exit(-1)



# main
# -----------------------------------------------------------------------------
if __name__ == '__main__':

    if len(sys.argv) != 3:
        raise SystemExit("Usage: %s <DOMAINFILE> <PROBLEMFILE>" % sys.argv[0])
        sys.exit(-1)

    else:
        # Calculamos la ruta absoluta del script
        pathname = os.path.dirname(sys.argv[0])
        scriptpath = os.path.abspath(pathname)


        if (os.path.isfile(sys.argv[1])):
            original_domain = os.path.abspath(sys.argv[1])

            if (os.path.isfile(sys.argv[2])):
                original_problem = os.path.abspath(sys.argv[2])

            else:
                print("The problem file does not exist: %s \n" % sys.argv[2])
                sys.exit(-1)

        else:
            print("The domain file does not exist: %s \n" % sys.argv[1])
            sys.exit(-1)


        domain_wac  	 	  = scriptpath + "/domain_without_action_costs.txt"
        problem_wac 	 	  = scriptpath + "/problem_without_action_costs.txt"
        domain_wce  	 	  = scriptpath + "/domain_wce.txt"
        problem_wce 	 	  = scriptpath + "/facts_wce.txt"
        domain_ready  	 	  = scriptpath + "/domain_ready.txt"
        problem_ready 	 	  = scriptpath + "/problem_ready.txt"
        action_costs_domain_file  = scriptpath + "/action_costs_from_domain_file.txt"
        action_costs_problem_file = scriptpath + "/action_costs_from_problem_file.txt"
        adl_2_strips		  = scriptpath + "/adl2strips/ff"


    print("\noriginal_domain: " + original_domain + "\ndomain_wac: " + domain_wac + "\ndomain_wce: " + domain_wce + "\ndomain_ready: " + domain_ready)
    print("\noriginal_problem: " + original_problem + "\nproblem_wac: " + problem_wac + "\nproblem_wce: " + problem_wce + "\nproblem_ready: " + problem_ready)
    print("\naction_costs_domain_file: " + action_costs_domain_file + "\naction_costs_problem_file: " + action_costs_problem_file + "\n")


    # Inicializacion de variables auxiliares
    functions = ""
    action_costs = []
    numMaxOfWhenPerAction = 0


    # Primero limpiamos el problema y el dominio de action costs
    name = "python2.7 " + scriptpath + "/clean_action_costs.py " + original_domain + " " + original_problem
    data = os.system(name)

    if(data != 0):
        print("ERROR al eliminar los action costs del problema y dominio original\n")
        remove_all_generated_files()


    print("\n\nAction costs processed.\n")


    # Analizamos el fichero de action costs del dominio
    data = open(action_costs_domain_file, 'r').readlines()
    for i in data:
        if((len(i) > 0) and (i[len(i)-1] == '\n')):
            i = i[:-1] + " "		# Quitamos el \n

        elements = i.split("|")

        if(len(elements) == 2):		# Functions
            functions = elements[0]

        elif(len(elements) == 3):	# Actions
            elements[1] = elements[1].strip()
            elements[2] = elements[2].strip()

            if(int(elements[2]) > numMaxOfWhenPerAction):
                numMaxOfWhenPerAction = int(elements[2])

            action_costs.append(elements[0] + " | " + elements[1])

        else:
            print("Unexpected String: " + str(i) + "\n")


    print("\nhighest number of when per action: " + str(numMaxOfWhenPerAction) + "\n")


    if(numMaxOfWhenPerAction == 0):	# No hay efectos condicionales
        # Eliminamos ficheros generados: Si no existe el dominio modificado se usara el original por defecto
        name = "rm -f " + domain_wac + " " + problem_wac + " " + action_costs_domain_file + " " + action_costs_problem_file
        data = os.system(name)
        print("\nEl dominio original no tiene efectos condicionales, por lo tanto no es necesario realizar la compilacion.\n")
        sys.exit(1)

    elif(numMaxOfWhenPerAction >= 4):	# Compilacion polinomica (-B)
        name = adl_2_strips + " -o " + domain_wac + " -f " + problem_wac + " -s " + scriptpath  + "/ -B"
        data = os.system(name)

        if(data != 0):
            print("ERROR al compilar los efectos condicionales de forma polinomica\n")
            remove_all_generated_files()

    else:				# Compilacion exponencial
        name = adl_2_strips + " -o " + domain_wac + " -f " + problem_wac + " -s " + scriptpath + "/"
        data = os.system(name)

        if(data != 0):
            print("ERROR al compilar los efectos condicionales de forma exponencial\n")
            remove_all_generated_files()


    print("\nConditional effects compiled.\n")


    # Restauramos los action costs
    name = "python2.7 " + scriptpath + "/restore_action_costs.py " + domain_wce + " " + problem_wce
    data = os.system(name)

    if(data != 0):
        print("ERROR al restaurar los action costs en el dominio y problema compilado\n")
        remove_all_generated_files()

    print("\nAction costs restored.\n")
