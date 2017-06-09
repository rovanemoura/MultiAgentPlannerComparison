#!/usr/bin/python2.7

import os
import sys



# main
# -----------------------------------------------------------------------------
if __name__ == '__main__':

    if len(sys.argv) != 7:
        raise SystemExit("Usage: %s <PLANS_FOLDER> <INPUT_NAME> <OUTPUT_NAME> <ORIGINAL_DOMAIN> <ORIGINAL_PROBLEM> <BEST_COST_FOUND>" % sys.argv[0])
        sys.exit(-1)

    else:
        # Calculamos la ruta del validate
        pathname = os.path.dirname(sys.argv[0])
        scriptpath = os.path.abspath(pathname)
        val = scriptpath + "/VAL-4.2.08/validate -v"


        if (os.path.isdir(sys.argv[1])):
            plans_folder = os.path.abspath(sys.argv[1])
            input_name = sys.argv[2]
            output_name = sys.argv[3]

            if (os.path.isfile(sys.argv[4])):
                original_domain = os.path.abspath(sys.argv[4])

                if (os.path.isfile(sys.argv[5])):
                    original_problem = os.path.abspath(sys.argv[5])
                    best_cost = int(sys.argv[6])

                else:
                    print("\nThe problem file does not exist: %s \n" % sys.argv[5])
                    sys.exit(-1)

            else:
                print("\nThe domain file does not exist: %s \n" % sys.argv[4])
                sys.exit(-1)

        else:
            print("\nThe plans folder does not exist: %s \n" % sys.argv[1])
            sys.exit(-1)


        print("\n\nplans_folder: " + plans_folder + "\ninput_name: " + input_name + "\noutput_name: " + output_name + "\noriginal_domain: " + original_domain + "\noriginal_problem: " + original_problem + "\nbest cost found: " + str(best_cost) + "\n\n\n")


        # Obtenemos las rutas de los planes de entrada y salida
        input_plans = []
        output_plans = []
        data = os.popen("ls -l " + plans_folder + "/" + input_name + "*")

        for line in data.readlines():
            if((len(line) > 0) and (line[len(line)-1] == '\n')):
                line = line[:-1].strip()		# Quitamos el \n

            elements = line.split()

            if(len(elements) > 0):
                name = elements[len(elements)-1].strip()
                ext = name[name.find(input_name)+len(input_name):]
                input_plans.append(name)
                output_plans.append(plans_folder + "/" + output_name + ext)

            else:
                print("\nWarning: El comando ls al buscar los ficheros de plan ha devuelto una linea sin elementos.\n")


        print("\nSe han encontrado " + str(len(input_plans)) + " ficheros de solucion:\n")
        for i in input_plans:
            print("input_plan: " + i)
        for i in output_plans:
            print("output_plan: " + i)
        print("\n\n")


        # Limpiamos los planes de entrada
        for i in xrange(0, len(input_plans)):

            input = open(input_plans[i], 'r').readlines()
            output = open(output_plans[i], 'w')

            for line in input:
                if((len(line) > 0) and (line[len(line)-1] == '\n')):
                    line = line[:-1]		# Quitamos el \n

                if (line.find("condef") < 0):
                    line = line.replace("_---_", " ")

                    if(line.find("-___-") >= 0):
                        line = line[:line.find("-___-")]
                        line += ")"

                    output.write(line + "\n")
            os.system("rm -f " + input_plans[i])
            output.close()


        # Verificamos los ficheros generados
        for i in output_plans:
            name = val + " " + original_domain + "  " + original_problem + " " + i
            data = os.popen(name)
            successful_plan = False
            current_cost = -1

            for line in data.readlines():
                line = line.strip()
                if((len(line) > 0) and (line[len(line)-1] == '\n')):
                    line = line[:-1]		# Quitamos el \n

                if(line.find("Successful plans:") >= 0):
                    successful_plan = True

                elif(line.find("Value:") >= 0):
                    cost_elements = line.split()
                    if(len(cost_elements) == 2):
                        current_cost = int(cost_elements[1].strip())
                    else:
                        print "ERROR! Wrong cost line: " + str(line)


            if((not successful_plan) or (current_cost == -1)):
                print("ERROR: plan " + str(i) + " is a wrong plan file or the plan cost is equal to -1, so we remove it")
                os.system("rm -f " + i)

            elif((successful_plan) and ((best_cost != -1) and (current_cost > best_cost))):
                print("ERROR: plan " + str(i) + " is worse (" + str(current_cost) + ") than the previous plan generated (" + str(best_cost) + "), therefore we remove it")
                os.system("rm -f " + i)

            else:
                 print(str(i) + ": successful_plan")

