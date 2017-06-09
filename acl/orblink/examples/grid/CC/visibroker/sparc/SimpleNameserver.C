#include <stdio.h>
#include <fstream.h>
#include "corba.h"

char * get_line_from_file (char * filename)
{	
  FILE *fp = fopen (filename, "r");
  char buffer[4096];
  if (!fp) 
    {return NULL;}
  fscanf(fp, "%s\n",buffer);
  return buffer;}

CORBA::Object_var SimpleNameserver_resolve(CORBA::ORB_var orb, char * name)
{
  char * str = get_line_from_file (name);
  if (!str) return NULL;
  return orb->string_to_object(str);
}
  
/*
int main ()
{
  cout << "Got line of: " << get_line_from_file("test.dat") << "\n";
}
*/
  
  
  
