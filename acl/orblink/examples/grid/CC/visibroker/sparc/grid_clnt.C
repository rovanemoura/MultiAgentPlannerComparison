// grid_clnt.C

#include <fstream.h>
#include "grid_c.hh"

CORBA::Object_var object_from_file(CORBA::ORB_var orb, char * filename);
int main (int argc , char * const* argv) {
  try{
    // Initialize the ORB.
    CORBA::ORB_var orb = CORBA::ORB_init (argc, argv);
    //Get an initial object
    CORBA::Object_var obj = object_from_file (orb, "/a/stiller/orblink/ior/grid");
    cout<< "Got object of: " << obj;
    example::grid_var grid = example::grid::_narrow (obj);
    cout << "Did narrow of: " << grid;
    cout << "Invoking call...\n";
    grid->set(2, 3, "C++ test\n");
    cout << "did set..";
    cout << "Got get of: " << grid->get (2, 3);
  }
  catch (const CORBA::Exception& e) {
    cerr << e <<endl;
    return (1);
  }
  return (0);
}

CORBA::Object_var object_from_file(CORBA::ORB_var orb, char * filename)
{
  FILE *fp = fopen (filename, "r");
  if (!fp) return NULL;
  char buffer[4096];
  fscanf (fp,"%s\n",buffer);
  return orb->string_to_object(buffer);
}

