
/**
 **************************************************************************
 *                                                                        *
 *                        -- DO NOT MODIFY --                             *
 *  This file is automatically generated by the VisiBroker IDL compiler.  *
 *  Generated code conforms to OMG's IDL-to-C++ 1.1 mapping as specified  *
 *  in OMG Document Number: 96-01-13                                      *
 *                                                                        *
 *  VisiBroker is copyrighted by Visigenic Software, Inc.                 *
 **************************************************************************
 */

#include "grid_c.hh"

example::grid_ptr example::grid_var:: _duplicate(example::grid_ptr _p) { return example::grid::_duplicate(_p); }
void example::grid_var::_release(example::grid_ptr _p) { CORBA::release(_p); }

example::grid_var::grid_var() : _ptr(example::grid::_nil()) {}

example::grid_var::grid_var(example::grid_ptr _p) : _ptr(_p) {}

example::grid_var::grid_var(const example::grid_var& _var) :
   _ptr(example::grid::_duplicate((example::grid_ptr)_var)) {}

example::grid_var::~grid_var() { CORBA::release(_ptr); }

example::grid_var& example::grid_var::operator=(example::grid_ptr _p) {
  CORBA::release(_ptr);
  _ptr = _p;
  return *this;
}


example::grid_ptr& example::grid_var::out() {
  CORBA::release(_ptr);
  _ptr = (example::grid_ptr)NULL;
  return _ptr;
}

VISistream& operator>>(VISistream& _strm, example::grid_var& _var) {
  _strm >> _var._ptr;
  return _strm;
}

VISostream& operator<<(VISostream& _strm, const example::grid_var& _var) {
  _strm << _var._ptr;
  return _strm;
}

istream& operator>>(istream& _strm, example::grid_var& _var) {
  VISistream _istrm(_strm);
  _istrm >> _var._ptr;
  return _strm;
}

ostream& operator<<(ostream& _strm, const example::grid_var& _var) {
  _strm << (CORBA::Object_ptr)_var._ptr;
  return _strm;
}

const CORBA::TypeInfo example::grid::_class_info(
  "example::grid",
  "IDL:example/grid:1.0",
  NULL,
  &example::grid::_factory,
  0, NULL,
  CORBA::Object::_desc(),
  0);

VISistream& operator>>(VISistream& _strm, example::grid_ptr& _obj) {
  CORBA::Object_var _var_obj(_obj);
  _var_obj = CORBA::Object::_read(_strm, example::grid::_desc());
  _obj = example::grid::_narrow(_var_obj);
  return _strm;
}

VISostream& operator<<(VISostream& _strm, const example::grid_ptr _obj) {
  _strm << (CORBA_Object_ptr)_obj;
  return _strm;
}


const CORBA::TypeInfo *example::grid::_desc() { return &_class_info; }

const CORBA::TypeInfo *example::grid::_type_info() const { return &_class_info; }

void *example::grid::_safe_narrow(const CORBA::TypeInfo& _info) const {
  if ( _info == _class_info) 
    return (void *)this;
  return CORBA_Object::_safe_narrow(_info);
}

CORBA::Object *example::grid::_factory() {
  return new example::grid;
}

example::grid_ptr example::grid::_this() {
  if( _root != NULL ) {
    return example::grid::_duplicate(_root);
  }
  else {
    return example::grid::_duplicate(this);
  }
}

example::grid *example::grid::_narrow(CORBA::Object *_obj) {
  if ( _obj == CORBA::Object::_nil() )
    return grid::_nil();
  else
    return grid::_duplicate((grid_ptr)_obj->_safe_narrow(_class_info));
}

example::grid *example::grid::_bind(
    const char *_object_name,
    const char *_host_name,
    const CORBA::BindOptions *_opt,
    CORBA::ORB_ptr _orb) {
  CORBA::Object_var _obj= CORBA::Object::_bind_to_object(
      "IDL:example/grid:1.0", _object_name, _host_name, _opt, _orb);
  return grid::_narrow(_obj);
}

CORBA::Short example::grid::width() {
  CORBA::Short _ret;
  CORBA::MarshalOutBuffer_var _obuf(_create_request(
      "_get_width",
      1,
      1320));
  CORBA::MarshalInBuffer_var _ibuf;
  try { _ibuf = _invoke(_obuf); }
  catch (const CORBA::TRANSIENT& ) { return width(); }
  VISistream& _vistrm = *(CORBA::MarshalInBuffer *)_ibuf;
  _vistrm >> _ret;
  return _ret;
}

void example::grid::set(
    CORBA::Short _n,
    CORBA::Short _m,
    const char* _value) {
  example::grid_var _thisvar = _this();
  CORBA::MarshalOutBuffer_var _obuf(_thisvar->_create_request(
      "set",
      1,
      370));
  VISostream& _ostrm = *(VISostream *)(CORBA::MarshalOutBuffer*)_obuf;
  _ostrm << _n;
  _ostrm << _m;
  _ostrm << _value;
  
  CORBA_MarshalInBuffer_var _ibuf;
  try { _ibuf = _thisvar->_invoke(_obuf); }
  catch (const CORBA::TRANSIENT& ) {
    set(
        _n,
        _m,
        _value);
    return;
  }

  VISistream& _vistrm = *(CORBA::MarshalInBuffer *)_ibuf;
}

CORBA::Short example::grid::height() {
  CORBA::Short _ret;
  CORBA::MarshalOutBuffer_var _obuf(_create_request(
      "_get_height",
      1,
      2336));
  CORBA::MarshalInBuffer_var _ibuf;
  try { _ibuf = _invoke(_obuf); }
  catch (const CORBA::TRANSIENT& ) { return height(); }
  VISistream& _vistrm = *(CORBA::MarshalInBuffer *)_ibuf;
  _vistrm >> _ret;
  return _ret;
}

char* example::grid::get(
    CORBA::Short _n,
    CORBA::Short _m) {
  char* _ret = (char*)0;
  example::grid_var _thisvar = _this();
  CORBA::MarshalOutBuffer_var _obuf(_thisvar->_create_request(
      "get",
      1,
      290));
  VISostream& _ostrm = *(VISostream *)(CORBA::MarshalOutBuffer*)_obuf;
  _ostrm << _n;
  _ostrm << _m;
  
  CORBA_MarshalInBuffer_var _ibuf;
  try { _ibuf = _thisvar->_invoke(_obuf); }
  catch (const CORBA::TRANSIENT& ) {
    return get(
        _n,
        _m);
  }

  VISistream& _vistrm = *(CORBA::MarshalInBuffer *)_ibuf;
  _vistrm >> _ret;
  return _ret;
}
