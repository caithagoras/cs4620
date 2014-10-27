/* File: ast_type.cc
 * -----------------
 * Implementation of type node classes.
 */
#include "ast_type.h"
#include "ast_decl.h"
#include <string.h>
#include <typeinfo>
#include <iostream>
using namespace std;
 
/* Class constants
 * ---------------
 * These are public constants for the built-in base types (int, double, etc.)
 * They can be accessed with the syntax Type::intType. This allows you to
 * directly access them and share the built-in types where needed rather that
 * creates lots of copies.
 */

Type *Type::intType    = new Type("int");
Type *Type::doubleType = new Type("double");
Type *Type::voidType   = new Type("void");
Type *Type::boolType   = new Type("bool");
Type *Type::nullType   = new Type("null");
Type *Type::stringType = new Type("string");
Type *Type::errorType  = new Type("error"); 

Type::Type(const char *n) {
    Assert(n);
    typeName = strdup(n);
}
	
NamedType::NamedType(Identifier *i) : Type(*i->GetLocation()) {
    Assert(i != NULL);
    (id=i)->SetParent(this);
} 

ArrayType::ArrayType(yyltype loc, Type *et) : Type(loc) {
    Assert(et != NULL);
    (elemType=et)->SetParent(this);
}

bool operator==(const Type &t1, const Type &t2){
  bool t1_is_named = true, t2_is_named = true, t1_is_array =  true, t2_is_array =true;
  
  try {
    dynamic_cast<const NamedType&>(t1);
  } catch (std::bad_cast &e){
    t1_is_named = false;
  }

  try {
    dynamic_cast<const NamedType&>(t2);
  } catch (std::bad_cast &e){
    t2_is_named = false;
  }

  try {
    dynamic_cast<const ArrayType&>(t1);
  } catch (std::bad_cast &e){
    t1_is_array = false;
  }
  
  try {
    dynamic_cast<const ArrayType&>(t2);
  } catch (std::bad_cast &e){
    t2_is_array = false;
  }
  
  if (t1_is_named && t2_is_named) {
    const NamedType &named_type_t1 = static_cast<const NamedType&>(t1);
    const NamedType &named_type_t2 = static_cast<const NamedType&>(t2);
    return strcmp(named_type_t1.id->name, named_type_t2.id->name) == 0;
  }
  
  if (t1_is_array && t2_is_array) {
    const ArrayType &array_type_t1 = static_cast<const ArrayType&>(t1);
    const ArrayType &array_type_t2 = static_cast<const ArrayType&>(t2);
    return operator==(*array_type_t1.elemType, *array_type_t2.elemType);
  }
  
  if (t1_is_named || t2_is_named || t1_is_array || t2_is_array)
    return false;

  return strcmp(t1.typeName, t2.typeName) == 0;
}

bool operator!=(const Type &t1, const Type &t2) {
  return ! (operator==(t1, t2));
}
