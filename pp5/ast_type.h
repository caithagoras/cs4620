/* File: ast_type.h
 * ----------------
 * In our parse tree, Type nodes are used to represent and
 * store type information. The base Type class is used
 * for built-in types, the NamedType for classes and interfaces,
 * and the ArrayType for arrays of other types.  
 *
 * pp3: You will need to extend the Type classes to implement
 * the type system and rules for type equivalency and compatibility.
 */
 
#ifndef _H_ast_type
#define _H_ast_type

#include "ast.h"
#include "list.h"
#include <iostream>


class Type : public Node {
  friend class Semantic;
 protected:
  char *typeName;
  
 public :
  static Type *intType, *doubleType, *boolType, *voidType,
    *nullType, *stringType, *errorType;
  
 Type(yyltype loc) : Node(loc) {}
  Type(const char *str);
  
  virtual void PrintToStream(std::ostream& out) const { out << typeName; }
  friend std::ostream& operator<<(std::ostream& out, const Type *t) { t->PrintToStream(out); return out; }
  friend bool operator==(const Type &t1, const Type &t2);
  virtual bool IsEquivalentTo(Type *other) { return this == other; }
};

class NamedType : public Type {
  friend class Semantic;
 protected:
  Identifier *id;
  
 public:
  NamedType(Identifier *i);
  void PrintToStream(std::ostream& out) const { out << id; }
  friend bool operator==(const Type &t1, const Type &t2);
};

class ArrayType : public Type {
  friend class Semantic;
 protected:
  Type *elemType;
  
 public:
  ArrayType(yyltype loc, Type *elemType);
  void PrintToStream(std::ostream& out) const { out << elemType << "[]"; }
  friend bool operator==(const Type &t1, const Type &t2);
};

bool operator==(const Type &t1, const Type &t2);
bool operator!=(const Type &t1, const Type &t2);
 
#endif
