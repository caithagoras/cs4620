/* File: ast_stmt.h
 * ----------------
 * The Stmt class and its subclasses are used to represent
 * statements in the parse tree.  For each statment in the
 * language (for, if, return, etc.) there is a corresponding
 * node class for that construct. 
 *
 * pp3: You will need to extend the Stmt classes to implement
 * semantic analysis for rules pertaining to statements.
 */


#ifndef _H_ast_stmt
#define _H_ast_stmt

#include "list.h"
#include "ast.h"

class Decl;
class VarDecl;
class Expr;
class Semantic;
  
class Program : public Node{
 protected:
  List<Decl*> *decls;
  friend class Semantic;
  
 public:
  Program(List<Decl*> *declList);
  void CheckAndEmit();
};

class Stmt : public Node {
 public:
 Stmt() : Node() {}
 Stmt(yyltype loc) : Node(loc) {}
  virtual ~Stmt() {}
};

class StmtBlock : public Stmt {
  friend class Semantic;

 protected:
  List<VarDecl*> *decls;
  List<Stmt*> *stmts;
    
 public:
  StmtBlock(List<VarDecl*> *variableDeclarations, List<Stmt*> *statements);
};

  
class ConditionalStmt : public Stmt {
  
 protected:
  Expr *test;
  Stmt *body;

 public:
  ConditionalStmt(Expr *testExpr, Stmt *body);
};

class LoopStmt : public ConditionalStmt {
  friend class Semantic;

 public:
 LoopStmt(Expr *testExpr, Stmt *body)
   : ConditionalStmt(testExpr, body) {}
};

class ForStmt : public LoopStmt {
  friend class Semantic;
 protected:
  Expr *init, *step;
  
 public:
  ForStmt(Expr *init, Expr *test, Expr *step, Stmt *body);
};

class WhileStmt : public LoopStmt {
  friend class Semantic;
 public:
 WhileStmt(Expr *test, Stmt *body) : LoopStmt(test, body) {}
};

class IfStmt : public ConditionalStmt {
  friend class Semantic;

 protected:
  Stmt *elseBody;
  
 public:
  IfStmt(Expr *test, Stmt *thenBody, Stmt *elseBody);
};

class BreakStmt : public Stmt {
 public:
 BreakStmt(yyltype loc) : Stmt(loc) {}
};

class ReturnStmt : public Stmt  
{
  friend class Semantic;
  protected:
    Expr *expr;
  
  public:
    ReturnStmt(yyltype loc, Expr *expr);
};

class PrintStmt : public Stmt
{
  friend class Semantic;
  protected:
    List<Expr*> *args;
    
  public:
    PrintStmt(List<Expr*> *arguments);
};


#endif