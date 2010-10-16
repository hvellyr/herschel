/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_annotate_h
#define bootstrap_annotate_h

#include "refcountable.h"

namespace heather
{
  class AptNode;
  class BlockNode;
  class CompileUnitNode;
  class DefNode;
  class FuncDefNode;
  class LetNode;
  class SymbolNode;
  class VardefNode;
  class ParamNode;
  class Scope;

  //------------------------------------------------------------------------------

  class Annotator : public RefCountable
  {
  public:
    Annotator();

    void annotateNode(AptNode* node, Scope* scope);

    void annotate(CompileUnitNode* node, Scope* scope);

    void annotate(AptNode* node, Scope* scope);
    void annotate(SymbolNode* node, Scope* scope);

    void annotate(DefNode* node, Scope* scope);
    void annotate(LetNode* node, Scope* scope);
    void annotate(BlockNode* node, Scope* scope);

    void annotate(ParamNode* node, Scope* scope);

  private:
    void annotate(VardefNode* node, Scope* scope, bool isLocal);
    void annotate(FuncDefNode* node, Scope* scope, bool isLocal);
  };

};                              // namespace

#endif                          // bootstrap_annotate_h
