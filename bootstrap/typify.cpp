/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
*/

#include "typify.hpp"

#include "annotate.hpp"
#include "ast.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "port.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "scope.hpp"
#include "str.hpp"
#include "symbol.hpp"
#include "traverse.hpp"
#include "typectx.hpp"
#include "utils.hpp"
#include "xmlrenderer.hpp"

#include <set>


namespace herschel {

namespace {
  void trackMoveablePositions(std::shared_ptr<AstNode> node)
  {
    if (auto binding = std::dynamic_pointer_cast<MoveableBinding>(node)) {
      if (auto luser = binding->lastUser().lock()) {
        if (auto moveablend = std::dynamic_pointer_cast<MoveableReferrer>(luser)) {
          moveablend->setIsInMovePos(true);
        }
      }
    }
  }

  void generateFinalizers(Typifier* typf, std::shared_ptr<BlockNode> parent,
                          std::shared_ptr<AstNode> node)
  {
    HR_LOG() << "~~~~1";
    if (auto binding = std::dynamic_pointer_cast<MoveableBinding>(node)) {
      HR_LOG() << "~~~~2 " << node->type();
      if (node->type().isValueType()) {
        auto deallocExpr = makeApplyNode(
            node->scope(), node->srcpos(),
            makeSymbolNode(node->scope(), node->srcpos(), String("deallocate")));
        deallocExpr->appendNode(node);

        std::shared_ptr<AstNode> expr;
        {
          Annotator an{typf->fCompiler};
          expr = an.annotateNode(deallocExpr);
        }

        typf->typifyNode(expr);

        parent->appendNode(expr);
      }
    }
  }
}  // namespace


template <typename T>
struct NodeTypifier {
  static void typify(Typifier* typf, T node)
  {
    typf->typifyNodeList(node->child_nodes());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SymbolNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SymbolNode> node)
  {
    auto var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                              K(showAmbiguousSymDef));
    if (var) {
      if (auto bindnd = dynamic_cast<const MoveableBinding*>(var)) {
        const_cast<MoveableBinding*>(bindnd)->setLastUser(node);
      }
      else if (auto fdn = dynamic_cast<const FuncDefNode*>(var))
        node->setLinkage(fdn->linkage());

      node->setType(var->type());
      return;
    }

    Type type0 = node->scope()->lookupType(node->name(), K(showAmbiguousSymDef));
    Type type1 = node->scope()->normalizeType(type0);

    Type type2 = degeneralizeType(node->srcpos(), type1, node->generics());
    if (type2.isDef())
      node->setType(Type::makeClassTypeOf(type2));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ArrayTypeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ArrayTypeNode> node)
  {
    typf->typifyNode(node->typeNode());

    auto symnd = std::dynamic_pointer_cast<SymbolNode>(node->typeNode());
    auto type = (symnd ? symnd->type() : node->typeNode()->type());
    auto type1 = node->scope()->normalizeType(type);
    node->setType(Type::makeArray(type1, 0, K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<TypeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<TypeNode> node)
  {
    hr_assert(node->type().isDef());
    Type type1 = node->scope()->normalizeType(node->type());
    node->setType(type1);
  }
};


template <>
struct NodeTypifier<std::shared_ptr<DefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<DefNode> node)
  {
    typf->typifyNode(node->defNode());
    node->setType(node->defNode()->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<LetNode>> {
  static void typify(Typifier* typf, std::shared_ptr<LetNode> node)
  {
    typf->typifyNode(node->defNode());
    if (auto nd = std::dynamic_pointer_cast<DelayTypeAnnotatable>(node->defNode())) {
      if (nd->isTypeSpecDelayed())
        return;
    }
    node->setType(node->defNode()->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<VardefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<VardefNode> node)
  {
    if (node->initExpr())
      typf->typifyNode(node->initExpr());

    if (!node->isTypeSpecDelayed())
      typf->setupBindingNodeType(node, "variable");
  }
};


template <>
struct NodeTypifier<std::shared_ptr<FuncDefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<FuncDefNode> node)
  {
    typf->typifyNodeList(node->params());
    if (node->body()) {
      typf->typifyNode(node->body());

      if (!node->body()->type().isDef())
        node->body()->setType(Type::makeAny());
    }

    typf->setupFunctionNodeType(node);

    if (node->name() == Names::kAppMain) {
      if (!node->retType().isAny()) {
        if (node->retType().typeId() != Names::kInt32TypeName) {
          HR_LOG(kError, node->srcpos(), E_TypeMismatch)
              << "return type of " MID_app_main "() must be " MID_Int32TypeName;
          HR_LOG(kError, node->srcpos(), E_TypeMismatch)
              << "found to be: " << node->retType();
        }
      }

      node->setRetType(Type::makeTypeRef(MID_Int32TypeName));
    }

    if (node->body()) {
      if (node->isMethod())
        typf->enforceAtomTypeConv(node->body(), node->retType());
      else
        typf->annotateTypeConv(node->body(), node->retType());
      typf->setBodyLastDstType(node->body(), node->retType());
    }

    for (auto c : node->params()) {
      trackMoveablePositions(c);
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<FunctionNode>> {
  static void typify(Typifier* typf, std::shared_ptr<FunctionNode> node)
  {
    typf->typifyNodeList(node->params());
    if (node->body()) {
      typf->typifyNode(node->body());

      if (!node->body()->type().isDef())
        node->body()->setType(Type::makeAny());
    }

    typf->setupFunctionNodeType(node);
    if (node->body())
      typf->annotateTypeConv(node->body(), node->retType());

    for (auto c : node->params()) {
      trackMoveablePositions(c);
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SlotdefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SlotdefNode> node)
  {
    // TODO
  }
};


template <>
struct NodeTypifier<std::shared_ptr<BlockNode>> {
  static void typify(Typifier* typf, std::shared_ptr<BlockNode> node)
  {
    hr_assert(!node->children().empty());

    typf->typifyNodeList(node->children());

    node->setType(node->children().back()->type());

    for (auto c : node->children()) {
      if (auto letnd = std::dynamic_pointer_cast<LetNode>(c)) {
        trackMoveablePositions(letnd->defNode());
      }
    }

    for (auto c : node->children()) {
      if (auto letnd = std::dynamic_pointer_cast<LetNode>(c)) {
        generateFinalizers(typf, node, letnd->defNode());
      }
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ParamNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ParamNode> node)
  {
    if (node->initExpr())
      typf->typifyNode(node->initExpr());

    typf->setupBindingNodeType(node, "parameter");

    if (node->type().isDef()) {
      if (node->isSpecArg()) {
        if (node->type().isPlainType())
          node->setTypeConv(kAtom2PlainConv);
        else if (node->type().isAny())
          node->setTypeConv(kTypeCheckConv);
        // else
        //   ; //OK
      }
      node->setDstType(node->type());
    }
  }
};


namespace {
  std::vector<FunctionParameter> typesForArgs(const NodeList& args)
  {
    std::vector<FunctionParameter> result;
    for (auto& arg : args) {
      if (auto keyedParam = std::dynamic_pointer_cast<KeyargNode>(arg)) {
        result.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                           keyedParam->key(), arg->type()));
      }
      else {
        result.push_back(FunctionParameter(FunctionParameter::kParamPos, !K(isSpec),
                                           String(), arg->type()));
      }
    }
    return result;
  }
}  // namespace


template <>
struct NodeTypifier<std::shared_ptr<ApplyNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ApplyNode> node)
  {
    typf->typifyNode(node->base());
    typf->typifyNodeList(node->children());

    if (node->isSimpleCall()) {
      auto funcNode =
          node->scope()->lookupFunction(node->simpleCallName(), K(showAmbiguousSymDef));

      if (funcNode) {
        if (funcNode->hasSpecializedParams()) {
          typf->reorderArguments(node, funcNode.get());
          typf->typifyNodeList(node->children());

          Type type = typf->typifyMatchAndCheckParameters(
              node->srcpos(), node->children(), funcNode.get(), node->simpleCallName());
          if (type.isDef())
            node->setType(type);

          if (node->simpleCallName() == Names::kLangAllocateArray) {
            typf->checkAllocateArraySignature(node);
          }

          typf->flattenArguments(node);

          node->setFunSign(funcNode->type().functionSignature());
        }
        else {
          if (auto bestFuncNode = node->scope()->lookupBestFunctionOverload(
                  node->simpleCallName(), typesForArgs(node->children()), node->srcpos(),
                  K(showAmbiguousSymDef))) {
            auto newBase =
                makeSymbolNode(node->scope(), node->srcpos(), bestFuncNode.fName);
            newBase->setRefersTo(kFunction, !K(isShared));
            typf->typifyNode(newBase);
            node->setBase(newBase);
            node->setRefFunction(bestFuncNode.fNode);

            // it could be that the function is only defined later ...
            if (!bestFuncNode.fNode->type().isFunction()) {
              typf->typifyNode(bestFuncNode.fNode);
            }
            node->setFunSign(bestFuncNode.fNode->type().functionSignature());

            typf->reorderArguments(node, bestFuncNode.fNode.get());
            typf->typifyNodeList(node->children());

            Type type = typf->typifyMatchAndCheckParameters(
                node->srcpos(), node->children(), bestFuncNode.fNode.get(),
                node->simpleCallName());
            if (type.isDef())
              node->setType(type);

            if (node->simpleCallName() == Names::kLangAllocateArray) {
              typf->checkAllocateArraySignature(node);
            }

            typf->flattenArguments(node);
          }
          else if (node->isRemoveable()) {
            node->setIsObsolete(true);
          }
          else {
            HR_LOG(kError, node->srcpos(), E_NoMatchingFunction)
                << "no matching function: " << node->simpleCallName();
          }
        }
      }
      else {
        auto varNode = node->scope()->lookupVarOrFunc(
            node->srcpos(), node->simpleCallName(), K(showAmbiguousSymDef));
        if (varNode) {
          if (varNode->type().isFunction()) {
            node->base()->setType(varNode->type());
            node->setType(varNode->type().functionSignature().returnType());
            // TODO: check function signature of the function type
            node->setFunSign(varNode->type().functionSignature());
          }
          else if (varNode->type().isClassTypeOf()) {
            auto newObjAllocExpr = makeApplyNode(
                node->scope(), node->srcpos(),
                makeSymbolNode(node->scope(), node->srcpos(), Names::kLangAllocate));
            newObjAllocExpr->appendNode(
                makeTypeNode(node->scope(), node->srcpos(), varNode->type()));

            //---
            std::shared_ptr<AstNode> funcNode;
            if (varNode->type().isOpen()) {
              auto apply = makeApplyNode(
                  node->scope(), node->srcpos(),
                  makeSymbolNode(node->scope(), node->srcpos(), Names::kLangInitFunctor));
              apply->appendNode(
                  makeTypeNode(node->scope(), node->srcpos(), varNode->type()));
              funcNode = apply;
            }
            else {
              String initName =
                  qualifyId(varNode->type().typeName(), Names::kInitFuncName);
              funcNode = makeSymbolNode(node->scope(), node->srcpos(), initName);
            }

            auto initExpr = makeApplyNode(node->scope(), node->srcpos(), funcNode);
            initExpr->appendNode(newObjAllocExpr);

            std::shared_ptr<AstNode> createNode;

            {
              Annotator an{typf->fCompiler};
              createNode = an.annotateNode(initExpr);
            }

            typf->typifyNode(createNode);

            // TODO: check function signature of the type constructor
            node->setBase(createNode);
            node->setType(createNode->type());
            // node->setFunSign(varNode->type().functionSignature());
          }
          else {
            HR_LOG(kError, node->srcpos(), E_NoCallable)
                << "Non callable in function call context";
          }
        }
        else if (node->isRemoveable()) {
          node->setIsObsolete(true);
        }
        else {
          HR_LOG(kError, node->srcpos(), E_UnknownSymbol)
              << "unknown symbol: " << node->simpleCallName();
        }
      }
    }
    else {
      if (auto typeNode = std::dynamic_pointer_cast<ArrayTypeNode>(node->base())) {
        node->setType(typeNode->type());
      }
      else if (auto symNode = std::dynamic_pointer_cast<SymbolNode>(node->base())) {
        node->setType(symNode->type());
      }
      else if (auto typeNode = std::dynamic_pointer_cast<TypeNode>(node->base())) {
        node->setType(typeNode->type());
      }
      else if (auto funNode = std::dynamic_pointer_cast<FunctionNode>(node->base())) {
        node->setType(funNode->type());
      }
      else if (auto applyNode = std::dynamic_pointer_cast<ApplyNode>(node->base())) {
        if (applyNode->type().isFunction()) {
          node->setType(applyNode->type().functionSignature().returnType());
        }
        else {
          node->setType(applyNode->type());
        }
      }
      else if (auto srNode = std::dynamic_pointer_cast<SlotRefNode>(node->base())) {
        node->setType(srNode->type());
      }
      else {
        // XmlRenderer out{std::make_shared<FilePort>(stderr)};
        // out.render(node->base());
        hr_invalid("Unhandled apply base node");
      }
    }

    typf->annotateTypeConv(node, node->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<WeakNode>> {
  static void typify(Typifier* typf, std::shared_ptr<WeakNode> node)
  {
    typf->typifyNodeList(node->child_nodes());
    if (node->isObsolete())
      node->reset();
  }
};


template <>
struct NodeTypifier<std::shared_ptr<AssignNode>> {
  static void typify(Typifier* typf, std::shared_ptr<AssignNode> node)
  {
    typf->typifyNode(node->rvalue());

    if (node->isTypeSpecDelayed()) {
      auto symNode = std::dynamic_pointer_cast<SymbolNode>(node->lvalue());
      hr_assert(symNode);

      auto var = node->scope()->lookupVarOrFunc(symNode->srcpos(), symNode->name(),
                                                K(showAmbiguousSymDef));
      auto vardefNode = const_cast<VardefNode*>(dynamic_cast<const VardefNode*>(var));
      hr_assert(vardefNode);

      if (vardefNode->type().isUnion()) {
        TypeVector newUnionTypes;
        for (auto ty : vardefNode->type().unionTypes()) {
          if (ty.isAny()) {
            newUnionTypes.push_back(node->rvalue()->type());
          }
          else {
            newUnionTypes.push_back(ty);
          }
        }

        auto newTy = Type::makeUnion(newUnionTypes, vardefNode->type().isValueType());

        symNode->setType(newTy);
        vardefNode->setType(newTy);
      }
      else {
        symNode->setType(node->rvalue()->type());
        vardefNode->setType(node->rvalue()->type());
      }

      node->setTypeSpecDelayed(false);
      vardefNode->setTypeSpecDelayed(false);
    }
    else
      typf->typifyNode(node->lvalue());

    Type ltype = node->lvalue()->type();
    Type rtype = node->rvalue()->type();

    if (!ltype.isDef()) {
      // infer the vardef type from rvalue expression
      node->lvalue()->setType(node->rvalue()->type());
    }
    else if (!rtype.isDef()) {
      HR_LOG(kError, node->rvalue()->srcpos(), E_TypeMismatch)
          << "Undefined type in assignment right hand value";
    }
    else if (!isCovariant(ltype, rtype, *node->scope(), node->srcpos()) &&
             !containsAny(rtype, node->srcpos())) {
      HR_LOG(kError, node->rvalue()->srcpos(), E_TypeMismatch)
          << "type mismatch in assignment: " << ltype.typeId() << " <- " << rtype;
    }
    else if (!ltype.isDef()) {
      // infer the vardef type from rvalue expression
      node->lvalue()->setType(rtype);
    }

    node->setType(rtype);

    typf->annotateTypeConv(node->rvalue(), ltype);
    typf->annotateTypeConv(node->lvalue(), ltype);
    typf->annotateTypeConv(node, ltype);
  }
};


template <>
struct NodeTypifier<std::shared_ptr<BinaryNode>> {
  static void typify(Typifier* typf, std::shared_ptr<BinaryNode> node)
  {
    Type retty;

    typf->typifyNode(node->left());
    typf->typifyNode(node->right());

    auto leftty = node->left()->type();
    auto rightty = node->right()->type();

    switch (node->op()) {
    case kOpInvalid: hr_invalid("type not determined yet");

    case kOpPlus:
    case kOpMinus:
    case kOpMultiply:
    case kOpDivide:
    case kOpMod:
    case kOpRem:
    case kOpExponent:
      if (leftty.isAny() || rightty.isAny()) {
        node->setType(Type::makeAny());
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }
      if (leftty.isNumber() || rightty.isNumber()) {
        node->setType(Type::makeTypeRef(Names::kNumberTypeName, K(isValue)));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isComplex() || rightty.isComplex()) {
        node->setType(Type::makeTypeRef(Names::kComplexTypeName, K(isValue)));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isAnyFloat()) {
        if (rightty.isAnyFloat())
          node->setType(maxFloatType(leftty, rightty));
        else
          node->setType(leftty);
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }
      if (rightty.isAnyFloat()) {
        if (leftty.isAnyFloat())
          node->setType(maxFloatType(leftty, rightty));
        else
          node->setType(rightty);
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isRational() || rightty.isRational()) {
        node->setType(Type::makeTypeRef(Names::kRationalTypeName, K(isValue)));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isAnyInt() && rightty.isAnyInt()) {
        node->setType(maxIntType(leftty, rightty));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }


      if (typf->checkBinaryFunctionCall(node, typf->operatorNameByOp(node->op()),
                                        node->left(), node->right()))
        return;

      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in binary operation (" << leftty << " and " << rightty
          << ")";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpEqual:
    case kOpGreater:
    case kOpGreaterEqual:
    case kOpIn:
    case kOpLess:
    case kOpLessEqual:
    case kOpUnequal:
      if (leftty.isAny() || rightty.isAny() ||
          (leftty.isAnySignedInt() && rightty.isAnySignedInt()) ||
          (leftty.isAnyUInt() && rightty.isAnyUInt()) ||
          (leftty.isAnyFloat() && rightty.isAnyFloat()) ||
          (leftty.isRational() && rightty.isRational()) ||
          (leftty.isComplex() && rightty.isComplex()) ||
          (leftty.isNumber() && rightty.isNumber()) ||
          isSameType(leftty, rightty, *node->scope(), node->srcpos())) {
        // any is always ok.
        node->setType(Type::makeBool());
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }

      // TODO: check that left and right type are comparable
      HR_LOG(kError) << "compare left: " << leftty;
      HR_LOG(kError) << "compare right" << rightty;
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in binary comparison";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpCompare:
      if (leftty.isAny() || rightty.isAny() ||
          (leftty.isAnySignedInt() && rightty.isAnySignedInt()) ||
          (leftty.isAnyUInt() && rightty.isAnyUInt()) ||
          (leftty.isAnyFloat() && rightty.isAnyFloat()) ||
          (leftty.isRational() && rightty.isRational()) ||
          (leftty.isComplex() && rightty.isComplex()) ||
          (leftty.isNumber() && rightty.isNumber()) ||
          isSameType(leftty, rightty, *node->scope(), node->srcpos())) {
        node->setType(Type::makeInt32());
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }

      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in binary comparison (compare)";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpIsa:
      if (rightty.isAny() || rightty.isClassTypeOf()) {
        node->setType(Type::makeBool());
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }
      // TODO: try to lookup a method which enables isa(leftty, rightty) and
      // use it's returntype
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible right side type in isa operation";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpConcat:
      if (leftty.isString() || leftty.isAny()) {
        if (rightty.isString() || rightty.isChar() || rightty.isAny()) {
          node->setType(leftty);
          typf->annotateTypeConv(node->left(), leftty);
          typf->annotateTypeConv(node->right(), leftty);
          typf->annotateTypeConv(node, node->type());
          return;
        }
      }
      // TODO: try to lookup a method which enables append(leftty, rightty)
      // and use it's returntype
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in append() operation";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpFold:
      if (leftty.isString() || leftty.isAny()) {
        // accept everything on the right hand side
        node->setType(leftty);
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }
      // TODO: try to lookup a method which enables fold(leftty, rightty) and
      // use it's returntype
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in fold() operation";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpLogicalAnd:
    case kOpLogicalOr:
      if (leftty.isBool() && rightty.isBool()) {
        node->setType(Type::makeBool());
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "bool types required in logical 'and'/'or' operations";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpBitAnd:
    case kOpBitOr:
    case kOpBitXor:
      if ((leftty.isAnyUInt() || leftty.isAny()) &&
          (rightty.isAnyUInt() || rightty.isAny())) {
        node->setType(leftty);
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
      }
      else {
        HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
            << "AND, OR, XOR operations require unsigned integer types on both sides";
        node->setType(Type::makeAny());
      }
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpShiftLeft:
    case kOpShiftRight:
      if (leftty.isAnyUInt() || leftty.isAny()) {
        if (rightty.isAnyInt() || rightty.isAny()) {
          node->setType(leftty);
          typf->annotateTypeConv(node->left(), leftty);
          typf->annotateTypeConv(node->right(), leftty);
        }
        else {
          HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
              << "bit operations require integer types on right side";
          node->setType(Type::makeAny());
        }
      }
      else {
        HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
            << "bit operations require unsigned integer types on left side";
        node->setType(Type::makeAny());
      }
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpMapTo:
    case kOpRange:
    case kOpBy:
    case kOpAs:
    case kOpAssign: hr_invalid("not handled operators?");
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SlotRefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SlotRefNode> node)
  {
    typf->typifyNode(node->base());

    Type basety =
        (node->base()->type().isDef()
             ? node->scope()->lookupType(node->base()->type())
             : node->scope()->lookupType(Names::kAnyTypeName, K(showAmbiguousSymDef)));

    if (!basety.isDef()) {
      String typenm = (node->base()->type().isDef() ? node->base()->type().typeId()
                                                    : Names::kAnyTypeName);
      HR_LOG(kError, node->srcpos(), E_UndefinedType)
          << "undefined type '" << typenm << "'";

      node->setType(Type::makeAny());
      node->setDstType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
    }
    else {
      if (basety.isRecord()) {
        Type slotType = basety.slotType(node->slotName(), *node->scope());
        if (slotType.isDef()) {
          node->setType(slotType);
          node->setDstType(slotType);
          typf->annotateTypeConv(node, node->type());
        }
        else {
          HR_LOG(kError, node->srcpos(), E_UnknownSlot)
              << "reference to unknown slot '" << node->slotName()
              << "' in type: " << basety;
          node->setType(Type::makeAny());
          node->setDstType(Type::makeAny());
          typf->annotateTypeConv(node, node->type());
        }
      }
      else if (!node->base()->isRemoveable()) {
        HR_LOG(kError, node->srcpos(), E_SlotRefToNonClass)
            << "reference to slot '" << node->slotName() << "' is non-record type";
        node->setType(Type::makeAny());
        node->setDstType(Type::makeAny());
        typf->annotateTypeConv(node, node->type());
      }
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<UnaryNode>> {
  static void typify(Typifier* typf, std::shared_ptr<UnaryNode> node)
  {
    typf->typifyNode(node->base());

    switch (node->op()) {
    case kUnaryOpNegate: node->setType(node->base()->type()); break;

    case kUnaryOpNot:
      if (!node->base()->type().isDef()) {
        HR_LOG(kError, node->srcpos(), E_BoolTypeExpected)
            << "bool expected for not operator";
        node->setType(Type::makeAny());
      }
      else if (node->base()->type().isBool()) {
        node->setType(node->base()->type());
      }
      else if (node->base()->type().isAny()) {
        node->setType(Type::makeBool());
      }
      else {
        HR_LOG(kError, node->srcpos(), E_BoolTypeExpected)
            << "bool expected for not operator";
        node->setType(Type::makeAny());
      }
      typf->annotateTypeConv(node->base(), Type::makeBool());
      break;

    case kUnaryOpInvalid: hr_invalid("unhandled unary operator"); break;
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<IfNode>> {
  static void typify(Typifier* typf, std::shared_ptr<IfNode> node)
  {
    typf->typifyNode(node->test());
    typf->annotateTypeConv(node->test(), Type::makeBool());

    typf->typifyNode(node->consequent());
    if (node->alternate())
      typf->typifyNode(node->alternate());

    if (node->alternate()) {
      Type cotype = node->consequent()->type();
      Type alttype = node->alternate()->type();

      if (isCovariant(cotype, alttype, *node->scope(), node->srcpos())) {
        node->setType(alttype);
        typf->annotateTypeConv(node->consequent(), alttype);
        typf->annotateTypeConv(node->alternate(), alttype);
        typf->annotateTypeConv(node, alttype);
      }
      else if (isCovariant(alttype, cotype, *node->scope(), node->srcpos())) {
        node->setType(cotype);
        typf->annotateTypeConv(node->consequent(), cotype);
        typf->annotateTypeConv(node->alternate(), cotype);
        typf->annotateTypeConv(node, cotype);
      }
      else {
        // if the if expression is not in tail position, the branch type
        // mismatch doesn't matter.
        if (node->isInTailPos() || node->isSingleTypeRequired()) {
          HR_LOG(kError, node->srcpos(), E_IfConsqTypeMismatch)
              << "types for if consequent and alternate branch do not match";
        }
        node->setType(Type::makeAny(K(isValue)));
        typf->annotateTypeConv(node->consequent(), Type::makeAny());
        typf->annotateTypeConv(node->alternate(), Type::makeAny());
        typf->annotateTypeConv(node, Type::makeAny());
      }
    }
    else {
      if (node->isInTailPos() || node->isSingleTypeRequired()) {
        // if the if expression is in tail position we should definitely have
        // have an alternate branch.
        HR_LOG(kError, node->srcpos(), E_IfAltTypeMismatch)
            << "unspecified alternate branch do not match type with consequent";
        node->setType(Type::makeAny(K(isValue)));
      }
      else {
        node->setType(node->consequent()->type());
      }
      typf->annotateTypeConv(node->consequent(), node->type());
      typf->annotateTypeConv(node, node->type());
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<KeyargNode>> {
  static void typify(Typifier* typf, std::shared_ptr<KeyargNode> node)
  {
    typf->typifyNode(node->value());
    node->setType(node->value()->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SelectNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SelectNode> node)
  {
    // TODO
    typf->typifyNode(node->test());
    if (node->comparator())
      typf->typifyNode(node->comparator());

    for (size_t i = 0; i < node->mappings().size(); i++) {
      if (node->mappings()[i].fTestValues.empty()) {
        typf->typifyNode(node->mappings()[i].fConsequent);
      }
      else {
        for (size_t j = 0; j < node->mappings()[i].fTestValues.size(); j++)
          typf->typifyNode(node->mappings()[i].fTestValues[j]);
      }
      typf->typifyNode(node->mappings()[i].fConsequent);
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<RangeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<RangeNode> node)
  {
    typf->typifyNode(node->from());
    typf->typifyNode(node->to());
    if (node->by()) {
      typf->typifyNode(node->by());
    }

    Type fromType = node->from()->type();
    Type toType = node->to()->type();
    bool fromIsOpen = fromType.isOpen();
    bool toIsOpen = toType.isOpen();

    if ((fromIsOpen || toIsOpen) && (fromIsOpen != toIsOpen)) {
      HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch)
          << "partial open types in range declaration defeats generics usage";
      node->setType(makeRangeType(Type::makeAny(K(isValue))));
      return;
    }

    if (!isSameType(fromType, toType, *node->scope(), node->srcpos())) {
      HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch) << "type of range is ambiguous";
      node->setType(makeRangeType(Type::makeAny(K(isValue))));
      return;
    }

    if (node->by()) {
      Type byType = node->by()->type();
      bool byIsOpen = byType.isOpen();

      if (byIsOpen && byIsOpen != fromIsOpen) {
        HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch)
            << "partial open types in range declaration defeats generics usage";
        node->setType(makeRangeType(Type::makeAny(K(isValue))));
        return;
      }

      if (!isSameType(fromType, byType, *node->scope(), node->by()->srcpos())) {
        HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch)
            << "step type does not match range type";
        node->setType(makeRangeType(Type::makeAny(K(isValue))));
        return;
      }
    }

    node->setType(makeRangeType(node->from()->type()));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<WhileNode>> {
  static void typify(Typifier* typf, std::shared_ptr<WhileNode> node)
  {
    typf->typifyNode(node->test());
    typf->typifyNode(node->body());

    typf->annotateTypeConv(node->test(), Type::makeBool());

    if (node->isInTailPos() || node->isSingleTypeRequired()) {
      // the while expression should never be in tail position
      HR_LOG(kWarn, node->srcpos(), E_WhileTypeMismatch)
          << "while in tail position enforces Any type";
      node->setType(Type::makeAny(K(isValue)));
    }
    else
      node->setType(node->body()->type());

    typf->annotateTypeConv(node->body(), node->type());
  }
};


static bool mapCommonType(Type& resultType, AstNode& node)
{
  Type ty0 = node.type();
  if (!resultType.isDef()) {
    resultType = ty0;
  }
  else if (!isSameType(ty0, resultType, *node.scope(), node.srcpos())) {
    resultType = Type::makeAny(K(isValue));
    return false;
  }

  return true;
}


template <>
struct NodeTypifier<std::shared_ptr<VectorNode>> {
  static void typify(Typifier* typf, std::shared_ptr<VectorNode> node)
  {
    NodeList& nl = node->children();
    typf->typifyNodeList(nl);

    Type valueType;
    TypeVector generics;
    for (size_t i = 0; i < nl.size(); i++) {
      if (!mapCommonType(valueType, *nl[i]))
        break;
    }

    if (!valueType.isDef())
      valueType = Type::makeAny(K(isValue));

    for (auto& nd : nl)
      typf->annotateTypeConv(nd, valueType);

    node->setType(Type::makeTypeRef(Names::kVectorTypeName, makeVector(valueType),
                                    TypeConstVector(), K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<DictNode>> {
  static void typify(Typifier* typf, std::shared_ptr<DictNode> node)
  {
    Type keyType;
    Type valueType;

    NodeList& nl = node->children();
    for (size_t i = 0; i < nl.size(); i++) {
      auto pair = dynamic_cast<BinaryNode*>(nl[i].get());
      hr_assert(pair);
      hr_assert(pair->op() == kOpMapTo);

      typf->typifyNode(pair->left());
      typf->typifyNode(pair->right());

      mapCommonType(keyType, *pair->left());
      mapCommonType(valueType, *pair->right());
    }

    if (!keyType.isDef())
      keyType = Type::makeAny(K(isValue));
    if (!valueType.isDef())
      valueType = Type::makeAny(K(isValue));

    for (size_t i = 0; i < nl.size(); i++) {
      auto pair = dynamic_cast<BinaryNode*>(nl[i].get());
      hr_assert(pair);
      hr_assert(pair->op() == kOpMapTo);
      typf->annotateTypeConv(pair->right(), valueType);
      typf->annotateTypeConv(pair->left(), keyType);
    }

    node->setType(Type::makeTypeRef(Names::kMapTypeName, makeVector(keyType, valueType),
                                    TypeConstVector(), K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ArrayNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ArrayNode> node)
  {
    NodeList& nl = node->children();
    typf->typifyNodeList(nl);

    Type valueType;
    TypeVector generics;
    for (auto& nd : nl) {
      if (!mapCommonType(valueType, *nd))
        break;
    }

    if (!valueType.isDef())
      valueType = Type::makeAny(K(isValue));

    for (auto& nd : nl)
      typf->annotateTypeConv(nd, valueType);

    node->setType(Type::makeArray(valueType, nl.size(), K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<CastNode>> {
  static void typify(Typifier* typf, std::shared_ptr<CastNode> node)
  {
    typf->typifyNode(node->base());

    if (!node->type().isOpen()) {
      Type type = node->scope()->lookupType(node->type());
      if (!type.isDef()) {
        HR_LOG(kError, node->srcpos(), E_UndefinedType)
            << "undefined type '" << node->type() << "'";
        node->setType(Type::makeAny(K(isValue)));
      }
      else {
        if (isInvariant(node->base()->type(), type, *node->scope(), node->srcpos())) {
          HR_LOG(kError, node->srcpos(), E_InvariantType) << "Cast to invariant type";
          node->setType(Type::makeAny(K(isValue)));
        }
        else
          node->setType(type);
      }
    }

    typf->annotateTypeConv(node->base(), node->type());
  }
};


static void typifyNodeType(std::shared_ptr<AstNode> node, const Type& type,
                           const String& defaultTypeName, bool maybeImaginary)
{
  Type ty =
      (type.isDef() ? node->scope()->lookupType(type)
                    : node->scope()->lookupType(defaultTypeName, K(showAmbiguousSymDef)));

  if (!ty.isDef()) {
    HR_LOG(kError, node->srcpos(), E_UndefinedType)
        << "undefined type '" << defaultTypeName << "'";
    node->setType(Type::makeAny(K(isValue)));
  }
  else {
    if (maybeImaginary && ty.isAnyNumber()) {
      auto nnd = std::dynamic_pointer_cast<BaseNumberNode>(node);
      if (nnd)
        ty.setIsImaginary(nnd->isImaginary());
    }
    node->setType(ty);
  }
}


template <>
struct NodeTypifier<std::shared_ptr<BoolNode>> {
  static void typify(Typifier* typf, std::shared_ptr<BoolNode> node)
  {
    typifyNodeType(node, Type(), Names::kBoolTypeName, !K(maybeImaginary));
    typf->annotateTypeConv(node, node->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<CharNode>> {
  static void typify(Typifier* typf, std::shared_ptr<CharNode> node)
  {
    typifyNodeType(node, Type(), Names::kCharTypeName, !K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<RationalNode>> {
  static void typify(Typifier* typf, std::shared_ptr<RationalNode> node)
  {
    typifyNodeType(node, node->type(), Names::kRationalTypeName, K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<RealNode>> {
  static void typify(Typifier* typf, std::shared_ptr<RealNode> node)
  {
    typifyNodeType(node, node->type(), Names::kFloat32TypeName, K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<IntNode>> {
  static void typify(Typifier* typf, std::shared_ptr<IntNode> node)
  {
    typifyNodeType(node, node->type(), Names::kInt32TypeName, K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<StringNode>> {
  static void typify(Typifier* typf, std::shared_ptr<StringNode> node)
  {
    typifyNodeType(node, Type(), Names::kStringTypeName, !K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<KeywordNode>> {
  static void typify(Typifier* typf, std::shared_ptr<KeywordNode> node)
  {
    typifyNodeType(node, Type(), Names::kKeywordTypeName, !K(maybeImaginary));
  }
};


//------------------------------------------------------------------------------

Typifier::Typifier(Compiler& compiler)
    : fCompiler(compiler)
{
}


void Typifier::typifyNode(std::shared_ptr<AstNode> node)
{
  dispatchNode<void>(node,
                     [&](auto nd) { NodeTypifier<decltype(nd)>::typify(this, nd); });
}


void Typifier::typifyNodeList(const NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    typifyNode(nl[i]);
}


//------------------------------------------------------------------------------

bool Typifier::isNodeCallToGenericFunction(const AstNode* node) const
{
  if (auto applyNode = dynamic_cast<const ApplyNode*>(node)) {
    if (applyNode->isSimpleCall()) {
      // special case lang|slice(array, idx)
      if (applyNode->simpleCallName() == Names::kLangSlice) {
        const NodeList& args = applyNode->children();
        if (args.size() == 2 && args[0]->type().isArray())
          return false;
      }
      // special case lang|slice!(array, idx, value)
      else if (applyNode->simpleCallName() == Names::kLangSliceX) {
        const NodeList& args = applyNode->children();
        if (args.size() == 3 && args[0]->type().isArray())
          return false;
      }
      else if (applyNode->simpleCallName() == Names::kLangIsaQ) {
        const NodeList& args = applyNode->children();
        if (args.size() == 2)
          return false;
      }

      auto funcNode = applyNode->scope()->lookupFunction(applyNode->simpleCallName(),
                                                         K(showAmbiguousSymDef));
      if (funcNode)
        return funcNode->hasSpecializedParams();
    }
  }
  return false;
}


void Typifier::annotateTypeConv(std::shared_ptr<AstNode> node, const Type& dstType)
{
  /*
    plain <- plain     (bitcast)
    plain <- atom      atom_2_x
    atom  <- atom      ok
    atom  <- any       type_check
    atom  <- plain     wrap_atom

    as a special check: if node is a function call to a generic function, it
    always has ATOM return representation, even if the type is plain.
   */
  bool isCallToGF = isNodeCallToGenericFunction(node.get());

  // this is a last resort stop hack, if either the node has not a valid type
  // or the dstType is not valid.  This may happen if we had parsing or typify
  // errors before.
  if (!node->type().isDef() || !dstType.isDef()) {
    node->setTypeConv(kTypeCheckConv);
    return;
  }

  if (dstType.isPlainType()) {
    if (isCallToGF) {
      // req. atom_2_x
      node->setTypeConv(kAtom2PlainConv);
    }
    else if (node->type().isPlainType()) {
      // ok.  TODO: maybe some bitcast between int/short/long, etc.
    }
    else {
      // req. atom_2_x
      node->setTypeConv(kAtom2PlainConv);
    }
  }
  else {
    if (isCallToGF) {
      // requires type_check
      node->setTypeConv(kTypeCheckConv);
    }
    else if (node->type().isPlainType()) {
      // req. wrap_atom
      node->setTypeConv(kPlain2AtomConv);
    }
    else if (node->type().isAny() || node->type().isClangAtom()) {
      // requires type_check
      node->setTypeConv(kTypeCheckConv);
    }
    else {
      // OK
    }
  }

  node->setDstType(dstType);
}


void Typifier::enforceAtomTypeConv(std::shared_ptr<AstNode> node, const Type& dstType)
{
  /*
    atom  <- atom      ok
    atom  <- any       type_check
    atom  <- plain     wrap_atom
   */
  bool isCallToGF = isNodeCallToGenericFunction(node.get());

  if (isCallToGF) {
    // requires type_check
    node->setTypeConv(kTypeCheckConv);
  }
  else if (node->type().isPlainType()) {
    // req. wrap_atom
    node->setTypeConv(kPlain2AtomConv);
  }
  else if (node->type().isAny() || node->type().isClangAtom()) {
    // requires type_check
    node->setTypeConv(kTypeCheckConv);
  }
  else {
    // OK
  }

  node->setDstType(dstType);
}


void Typifier::setBodyLastDstType(std::shared_ptr<AstNode> body, const Type& dstType)
{
  body->setDstType(dstType);
  if (auto block = std::dynamic_pointer_cast<BlockNode>(body)) {
    hr_assert(!block->children().empty());

    setBodyLastDstType(block->children().back(), dstType);
  }
}


void Typifier::setupBindingNodeType(std::shared_ptr<BindingNode> node, zstring errdesc)
{
  hr_assert(node->scope());

  if (node->type().isDef() && node->type().isOpen())
    return;

  String typenm = (node->type().isDef() ? node->type().typeId() : Names::kAnyTypeName);
  Type bindty =
      (node->type().isDef()
           ? node->scope()->lookupType(node->type())
           : node->scope()->lookupType(Names::kAnyTypeName, K(showAmbiguousSymDef)));
  if (!bindty.isDef()) {
    HR_LOG(kError, node->srcpos(), E_UndefinedType)
        << "undefined type '" << typenm << "' in " << errdesc;
    node->setType(Type::makeAny());
    if (node->initExpr()) {
      node->initExpr()->setDstType(Type::makeAny());
      annotateTypeConv(node->initExpr(), node->type());
    }
  }
  else {
    hr_assert(bindty.isDef());
    // if (bindty.isOpen()) {
    //   if (node->type().isDef())
    //     bindty = node->scope()->normalizeType(bindty, node->type());
    // }
    node->setType(bindty);

    if (node->initExpr()) {
      if (bindty.isAny()) {
        // infer the vardef type from the init expression
        node->setType(node->initExpr()->type());
        node->initExpr()->setDstType(node->initExpr()->type());
        annotateTypeConv(node->initExpr(), node->type());
      }
      else if (!node->initExpr()->type().isDef()) {
        HR_LOG(kError, node->initExpr()->srcpos(), E_TypeMismatch)
            << "Undefined type in %s initialization " << errdesc;
        node->initExpr()->setDstType(Type::makeAny());
      }
      else if (!isContravariant(bindty, node->initExpr()->type(), *node->scope(),
                                node->srcpos())) {
        HR_LOG(kError, node->initExpr()->srcpos(), E_TypeMismatch)
            << "type mismatch in %s initialization" << errdesc;
        node->initExpr()->setDstType(Type::makeAny());
      }
      else {
        annotateTypeConv(node->initExpr(), node->type());
      }
    }
  }
}


void Typifier::setupFunctionNodeType(std::shared_ptr<FunctionNode> node)
{
  FunctionParamVector params;
  for (size_t i = 0; i < node->params().size(); i++) {
    auto prmnd = std::dynamic_pointer_cast<ParamNode>(node->params()[i]);
    hr_assert(prmnd);

    if (prmnd->flags() == kPosArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamPos, !K(isSpec),
                                         String(), prmnd->type()));
    }
    else if (prmnd->flags() == kSpecArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamPos, K(isSpec),
                                         String(), prmnd->type()));
    }
    else if (prmnd->flags() == kNamedArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                         prmnd->key(), prmnd->type()));
    }
    else if (prmnd->flags() == kRestArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamRest, !K(isSpec),
                                         String(), prmnd->type()));
    }
    else {
      hr_invalid("undefined parameter type?");
    }
  }

  if (!node->retType().isDef() || node->retType().isAny()) {
    // try to infer the return type from the body's type
    if (node->body()) {
      node->setRetType(node->body()->type());
    }
    else {
      // TODO: make warnings like this optional
      // HR_LOG(kWarning, node->srcpos(), E_UndefinedType)
      //          << "undefined return type on function defaults to lang|Any";
      node->setRetType(Type::makeAny());
    }
  }
  else if (!node->retType().isOpen()) {
    String typenm =
        (node->retType().isDef() ? node->retType().typeId() : Names::kAnyTypeName);
    Type retty =
        (node->retType().isDef()
             ? node->scope()->lookupType(node->retType())
             : node->scope()->lookupType(Names::kAnyTypeName, K(showAmbiguousSymDef)));
    if (!retty.isDef()) {
      HR_LOG(kError, node->srcpos(), E_UndefinedType)
          << "undefined return type '" << typenm << "'";
      node->setRetType(Type::makeAny());
    }
    else {
      node->setRetType(retty);
    }
  }

  FunctionSignature sign(!K(isGeneric), String(), node->retType(), params);
  node->setType(Type::makeFunction(sign));
}


class FindReturnTraverseDelegate {
public:
  bool apply(std::shared_ptr<AstNode> node, TraversePhase phase)
  {
    if (phase == TraversePhase::before) {
      // don't dive into nested functions
      if (std::dynamic_pointer_cast<FunctionNode>(node))
        return false;

      auto apply = std::dynamic_pointer_cast<ApplyNode>(node);
      if (apply && apply->isSimpleCall() && apply->simpleCallName() == Names::kLangReturn)
        fReturns.push_back(apply);
    }
    return true;
  }

  std::vector<std::shared_ptr<AstNode>> fReturns;
};


void Typifier::checkFunctionReturnType(std::shared_ptr<FunctionNode> node)
{
  if (node->body()) {
    if (!isContravariant(node->retType(), node->body()->type(), *node->scope(),
                         node->srcpos()) &&
        !containsAny(node->body()->type(), node->srcpos())) {
      HR_LOG(kError, node->srcpos(), E_TypeMismatch)
          << "function's body type does not match its return type";
    }


    FindReturnTraverseDelegate delegate;
    Traversator<FindReturnTraverseDelegate>(delegate).traverseNode(node);

    for (auto ret : delegate.fReturns) {
      if (!isContravariant(node->retType(), ret->type(), *ret->scope(), ret->srcpos())) {
        HR_LOG(kError, ret->srcpos(), E_TypeMismatch)
            << "return's type does not match outer block type";
      }
    }
  }
}


void Typifier::checkArgParamType(TypeCtx& localCtx,
                                 const std::shared_ptr<ParamNode>& param,
                                 std::shared_ptr<AstNode> arg, int idx,
                                 const String& funcName)
{
  if (param->type().isOpen()) {
    if (!param->type().matchGenerics(localCtx, arg->type(), *arg->scope(),
                                     arg->srcpos())) {
      HR_LOG(kError) << "param: " << param->type();
      HR_LOG(kError) << "arg: " << arg->type();
      HR_LOG(kError, arg->srcpos(), E_TypeMismatch)
          << "type mismatch for argument " << idx << " in call to function '" << funcName
          << "'";
      return;
    }
  }
  else {
    if (!isContravariant(param->type(), arg->type(), *arg->scope(), arg->srcpos()) &&
        !containsAny(arg->type(), arg->srcpos())) {
      HR_LOG(kError, arg->srcpos(), E_TypeMismatch)
          << "type mismatch for argument " << idx << " in call to function '" << funcName
          << "'";
      return;
    }
  }

  // if the parameter is specialized enforce passing to AtomType
  if (param->flags() == kSpecArg)
    enforceAtomTypeConv(arg, param->type());
  else
    annotateTypeConv(arg, param->type());
}


Typifier::KeyargReturn Typifier::findKeyedArg(const NodeList& args, size_t argidx,
                                              const String& key)
{
  auto retval = KeyargReturn{nullptr, 0};

  for (size_t i = argidx; i < args.size(); i++) {
    auto keyarg = dynamic_cast<KeyargNode*>(args[i].get());
    if (!keyarg)
      return retval;

    if (keyarg->key() == key) {
      return KeyargReturn{keyarg, i};
    }
  }
  return retval;
}


void Typifier::reorderArguments(std::shared_ptr<ApplyNode> node,
                                const FunctionNode* funcNode)
{
  const NodeList& funcParams = funcNode->params();

  NodeList newArgs;
  newArgs.reserve(funcParams.size());

  NodeList posArgs;
  posArgs.reserve(node->children().size());
  std::map<String, std::shared_ptr<KeyargNode>> namedArgs;

  for (auto arg : node->children()) {
    if (auto keyArg = std::dynamic_pointer_cast<KeyargNode>(arg)) {
      namedArgs.insert(std::make_pair(keyArg->key(), keyArg));
    }
    else {
      posArgs.push_back(arg);
    }
  }

  size_t posArgIdx = 0;
  for (auto i = 0; i < funcParams.size(); ++i) {
    auto param = std::dynamic_pointer_cast<ParamNode>(funcParams[i]);

    if (param) {
      if (param->flags() == kPosArg || param->flags() == kSpecArg) {
        if (posArgIdx >= posArgs.size()) {
          // there was an error before.  Not enough positional arguments
          return;
        }
        newArgs.push_back(posArgs[posArgIdx]);
        ++posArgIdx;
      }
      else if (param->flags() == kNamedArg) {
        auto iArg = namedArgs.find(param->key());
        if (iArg != namedArgs.end()) {
          newArgs.push_back(iArg->second);
        }
        else {
          hr_assert(param->initExpr());
          newArgs.push_back(makeKeyargNode(param->scope(), param->srcpos(), param->key(),
                                           param->initExpr()));
        }
      }
      else if (param->flags() == kRestArg) {
        // TODO: create code to generate a array on stack, and assign
        // the remaining arguments to it; then pass the array as
        // single parameter.
        for (auto i = posArgIdx; i < posArgs.size(); ++i) {
          newArgs.push_back(posArgs[i]);
        }

        break;
      }
    }
  }

  node->replaceChildren(newArgs);
}


void Typifier::flattenArguments(std::shared_ptr<ApplyNode> node)
{
  NodeList newArgs;
  newArgs.reserve(node->children().size());

  for (auto arg : node->children()) {
    if (auto keyArg = std::dynamic_pointer_cast<KeyargNode>(arg)) {
      newArgs.push_back(keyArg->value());
    }
    else {
      newArgs.push_back(arg);
    }
  }

  node->replaceChildren(newArgs);
}


Type Typifier::typifyMatchAndCheckParameters(const SrcPos& srcpos, const NodeList& args,
                                             const FunctionNode* funcNode,
                                             const String& funcName)
{
  const NodeList& funcParams = funcNode->params();

  TypeCtx localCtx;
  size_t argidx = 0;
  std::set<int> argIndicesUsed;
  for (size_t i = 0; i < funcParams.size(); ++i) {
    auto param = std::dynamic_pointer_cast<ParamNode>(funcParams[i]);

    if (param) {
      if (param->flags() == kPosArg || param->flags() == kSpecArg) {
        auto arg = args[argidx];

        if (i >= args.size()) {
          HR_LOG(kError, srcpos, E_BadArgNumber) << "not enough arguments";
          return Type();
        }
        checkArgParamType(localCtx, param, arg, i, funcName);
        argidx++;
      }
      else if (param->flags() == kNamedArg) {
        auto keyval = findKeyedArg(args, argidx, param->key());
        if (!keyval.fKeyarg) {
          // I think this can't happen anymore.  Default parameter
          // values are rendered in place in the nodify (?) phase.
          hr_invalid("");
          if (param->initExpr()) {
            if (param->initExpr()->type().isOpen()) {
              Type iety = param->initExpr()->type().replaceGenerics(localCtx);

              if (iety.isDef())
                param->initExpr()->setType(iety);
              else
                HR_LOG(kError, srcpos, E_TypeMismatch)
                    << "init value to argument " << (int)i << " of function " << funcName
                    << " has unmatched generic type";
            }

            checkArgParamType(localCtx, param, param->initExpr(), i, funcName);
          }
        }
        else {
          if (keyval.fKeyarg->value()->type().isOpen()) {
            Type keyvalty = keyval.fKeyarg->value()->type().replaceGenerics(localCtx);
            if (auto applyNd =
                    std::dynamic_pointer_cast<ApplyNode>(keyval.fKeyarg->value())) {
              for (auto cNd : applyNd->children()) {
                Type cNdTy = cNd->type().replaceGenerics(localCtx);
                if (cNdTy.isDef())
                  cNd->setType(cNdTy);
              }
            }

            if (keyvalty.isDef())
              keyval.fKeyarg->value()->setType(keyvalty);
            else
              HR_LOG(kError, srcpos, E_TypeMismatch)
                  << "init value to argument " << (int)i << " of function " << funcName
                  << " has unmatched generic type";
          }

          checkArgParamType(localCtx, param, keyval.fKeyarg->value(), keyval.fIdx,
                            funcName);
          argIndicesUsed.insert(keyval.fIdx);
        }
      }
      else if (param->flags() == kRestArg) {
        // TODO: type of rest arguments
        // TypeVector types;
        // for (size_t j = 0; j < args.size(); j++) {
        //   if (argIndicesUser.find(j) != argIndicesUser.end()) {
        //     // argument not yet used
        //     types.push_back(args[j]->type());
        //   }
        // }
        // Type seq = Type::makeIntersection(types, true);
        argidx = args.size();
      }
      else {
        hr_invalid("");
      }
    }
  }

  // if (argidx < args.size()) {
  //   HR_LOG(kError, srcpos, E_BadArgNumber) << "Too much arguments";
  // }

  if (funcNode->retType().isOpen()) {
    Type retty = funcNode->retType().replaceGenerics(localCtx);

    if (retty.isDef())
      return retty.setIsValueType(funcNode->retType().isValueType());
    else
      HR_LOG(kError, srcpos, E_TypeMismatch)
          << "function has unmatched generic return type.";

    // Type retty = funcnode->retType();
    // if (localCtx.hasType(retty.typeName())) {
    //   node.setType(localCtx.lookupType(retty.typeName()));
    // }
    // else {
    //   HR_LOG(kError, node.srcpos(), E_TypeMismatch) <<
    //          "function has unmatched generic return type.";
    // }

    return Type();
  }
  else
    return funcNode->retType();
}


void Typifier::checkAllocateArraySignature(std::shared_ptr<ApplyNode> node)
{
  const NodeList& args = node->children();
  if (args.size() == 2) {
    hr_assert(node->type().isArray());

    Type arrayBaseType = node->type().arrayBaseType();
    if (arrayBaseType.isBaseType()) {
      // this is always ok.
    }
    else if (arrayBaseType.isType()) {
      // TODO: when we distinguish nullable types, this is only allowed if the
      // type is nullable
      HR_LOG(kError, node->srcpos(), E_ArrayReqDefaultCtor)
          << "Can't create array of Type base type without explicit initializer";
    }
  }
}


String Typifier::operatorNameByOp(OperatorType type) const
{
  switch (type) {
  case kOpBitAnd: return String(MID_bitand);
  case kOpBitOr: return String(MID_bitor);
  case kOpBitXor: return String(MID_bitxor);
  case kOpCompare: return String(MID_compare);
  case kOpConcat: return String(MID_concat);
  case kOpDivide: return String(MID_divide);
  case kOpEqual: return String(MID_equalq);
  case kOpExponent: return String(MID_exponent);
  case kOpFold: return String(MID_fold);
  case kOpGreater: return String(MID_greaterq);
  case kOpGreaterEqual: return String(MID_greaterequalq);
  case kOpIn: return String(MID_in);
  case kOpIsa: return String(MID_isaQ);
  case kOpLess: return String(MID_lessq);
  case kOpLessEqual: return String(MID_lessequalq);
  case kOpLogicalAnd: return String(MID_logand);
  case kOpLogicalOr: return String(MID_logor);
  case kOpMinus: return String(MID_subtract);
  case kOpMod: return String(MID_mod);
  case kOpRem: return String(MID_rem);
  case kOpMultiply: return String(MID_multiply);
  case kOpPlus: return String(MID_add);
  case kOpShiftLeft: return String(MID_shiftleft);
  case kOpShiftRight: return String(MID_shiftright);
  case kOpUnequal: return String(MID_unequalq);

  case kOpInvalid:
  case kOpAssign: return String("=");
  case kOpAs: return String(MID_castto);
  case kOpBy: return String(MID_byid);
  case kOpMapTo: return String(MID_mapto);
  case kOpRange: return String(".."); hr_invalid("");
  }

  return String();
}


bool Typifier::checkBinaryFunctionCall(std::shared_ptr<BinaryNode> node,
                                       const String& funcName,
                                       std::shared_ptr<AstNode> leftArg,
                                       std::shared_ptr<AstNode> rightArg)
{
  auto funcNode = node->scope()->lookupFunction(funcName, K(showAmbiguousSymDef));

  if (funcNode) {
    auto args = makeNodeList({leftArg, rightArg});

    if (auto bestFuncNode = node->scope()->lookupBestFunctionOverload(
            funcName, typesForArgs(args), node->srcpos(), K(showAmbiguousSymDef))) {
      if (bestFuncNode.fNode->params().size() > 2) {
        HR_LOG(kError, node->srcpos(), E_WrongOperatorFuncSign)
            << "operator implementation with wrong parameter count";
        return false;
      }

      // auto newBase = makeSymbolNode(node->scope(), node->srcpos(), bestFuncNode.fName);
      // node->setBase(newBase);
      node->setRefFunction(bestFuncNode.fNode);
      // typf->reorderArguments(node, bestFuncNode.get());
      // typf->flattenArguments(node);

      Type type =
          typifyMatchAndCheckParameters(node->srcpos(), args, funcNode.get(), funcName);
      if (type.isDef()) {
        node->setType(type);
        return true;
      }
    }
    else {
      HR_LOG(kError, node->srcpos(), E_NoMatchingFunction)
          << "no matching implementation for operator: " << funcName;
    }
  }

  return false;
}


//------------------------------------------------------------------------------

TypifyPass::TypifyPass(int level, Compiler& compiler)
    : AstNodeCompilePass(level, K(showNodeType))
    , fCompiler(compiler)
{
}


std::shared_ptr<AstNode> TypifyPass::doApply(std::shared_ptr<AstNode> src)
{
  auto ty = Typifier{fCompiler};
  ty.typifyNode(src);
  return src;
}

}  // namespace herschel
