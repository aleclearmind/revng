//
// Copyright rev.ng Srls. See LICENSE.md for details.
//

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"

#include "revng/Support/Assert.h"

#include "GlobalDeclCreationAction.h"

#include "ASTBuildAnalysis.h"
#include "DecompilationHelpers.h"
#include "IRASTTypeTranslation.h"
#include "Mangling.h"

using namespace llvm;

namespace clang {
namespace tooling {

using GlobalsMap = GlobalDeclCreationAction::GlobalsMap;
using TypeDeclMap = std::map<const llvm::Type *, clang::TypeDecl *>;
using FieldDeclMap = std::map<clang::TypeDecl *,
                              llvm::SmallVector<clang::FieldDecl *, 8>>;

class GlobalDeclsCreator : public ASTConsumer {
public:
  explicit GlobalDeclsCreator(llvm::Function &F,
                              IR2AST::StmtBuilder &ASTBldr,
                              GlobalsMap &GMap,
                              TypeDeclMap &TDecls,
                              FieldDeclMap &FieldDecls) :
    TheF(F),
    ASTBuilder(ASTBldr),
    GlobalVarAST(GMap),
    TypeDecls(TDecls),
    FieldDecls(FieldDecls) {}

  virtual void HandleTranslationUnit(ASTContext &Context) override;

private:
  llvm::Function &TheF;
  IR2AST::StmtBuilder &ASTBuilder;
  GlobalsMap &GlobalVarAST;
  TypeDeclMap &TypeDecls;
  FieldDeclMap &FieldDecls;
};

void GlobalDeclsCreator::HandleTranslationUnit(ASTContext &Context) {
  uint64_t UnnamedNum = 0;
  TranslationUnitDecl *TUDecl = Context.getTranslationUnitDecl();
  for (GlobalVariable *G : getDirectlyUsedGlobals(TheF)) {
    using namespace IRASTTypeTranslation;
    QualType ASTTy = getOrCreateQualType(G,
                                         Context,
                                         *TUDecl,
                                         TypeDecls,
                                         FieldDecls);

    std::string VarName = G->getName();
    if (VarName.empty()) {
      raw_string_ostream Stream(VarName);
      Stream << "global_" << UnnamedNum++;
    }
    IdentifierInfo &Id = Context.Idents.get(makeCIdentifier(VarName));
    VarDecl *NewVar = VarDecl::Create(Context,
                                      TUDecl,
                                      {},
                                      {},
                                      &Id,
                                      ASTTy,
                                      nullptr,
                                      StorageClass::SC_Static);
    if (G->hasInitializer()) {
      revng_assert(not G->isExternallyInitialized());

      llvm::Constant *LLVMInit = G->getInitializer();
      const clang::Type *UnderlyingTy = ASTTy.getTypePtrOrNull();
      if (UnderlyingTy != nullptr and not isa<llvm::ConstantExpr>(LLVMInit)) {
        clang::Expr *Init = nullptr;
        if (UnderlyingTy->isCharType()) {
          uint64_t UniqueInteger = LLVMInit->getUniqueInteger().getZExtValue();
          revng_assert(UniqueInteger < 256);
          Init = new (Context)
            CharacterLiteral(static_cast<unsigned>(UniqueInteger),
                             CharacterLiteral::CharacterKind::Ascii,
                             Context.CharTy,
                             {});
        } else if (UnderlyingTy->isBooleanType()) {
          const llvm::ConstantInt *CInt = cast<llvm::ConstantInt>(LLVMInit);
          uint64_t InitValue = CInt->getValue().getZExtValue();
          APInt InitVal = LLVMInit->getUniqueInteger();
          QualType BoolTy = getOrCreateBoolQualType(Context,
                                                    TypeDecls,
                                                    G->getType());
          QualType IntT = Context.IntTy;
          APInt Const = APInt(Context.getIntWidth(IntT), InitValue, true);
          Expr *IntLiteral = IntegerLiteral::Create(Context, Const, IntT, {});
          Init = createCast(BoolTy, IntLiteral, Context);
        } else if (UnderlyingTy->isIntegerType()
                   and not UnderlyingTy->isPointerType()
                   and not UnderlyingTy->isAnyCharacterType()) {

          Init = ASTBuilder.getLiteralFromConstant(LLVMInit);
        }

        if (Init)
          NewVar->setInit(Init);
      }
    }
    GlobalVarAST[G] = NewVar;
  }
}

std::unique_ptr<ASTConsumer> GlobalDeclCreationAction::newASTConsumer() {
  return std::make_unique<GlobalDeclsCreator>(TheF,
                                              ASTBuilder,
                                              GlobalVarAST,
                                              TypeDecls,
                                              FieldDecls);
}

std::unique_ptr<ASTConsumer>
GlobalDeclCreationAction::CreateASTConsumer(CompilerInstance &,
                                            llvm::StringRef) {
  return newASTConsumer();
}

} // end namespace tooling
} // end namespace clang
