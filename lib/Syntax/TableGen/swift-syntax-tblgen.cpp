#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Main.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;

enum class ActionType {
  None,
  GenImplementation,
  GenInterface,
};

enum class Category {
  Unknown,
  Decl,
  Stmt,
  Expr,
  Type,
  Pattern,
  SyntaxFactory,
  SyntaxRewriter
};

enum class TargetLanguage {
  CPlusPlus,
};

namespace options {
static cl::opt<ActionType> Action(
  cl::desc("Action to perform: "),
  cl::init(ActionType::None),
  cl::values(
    clEnumValN(ActionType::GenImplementation, "implementation",
      "Generate the implementation for the given syntax category"),
    clEnumValN(ActionType::GenInterface, "interface",
      "Generate the interface for the given syntax category")
    )
);
static cl::opt<TargetLanguage> Language(
  "language",
  cl::desc("Target language to emit: "),
  cl::init(TargetLanguage::CPlusPlus),
  cl::values(
    clEnumValN(TargetLanguage::CPlusPlus, "c++",
      "Generate the interface or implementation in C++")
  )
);
static cl::opt<std::string> Category(
  "category",
  cl::desc("Category to emit"));

} // end options namespace

#pragma mark - Helpers

/// Returns true if the record type is a subclass of Syntax.
/// Used to filter out values in a record that tablegen automatically inserts, like NAME,
/// or auxiliary fields that we have added, like IsRequired.
static const RecordRecTy *getIfSyntaxType(const RecTy *Ty, const RecordKeeper &Records) {
  if (auto RecordTy = dyn_cast<RecordRecTy>(Ty)) {
    if (RecordTy->getRecord()->isSubClassOf(Records.getClass("Syntax"))) {
      return RecordTy;
    }
  }
  return nullptr;
}

static Category getCategory() {
  return llvm::StringSwitch<Category>(options::Category)
  .Case("Decl", Category::Decl)
  .Case("Expr", Category::Expr)
  .Case("Stmt", Category::Stmt)
  .Case("Type", Category::Type)
  .Case("Pattern", Category::Pattern)
  .Case("SyntaxFactory", Category::SyntaxFactory)
  .Case("SyntaxRewriter", Category::SyntaxRewriter)
  .Default(Category::Unknown);
}

#pragma mark - Syntax

static bool printSyntaxInterface(const Record *Rec,
                                 raw_ostream &OS, const RecordKeeper &Records) {
  auto ClassName = std::string(Rec->getName()) + "Syntax";
  auto DataClassName = ClassName + "Data";

  OS << "class " << ClassName << " final : public Syntax {\n"
  "  friend struct SyntaxFactory;\n"
  "  friend class " << DataClassName << ";\n"
  "  friend class SyntaxData;\n"
  "\n"
  "  using DataType = " << DataClassName << ";\n"
  "\n"
  "  enum class Cursor : CursorIndex {\n";
  for (const auto Child : Rec->getValues()) {
    if (getIfSyntaxType(Child.getType(), Records)) {
      OS << "    " << Child.getName() << ",\n";
    }
  }
  OS << "  };\n";

  OS <<
  "  " << ClassName << "(RC<SyntaxData> Root, const " << DataClassName << "*Data);\n"
  "public:\n";
  for (const auto Child : Rec->getValues()) {

    auto ChildType = getIfSyntaxType(Child.getType(), Records);
    if (ChildType == nullptr) {
      continue;
    }
    auto ChildTypeName = ChildType->getAsString() + "Syntax";
    if (ChildType->getRecord() == Records.getClass("Token")) {
      ChildTypeName = "RC<" + ChildTypeName + ">";
    }

    OS << "  " << ChildTypeName << " get" << Child.getName() << "() const;\n";
    OS << "  " << ClassName << " with" << Child.getName() << "(" << ChildTypeName << " New" << Child.getName() << ") const;\n\n";
  }
  OS <<
  "  static bool classof(const Syntax *S) {\n"
  "    return S->getKind() == SyntaxKind::" << ClassName << ";\n"
  "  }\n"
  "};\n\n";

  return false;
}

static bool printSyntaxDataInterface(const Record *Rec,
                                     raw_ostream &OS,
                                     const RecordKeeper &Records) {
  auto Kind = Rec->getName();
  auto ClassName = std::string(Kind) + "Syntax";
  auto DataClassName = ClassName + "Data";

  OS << "class " << DataClassName << " final : public SyntaxData {\n"
  "  friend class SyntaxData;\n"
  "  friend struct SyntaxFactory;\n"
  "\n" <<
  "  " << DataClassName << "(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr, CursorIndex IndexInParent = 0);\n"
  "\n"
  "  static RC<" << DataClassName << "> make(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr, CursorIndex IndexInParent = 0);\n"
  "  static RC<" << DataClassName << "> makeBlank();\n"
  "\n"
  "public:\n"
  "  static bool classof(const SyntaxData *SD) {\n"
  "    return S->getKind() == SyntaxKind::" << Kind << ";\n"
  "  }\n"
  "};\n\n";
  return false;
}

static bool printSyntaxInterfaces(raw_ostream &OS, const RecordKeeper &Records) {
  auto Nodes = Records.getAllDerivedDefinitions(options::Category);
  auto Any = std::string { "Any" } + options::Category;
  for (const auto *Node : Nodes) {
    if (Node->getName() == Any) {
      continue;
    }
    printSyntaxInterface(Node, OS, Records);
    printSyntaxDataInterface(Node, OS, Records);
  }

  return false;
}

static bool printSyntaxImplementation(const Record *Rec,
                                      raw_ostream &OS,
                                      const RecordKeeper &Records) {
  // TODO
  return false;
}

static bool printSyntaxDataImplementation(const Record *Rec,
                                          raw_ostream &OS,
                                          const RecordKeeper &Records) {
  // TODO
  return false;
}

static bool printSyntaxImplementations(raw_ostream &OS,
                                       const RecordKeeper &Records) {
  auto Nodes = Records.getAllDerivedDefinitions(options::Category);
  auto Any = std::string { "Any" } + options::Category;
  for (const auto *Node : Nodes) {
    if (Node->getName() == Any) {
      continue;
    }
    printSyntaxImplementation(Node, OS, Records);
    printSyntaxDataImplementation(Node, OS, Records);
  }

  return false;
}

#pragma mark - SyntaxFactory

static bool printSyntaxFactoryInterface(raw_ostream &OS,
                                        const RecordKeeper &Record) {
  // TODO
  return false;
}


static bool printSyntaxFactoryImplementation(raw_ostream &OS,
                                             const RecordKeeper &Records) {
  // TODO
  return false;
}

#pragma mark - SyntaxRewriter

static bool printSyntaxRewriterInterface(raw_ostream &OS,
                                         const RecordKeeper &Records) {
  // TODO
  return false;
}

static bool printSyntaxRewriterImplementation(raw_ostream &OS,
                                              const RecordKeeper &Records) {
  // TODO
  return false;
}

static bool genInterface(raw_ostream &OS, RecordKeeper &Records) {
  switch (getCategory()) {
    case Category::Decl:
    case Category::Expr:
    case Category::Stmt:
    case Category::Type:
    case Category::Pattern:
      return printSyntaxInterfaces(OS, Records);
    case Category::SyntaxFactory:
      return printSyntaxFactoryInterface(OS, Records);
    case Category::SyntaxRewriter:
      return printSyntaxRewriterInterface(OS, Records);
    case Category::Unknown:
      llvm_unreachable("Unknown category given");
  }
}

static bool genImplementation(raw_ostream &OS, RecordKeeper &Records) {
  switch (getCategory()) {
    case Category::Decl:
    case Category::Expr:
    case Category::Stmt:
    case Category::Type:
    case Category::Pattern:
      return printSyntaxImplementations(OS, Records);
    case Category::SyntaxFactory:
      return printSyntaxFactoryImplementation(OS, Records);
    case Category::SyntaxRewriter:
      return printSyntaxRewriterImplementation(OS, Records);
    case Category::Unknown:
      llvm_unreachable("Unknown category given");
  }
}

static bool SyntaxTableGenMain(raw_ostream &OS, RecordKeeper &Records) {
  switch (options::Action) {
    case ActionType::None:
      llvm::errs() << "action required\n";
      llvm::cl::PrintHelpMessage();
      return true;
    case ActionType::GenInterface:
      return genInterface(OS, Records);
    case ActionType::GenImplementation:
      return genImplementation(OS, Records);
  }
}

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  cl::ParseCommandLineOptions(argc, argv);
  if (getCategory() == Category::Unknown) {
    errs() << options::Category << " is an unknown category!\n";
    cl::PrintHelpMessage();
    return 1;
  }
  return TableGenMain(argv[0], &SyntaxTableGenMain);
}
