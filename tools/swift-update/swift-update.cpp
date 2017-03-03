//===--- swift-update.cpp - Swift Migrator application --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Syntax/LegacyASTTransformer.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;
using namespace swift::syntax;

namespace {
enum class ActionType {
};
} // end anonymous namespace

namespace options {
static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("path to the SDK to build against"));

static llvm::cl::opt<std::string>
Triple("target", llvm::cl::desc("target triple"));

static llvm::cl::opt<std::string>
ResourceDir("resource-dir",
  llvm::cl::desc("The directory that holds the compiler resource files"));

static llvm::cl::list<std::string>
ImportPaths("I",
            llvm::cl::desc("add a directory to the import search path"));

static llvm::cl::list<std::string>
FrameworkPaths("F",
                llvm::cl::desc("add a directory to the framework search path"));

} // end options namespace

int main(int argc, const char *argv[]) {
  // Print a stack trace if we signal out.
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);

  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);
  DiagnosticEngine &Diags = Instance.getDiags();
  CompilerInvocation Invocation;

  auto Args = llvm::makeArrayRef(argv, argc).slice(1);

  // By default, ignore deployment target mismatches for migration. They should
  // only impact linking, which we don't care about in the migrator.
  // You can override with -enable-target-os-checking
  Invocation.getLangOptions().EnableTargetOSChecking = false;

  if (Invocation.parseArgs(Args, Diags)) {
    return 1;
  }

  auto &FEOpts = Invocation.getFrontendOptions();
  if (!FEOpts.PrimaryInput.hasValue() && FEOpts.InputFilenames.size() == 1)
    FEOpts.PrimaryInput = SelectedInput(0);

  if (FEOpts.InputFilenames.empty()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_requires_an_input_file);
    return 1;
  }

  return 0;
}
