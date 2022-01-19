/// \file Main.cpp
/// \brief This tool is used for ABI conversion testing. It's purpose is to
/// make sure that the function still corresponds to the intended ABI no matter
/// what kind of work was done on top of it. It works with both
/// `RawFunctionType` and `CABIFunctionType`.
///
/// \note: This tool requires a runtime abi artifact. It can be obtained using
/// revng-qa. For more information see the corresponding repository.

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <fstream>
#include <streambuf>
#include <string>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"

#include "Verify.h"

namespace Options {

using namespace llvm::cl;

OptionCategory Category("ABI Options");

using namespace model::ABI;
using ABI = model::ABI::Values;
constexpr const char *TrgDesc = "The ABI of the input/output binaries.";
#define clABIDescription(name) clEnumVal(name, model::ABI::getDescription(name))
auto TaV = values(clABIDescription(SystemV_x86_64),
                  clABIDescription(SystemV_x86),
                  clABIDescription(SystemV_x86_regparm_3),
                  clABIDescription(SystemV_x86_regparm_2),
                  clABIDescription(SystemV_x86_regparm_1),
                  clABIDescription(Microsoft_x86_64),
                  clABIDescription(Microsoft_x86_64_vectorcall),
                  clABIDescription(Microsoft_x86_64_clrcall),
                  clABIDescription(Microsoft_x86_cdecl),
                  clABIDescription(Microsoft_x86_stdcall),
                  clABIDescription(Microsoft_x86_thiscall),
                  clABIDescription(Microsoft_x86_fastcall),
                  clABIDescription(Microsoft_x86_clrcall),
                  clABIDescription(Microsoft_x86_vectorcall),
                  clABIDescription(AArch64),
                  clABIDescription(ARM),
                  clABIDescription(SystemV_MIPS_o32),
                  clABIDescription(SystemZ_s390x));
opt<ABI> TargetABI("abi", Required, desc(TrgDesc), TaV, cat(Category));

constexpr const char *FnDesc = "<input file name>";
opt<std::string> Filename(Positional, Required, desc(FnDesc), cat(Category));

constexpr const char *AtDesc = "<runtime abi artifact name>";
opt<std::string> Artifact(Positional, Required, desc(AtDesc), cat(Category));

opt<std::string> Output("o",
                        desc("Optional output filename, if not specified, the "
                             "output is dumped to `stdout`"),
                        value_desc("path"),
                        cat(Category));

} // namespace Options

int main(int argc, const char *argv[]) {
  // Enable LLVM stack trace
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  llvm::cl::HideUnrelatedOptions(Options::Category);
  llvm::cl::ParseCommandLineOptions(argc, argv);

  llvm::raw_fd_ostream *OutputStreamPtr;
  if (Options::Output.empty()) {
    OutputStreamPtr = &llvm::outs();
  } else {
    std::error_code EC;
    static llvm::raw_fd_ostream OutputStream(Options::Output, EC);
    if (!EC || OutputStream.has_error()) {
      dbg << "Unable to open an output file: '" << Options::Output << "'.\n";
      return ExitCode::FailedOpeningTheOutputFile;
    }
    OutputStreamPtr = &OutputStream;
  }

  auto InputOrError = llvm::MemoryBuffer::getFileOrSTDIN(Options::Filename);
  if (!InputOrError) {
    dbg << "Unable to open an input file: '" << Options::Filename << "'.\n";
    return ExitCode::FailedOpeningTheInputFile;
  }
  llvm::StringRef InputText = (*InputOrError)->getBuffer();

  auto ArtifactOrError = llvm::MemoryBuffer::getFileOrSTDIN(Options::Artifact);
  if (!ArtifactOrError) {
    dbg << "Unable to open the artifact file: '" << Options::Artifact << "'.\n";
    return ExitCode::FailedOpeningTheArtifactFile;
  }
  llvm::StringRef Artifact = (*ArtifactOrError)->getBuffer();

  auto Deserialized = TupleTree<model::Binary>::deserialize(InputText);
  if (!Deserialized) {
    dbg << "Unable to deserialize the model: '" << Options::Filename << "'.\n";
    return ExitCode::FailedDeserializingTheModel;
  }
  if (!Deserialized->verify()) {
    dbg << "Model verification failed: '" << Options::Filename << "'.\n";
    return ExitCode::FailedVerifyingTheModel;
  }

  auto &Model = *Deserialized;
  return verifyABI(Model, Artifact, Options::TargetABI, *OutputStreamPtr);
}
