//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#ifdef _WIN32
// Windows lacks dlfcn.h — translation is disabled on Windows anyway
// (libtcg is a Linux-only QEMU artifact), but to keep this TU
// compilable we provide thin shims over LoadLibrary/GetProcAddress.
// If translation is ever invoked on Windows, the dlopen of a `.so`
// will fail at runtime, which is the correct behaviour.
#include <windows.h>
#define RTLD_LAZY 0
static inline void *dlopen(const char *Path, int) {
  return reinterpret_cast<void *>(LoadLibraryA(Path));
}
static inline void *dlsym(void *Handle, const char *Name) {
  return reinterpret_cast<void *>(GetProcAddress(static_cast<HMODULE>(Handle),
                                                 Name));
}
static inline int dlclose(void *Handle) {
  return FreeLibrary(static_cast<HMODULE>(Handle)) ? 0 : -1;
}
#else
extern "C" {
#include "dlfcn.h"
}
#endif

#include "revng/Lift/LibTcg.h"
#include "revng/Support/ResourceFinder.h"

static std::string
findLibTcgPath(const model::Architecture::Values Architecture) {
  llvm::StringRef ArchName = model::Architecture::getQEMUName(Architecture);
  const std::string LibTcgName = "/lib/libtcg-" + ArchName.str() + ".so";
  auto OptionalLibTcg = revng::ResourceFinder.findFile(LibTcgName);
  revng_assert(OptionalLibTcg.has_value(), "Cannot find libtinycode");
  return OptionalLibTcg.value();
}

LibTcg LibTcg::get(model::Architecture::Values Architecture) {
  LibTcg Result;

  // Look for the library in the system's paths
  std::string LibraryPath = findLibTcgPath(Architecture);
  Result.LibraryHandle = dlopen(LibraryPath.c_str(), RTLD_LAZY);
  revng_assert(Result.LibraryHandle != nullptr);

  // Obtain the address of the libtcg_load entry point
  using LibTcgLoadFunc = LIBTCG_FUNC_TYPE(libtcg_load);
  void *LibTcgLoadSym = dlsym(Result.LibraryHandle, "libtcg_load");
  auto LibTcgLoad = reinterpret_cast<LibTcgLoadFunc *>(LibTcgLoadSym);
  revng_assert(LibTcgLoad != nullptr);

  // Load the libtcg interface containing relevant function pointers
  Result.Interface = LibTcgLoad();

  Result.Context = Result.Interface.context_create();
  revng_assert(Result.Context != nullptr, "Failed to create libtcg context");

  Result.ArchInfo = Result.Interface.get_arch_info();

  std::set<llvm::StringRef> Names;
  for (int I = 0; I < Result.ArchInfo.num_globals; ++I) {
    if (Result.ArchInfo.globals[I].name == nullptr)
      continue;
    auto Offset = Result.ArchInfo.globals[I].offset
                  + Result.ArchInfo.env_offset;
    llvm::StringRef Name(Result.ArchInfo.globals[I].name);

    revng_assert(not Names.contains(Name));
    revng_assert(not Result.GlobalNames.contains(Offset));
    Result.GlobalNames[Offset] = Name;
  }

  return Result;
}

LibTcg::~LibTcg() {
  Interface.context_destroy(Context);
  dlclose(LibraryHandle);
}
