#pragma once

//
// This file is distributed under the MIT License. See LICENSE.md for details.
//

#include <map>
#include <optional>
#include <set>
#include <type_traits>

#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

#include "revng/Support/Assert.h"
#include "revng/Support/Debug.h"
#include "revng/TupleTree/TupleTree.h"

inline Logger<> ModelVerifyLogger("model-verify");

namespace model {
class Type;
class Identifier;

class VerifyHelper {
private:
  std::set<const model::Type *> VerifiedCache;
  std::map<const model::Type *, uint64_t> SizeCache;
  std::set<const model::Type *> InProgress;
  bool AssertOnFail = false;
  std::map<model::Identifier, std::string> GlobalSymbols;

  // TODO: This is a hack for now, but the methods, when the Model does not
  // verify, should return an llvm::Error with the error message found by this.
  std::string ReasonBuffer;

public:
  VerifyHelper() = default;
  VerifyHelper(bool AssertOnFail) : AssertOnFail(AssertOnFail) {}

  ~VerifyHelper() { revng_assert(InProgress.size() == 0); }

public:
  void setVerified(const model::Type *T) {
    revng_assert(not isVerified(T));
    VerifiedCache.insert(T);
  }

  bool isVerified(const model::Type *T) const {
    return VerifiedCache.contains(T);
  }

  const std::string &getReason() const { return ReasonBuffer; }

public:
  bool isVerificationInProgress(const model::Type *T) const {
    return InProgress.contains(T);
  }

  void verificationInProgress(const model::Type *T) {
    revng_assert(not isVerificationInProgress(T));
    revng_assert(not isVerified(T));
    InProgress.insert(T);
  }

  void verificationCompleted(const model::Type *T) {
    revng_assert(isVerificationInProgress(T));
    InProgress.erase(T);
  }

public:
  void setSize(const model::Type *T, uint64_t Size) {
    revng_assert(not size(T));
    SizeCache[T] = Size;
  }

  std::optional<uint64_t> size(const model::Type *T) {
    auto It = SizeCache.find(T);
    if (It != SizeCache.end())
      return It->second;
    else
      return std::nullopt;
  }

public:
  [[nodiscard]] bool isGlobalSymbol(const model::Identifier &Name) const;
  [[nodiscard]] bool registerGlobalSymbol(const model::Identifier &Name,
                                          const std::string &Path);

public:
  bool maybeFail(bool Result) { return maybeFail(Result, {}); }

  bool maybeFail(bool Result, const llvm::Twine &Reason) {
    if (AssertOnFail and not Result) {
      revng_abort(Reason.str().c_str());
    } else {
      if (not Result)
        InProgress.clear();
      return Result;
    }
  }

  template<typename T>
  bool maybeFail(bool Result, const llvm::Twine &Reason, T &Element) {
    if (not Result) {
      InProgress.clear();

      {
        llvm::raw_string_ostream StringStream(ReasonBuffer);
        StringStream << Reason << "\n";
        serialize(StringStream, const_cast<std::remove_const_t<T> &>(Element));
      }

      if (AssertOnFail) {
        revng_abort(ReasonBuffer.c_str());
      } else {
        revng_log(ModelVerifyLogger, ReasonBuffer);
      }
    }

    return Result;
  }

  bool fail() { return maybeFail(false); }
  bool fail(const llvm::Twine &Reason) { return maybeFail(false, Reason); }

  template<typename T>
  bool fail(const llvm::Twine &Reason, T &Element) {
    return maybeFail(false, Reason, Element);
  }
};

} // namespace model
