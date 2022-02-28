//
// Copyright (c) rev.ng Srls. See LICENSE.md for details.
//

#include <memory>
#include <type_traits>

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Debug.h"

#include "revng/ADT/FilteredGraphTraits.h"
#include "revng/Support/Assert.h"
#include "revng/Support/Debug.h"

#include "revng-c/DataLayoutAnalysis/DLATypeSystem.h"

#include "../DLAHelpers.h"
#include "DLAStep.h"

using namespace llvm;

static Logger<> Log("dla-compute-upper-member-access");

namespace dla {

using LTSN = LayoutTypeSystemNode;
using GraphNodeT = LTSN *;
using NonPointerFilterT = EdgeFilteredGraph<GraphNodeT, isNotPointerEdge>;
using ConstNonPointerFilterT = EdgeFilteredGraph<const LTSN *,
                                                 isNotPointerEdge>;

bool ComputeUpperMemberAccesses::runOnTypeSystem(LayoutTypeSystem &TS) {
  if (VerifyLog.isEnabled())
    revng_assert(TS.verifyDAG() and TS.verifyInheritanceTree());
  bool Changed = false;

  using LTSN = LayoutTypeSystemNode;

  if (Log.isEnabled())
    TS.dumpDotOnFile("before-compute-upper-member-access.dot", true);

  std::set<const LTSN *> Visited;
  for (LTSN *Root : llvm::nodes(&TS)) {
    revng_log(Log, "Root ID: " << Root->ID);
    revng_assert(Root != nullptr);
    // Leaves need to have ValidLayouts, otherwise they should have been trimmed
    // by PruneLayoutNodesWithoutLayout
    revng_assert(not isLeaf(Root) or Root->Size);
    if (not isRoot(Root))
      continue;

    LoggerIndent Indent{ Log };
    revng_log(Log, "Is Root");
    revng_log(Log, "post_order_ext from Root");
    LoggerIndent MoreIndent{ Log };

    revng_assert(isInheritanceRoot(Root));

    for (LTSN *N : post_order_ext(NonPointerFilterT(Root), Visited)) {
      revng_log(Log, "N->ID: " << N->ID);
      revng_assert(not isLeaf(N) or N->Size);
      uint64_t FinalSize = N->Size;

      // Look at all the instance-of edges and inheritance edges all together.
      bool HasBaseClass = false;
      revng_log(Log, "N's children");
      LoggerIndent MoreMoreIndent{ Log };
      for (auto &[Child, EdgeTag] : children_edges<ConstNonPointerFilterT>(N)) {
        revng_log(Log, "Child->ID: " << Child->ID);
        LoggerIndent MoreMoreMoreIndent{ Log };
        revng_log(Log,
                  "EdgeTag->Kind: "
                    << dla::TypeLinkTag::toString(EdgeTag->getKind()));
        auto ChildSize = Child->Size;
        revng_assert(ChildSize > 0LL);

        switch (EdgeTag->getKind()) {

        case TypeLinkTag::LK_Inheritance: {
          // Treated as instance at offset 0, but can only have one.
          // Should only have one parent in inheritance hierarchy.
          revng_assert(not HasBaseClass);
          HasBaseClass = true;
          FinalSize = std::max(FinalSize, ChildSize);
        } break;

        case TypeLinkTag::LK_Instance: {
          const OffsetExpression &OE = EdgeTag->getOffsetExpr();
          revng_assert(OE.Strides.size() == OE.TripCounts.size());

          // Ignore stuff at negative offsets.
          revng_assert(OE.Offset >= 0LL);

          // If we have an array, we have to compute its size, taking into
          // account the strides and the trip counts.
          for (const auto &[TripCount, Stride] :
               llvm::reverse(llvm::zip(OE.TripCounts, OE.Strides))) {

            revng_assert(Stride > 0LL);
            auto StrideSize = static_cast<uint64_t>(Stride);

            // If we have a TripCount, we expect it to be strictly positive.
            revng_assert(not TripCount.has_value() or TripCount.value() > 0LL);

            // Arrays with unknown numbers of elements are considered as if
            // they had a single element
            auto NumElems = TripCount.has_value() ? TripCount.value() : 1;
            revng_assert(NumElems);

            // Here we are computing the larger size that is known to be
            // accessed. So if we have an array, we consider it to be one
            // element shorter than expected, and we add ChildSize only once
            // at the end.
            // This is equivalent to:
            // ChildSize = (NumElems * StrideSize) - (StrideSize - ChildSize);
            ChildSize = ((NumElems - 1) * StrideSize) + ChildSize;
          }

          revng_assert(ChildSize);

          int64_t ChildOffset = std::max<int64_t>(OE.Offset, 0LL);
          uint64_t ChildUpperOffset = ChildOffset + ChildSize;
          FinalSize = std::max(FinalSize, ChildUpperOffset);
        } break;

        default:
          revng_unreachable("unexpected edge");
        }
      }
      if (FinalSize != N->Size)
        Changed = true;

      N->Size = FinalSize;
      revng_assert(FinalSize);
    }
  }

  if (Log.isEnabled())
    TS.dumpDotOnFile("after-compute-upper-member-access.dot");

  return Changed;
}

} // end namespace dla
