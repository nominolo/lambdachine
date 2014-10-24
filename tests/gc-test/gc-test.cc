#include "memorymanager.hh"

/*

MemoryManager

 - allocString(bytes)
 - allocCode(bytes)
 - allocStaticClosure(bytes)

 - alloc(bytes)
 [maybe: alloc(section, bytes),
    where section = small-heap, large-objects, non-pointer-large, non-moving,
                    

 - getBumpAllocBlock(): Block
 - kMaxBumpAllocSize
 - blocksPending (?)
 - performGC (requires blocksPending == 0)

 - GC parameters
    - setMinHeapSize
    - setInitialHeapSize
    - grow factor
    - ...

 - stats
    - numGCs
    - allocated
    - copied
    - ...

 - debugging utils
    - looksLikeInfoTable
    - looksLikeClosure/Object
    - sanityCheck*

FindRoots<Callback>:  -- callback (statically) determines what
                      -- should be done for each root

 - setTopOfStackMask (?)
 - scavengeStack<Callback>(Word *base, Word *top, BcIns *pc)
    - scavengeFrame

 - scavengeStaticClosure(Closure *)

 - scavengeLarge

 - size_t scavenge(Closure *)
     calls evacuateCallback on each pointer in the object
     returns object's size

 - TODO: How do we find all the stacks?

GCStrategy:
 
 - evacuate
 - scavengeBlock  -- copying-GC specifyc
 - markBlockScavenged
 - clearAllBlockScavengedMarks

   // TODO: Move block scavenged bits into region data?
   // 1M regions / 32K blocks = 32 blocks per region =
   // 32 bits

 - 

 */
