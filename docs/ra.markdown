Asm Code Generation Overview
=============================

Code generation and register allocation a performed as a single backwards pass over the IR. For each IR instruction we decide which registers will be used in the instruction and then generate the machine code using those registers. Most IR instructions map to one or two machine instructions. Notable exceptions are heap allocations which need to both allocate the space and copy the values into the heap.

Two Operand Instructions
-------------------------

Each two operand IR instruction has the form

    D = op L R

where D is the destination register L is the left operand and R is the
right operand.

When we go to allocate registers for the instruction we

1. R - Make sure the right operand has a register.

    If R already has a register allocated then simply use that register
    If R does not have a register allocated we need to choose one
      If there is a free register available then use it
      If there are no more registers available then evict a register
        Evicting a register will assign a spill slot to the IR that is spilled
 
        Generate a load for the spilled instruction. We generate a load now
        because we are allocating bottom up and we already generated
        instructions that expected to find the spilled value in a particular
        register. We will generate the store for the spilled value when we
        get to its defition point which is earlier in the instruction
        stream.

2. D - Make sure the destination has a register allocated

    The D may already have a register allocated if it was referenced a an L
    or R value in the instructions below.

    If it has a register allocated then reuse it, Otherwise find a
    register to use as for R

    If D was assigned a spill slot earlier then generate a store to spill
    the regsiter to its assigned slot. We do this so that the later load
    will find the register in the right location.

    Mark the register used by D as free. Since we going bottom up, the D
    register will be free at all the instructions above the current op,
    since D is defined in this instruction.

3. op - Generate the code for the operation using D = op D R

4. L - Generate code to make sure that L is in the same register as D. 

    If L does not have a register then 
        If L can be rematerailzed then remat into D
        Else allocate D's register to L
    Otherwise generate a register-to-register copy here.
    
    This is a required step for all of the x86 two operand machine
    instructions.

One Operand Instructions
------------------------

Each one operand instruction has the form of

    D = op R

Code is generated similarly for the two-operand case except that L is not needed.

Spills
-------------------------

Since we are generating code bottom up, we will find uses of values before their definitions. At the first use site an instruction will be assigned a register. As codegen continues it will use the register for references to that value. If there are not enough registers, a value may have to be spilled. When a value is spilled a restore is generated that loads the value from it spill slot into the currently assigned register so that the code already generated will have the correct value. If it is referenced again before its definition point, it will be assigned a (possibly different) register so that a single value could be assigned to different registers during the course of execution. When we reach the (single because of SSA) definition point and a value has a spill we will generate code to save it to the correct spill slots so that later loads will find the correct value in the spill slot.

The cost to spill is a combination of the reference number plus an extra bit
for those values that flow to phi nodes. Due to the organization of the IR,
constants will have a smaller cost than values defined outside the loop, which
will have a smaller cost than values defined inside the loop. The extra phi-
factor tries to ensure that values in phi nodes will get a register.

Constants and Rematerialization
-------------------------------

If a reference is to a constant value (which we can detect by checking ref < REF_BASE) then we try to generate code that uses the constant value without a register. Because of the limitiations of the x86 instruction set, only 32-bit constants can be used in most instructions. To use a full 64-bit constnatn, the value must be loaded to a register with a `mov` operation, like
    
    movq 0x1122334455667788, r

Currently, only KINT constants are small enought to fold directly into the instructions.

Constants are cheaper to spill because they don't have to actually be stored and loaded from the stack. Since they are constant, they can alwyas be recreated on demand. We call this rematerialization and it is used by the register allocator so that we consider it cheaper to spill constants.

Generating code for loops
=========================

Code generation for loops can be broken down into three major parts

    1. Register allocation for phi nodes at the bottom of the loop
    2. Codegen for the body of the loop
    3. Codegen to resolve phi conflicts at the top of the loop

Phi nodes are found at the bottom of the loop, unlike traditional compiler IRs which would place them at the top. In this discussion we talk about the LHS and RHS of a phi node. We think of a phi node like

    phi(L, R)

The L is the value coming from outside the loop, and the R is the value defined inside the loop. Unlike traditional phi nodes, the phi nodes in this IR do not define a new variable that is referenced in the loop. The value computed by the phi node redefines the L value, so that the phi node can operationally be read as
    
    L = phi(L,R)

Although the phi nodes are found at the bottom of the loop in the IR, its operations take effect at the top of the loop. After register allocation, we will have a situation like this

    L = L;
    LOOP:
        ;; L = phi(L,R)
        ...
        R = ...
        ...
        L = R
    GOTO LOOP

Where the phi node has been replaced by copies. How the copies actually get placed is a bit complicated and is discussed in the "Resolving PHI node allocations at the LOOP marker" section below.


Phi Nodes (bottom of loop)
--------------------------
When we begin code generation, the first thing we will do is allocate registers for the RHS of the phi nodes.

1. Assign regs to rhs of the phi node.
	* Mark the register assigned to the RHS as a hint for the LHS
	* Keep track of each machine register `r` that we have assigned to the RHS of a phi node (phiset)
	* Keep track of the LHS of each register we have assigned to a RHS of a phi node (phimap[r] -> LHS)


Loop Codgen (loop body)
------------------------
The codgen for the loop body continues as for the rest of the IR as described above. The LHS are not handled specially. We stored the hint for the LHS to take the same register as the RHS so it will automatically get that register if it is free when we first need to allocate a register for the LHS.

2. If we see the LHS of the phi node and cannot assign the same register (stored as a hint), then we have overlapping live ranges and the two sides of the phi node must be given different registers.
3. If we see the LHS of a phi node and the register is free then we can assign the same register and the copying will happen automatically.
4. Generate copies and stores as neede for phi nodes at the loop marker


Loop Marker (top of loop)
-------------------------
When we get to the loop marker we have reached the top of the loop. We need to take care of any phi nodes that have different registers assigned to the LHS and RHS. Resolving the differences is skecthed below and discussed in detail in a later section.

Check all assigned phi register (phiregs)
	* If reg(LHS) == reg(RHS) then pass
	* Else
		* If reg(RHS) is free then call  rename(LHS, RHS)
          This will change the LHS register assignement to use r(RHS) outside of the loop and insert a copy from r(RHS) to r(LHS) at the top of the loop.
		* Else (r(RHS) is not free)
            * If we are blocked by another phi register then we have a cycle of depencies between the phi nodes and we need to use an extra register to be able to correctly insert copies to break the cycle.
            * Otherwise (not blocked by another phi register) we will choose to spill the value that is taking our register. Spilling the non-phi value will insert a load for that value at the top of the loop and then call rename(r(LHS),r(RHS)). The rename will use the r(RHS) register for the phi outside the loop, insert a copy from r(RHS) to r(LHS) at the top of the loop, and a load from non-phi into r(RHS) at the top of the loop (after the copy).

Once phi register mismatches have been resolved we have generated all the code for the loop. Now that we know the location of the top of the loop we insert the jump at the bottom of the loop that returns to the top of the loop.

Resolving PHI node allocations at the LOOP marker
=================================================

Resolving phi node conflicts can be a bit confusing so here are some more details.


No conflict between LHS and RHS assignment
------------------------------------------
The simplest case is when we can assign both the LHS and RHS of the phi node to the same register.

	LOOP:
		x1  = x0 + 1
		phi(x0, x1)


We start by allocating the RHS of the phi node, giving `x1:r1`. Next, when we get to the next instruction, we can free the register used by x1 (since the live range of x1 starts here) and give it to x0. So we get

	{ 
	  x1 : r1
	  x0 : r1
	}

LHS and RHS were given different registers
------------------------------------------
Slightly more difficult is when we have given different registers to the LHS and RHS of a PHI node. This could happen because the live ranges of the LHS and RHS overlap, as shown below

	x1  = x0 + 1
	... = x0
	phi(x0, x1)

Or it could be because there is a gap in the live ranges and the register was taken by another variable, such as

	... = x0
	... = y
	x1  = ...
	phi(x0, x1)

In this case we could assign a register to `x1`, say `r1`. Then free it after the defintion of `x1` and assign the same register to `y`. When we get to the use of `x0` the register `r1` is not available so we must assign it a new register, say `r2`. We end with this assignment

	{ 	
		x1 : r1, 
		y1 : r1, 
		x0 : r2
	}

When we have assigned the LHS and RHS of PHI nodes different registers, then we have to  have to reconcile the difference between `x1` and `x0`. 

There are three cases to consider when we have a conflict betwwen the LHS and RHS of a phi node. Assume that the RHS was given register `r1` and the LHS was given register `r2`. Now, when we get to the top of the loop, one of the three cases should hold

  1. The register `r1` is free
  2. The register `r1` is occupied by a value that is *not* an operand to a phi node.
  3. The register `r1` is occupied by a value that *is* an operand to a phi node.


**Case 1 `r1` is free **

Example:

	x1  = x0 + 1
	... = x0
	phi(x0, x1)

	{
		x1 : r1
		x0 : r2
	}

In this case we can insert a copy from `r1` to `r2` at the top of the loop and assign `r1` to `x0` outside of the loop. Since `r1` is on free on entry to the loop we can safely take it for use by `x0` outside of the loop. The final code will look like

	r1  = ... (initialze x0)
	LOOP:
		r2  = r1
	;; body
		r1  = r2 + 1
		... = r2
	goto LOOP

** Case 2 `r1` is occupied by a non-phi value **

Example:

	... = y
	x1  = ...
	... = x0
	phi(x0, x1)
	{
		x1 : r1
		y  : r1
		x0 : r2
	}

In this case we steal the register `r1` from `y` for `x0` to use outside the loop by spilling `y`. At the loop header we will copy the value from `r1` to `r2`, and then load the value from `y` into `r1`. This load will be processed on every loop iteration. The value stored in memory for `y` will always be up-to-date, because `y` is not modified in the loop (if it was, then it would be an operand to a phi-node).

	r1  = ... (initialize x0)
	LOOP:
		r2 = r1
		r1 = LOAD y
	;; body
		... = r1  (y)
		r1  = ... (x1)
		... = r2  (x0)
	goto LOOP

** Case 3: `r1` is occupied by a value used in another phi node **

Example:

	... = z
	... = x0
	y1  = ...
	... = y0
	x1  = ...
	phi(x0, x1)
	phi(y0, y1)
	{
		x1 : r1
		y1 : r2
		y0 : r1
		x0 : r2
		z  : r3
	}

This kind of allocation leads to a configuration of phi nodes that looks like

	x = phi(r2, r1)
	y = phi(r1, r2)

In this case we have a cycle of dependencies between the registers we have allocated for our phi nodes and we cannot just insert a copy between the registers. Inserting copies would give either

	(x0) r2 = r1 (x1)
	(y0) r1 = r2 (y1) ; y0 gets x1 instead of y1

or

	(y0) r1 = r2 (y1)
	(x0) r2 = r1 (x1) ; x0 gets y1 instead of x1

In both cases we are propgating a wrong value through the copies.

We need an extra register to break the cycle. When allocating registers for phi nodes at the bottom of a loop we make sure to reserve a few registers that are not allocated to phi nodes so that we can always find a register to use to break the cycle.

To break the cycle we choose a register that is not part of a cycle and spill it. Assuming the allocation example above, we have `r1,r2` that are part of the cycle and `r3` that is not part of the cycle.


Starting out we have currently allocated `r2` to `x` at its definition point and `r1` to `y` at its definion point. We have the following register assignments, which are the same inside and outside of the loop.

	
	{x0 : r2, y0 : r1 }
	LOOP:
	;; body
	{ x1 : r1, 
	  y1 : r2, 
	  x0 : r2, 
	  y0 : r1, 
	  z  : r3 }

We detect that there is a cycle in the phi assignments, so we choose to break the cycle at `r1` by using `r3`. First, we steal `r3` for use outside of the loop by `y0` (since `y0` is assigned to `r1` outside of the loop). To do this we need to generate a copy from `r3` to `r1`. This gives us the following code
	
	{x0 : r2, y0 : r3 }
	LOOP:
	;; now r1 is marked as free (Case 1)
	r1 = r3
	r3 = LD z ;; spill z
	;; body
	{ x1 : r1, 
	  y1 : r2, 
	  x0 : r2, 
	  y0 : r1, 
	  z  : r3 }

Now we again check our phi node assignments. We see that the assignment for x1 and x0 still do not match, but this time `r1` is free so we can change the assignment for `x0` to use `r1` outside the loop and generate a copy as in **Case 1**. Doing so gives us this code

	{x0 : r1, y0 : r3 }
	LOOP:
			  ;; now r2 is marked as free
	r2 = r1   ;; (rename(r2,r1) for x0 outside the loop)
			  ;;
	r1 = r3   ;; (rename(r1,r3) for y0 outside the loop)
	r3 = LD z ;; spill z
	;; body
	{ x1 : r1, 
	  y1 : r2, 
	  x0 : r2, 
	  y0 : r1, 
	  z  : r3 }

As we examine the next phi register, we see that `y0` and `y1` still differ, but now `r2` is free so we again fall into **Case 1** which lets us reassign `y0` to `r2` outside the loop.

	{x0 : r1, y0 : r2 }
	LOOP:
	r3 = r2   ;; (rename(r3,r2) for y0 outside the loop)
			  ;;
	r2 = r1   ;; (rename(r2,r1) for x0 outside the loop)
			  ;;
	r1 = r3   ;; (rename(r1,r3) for y0 outside the loop)
	r3 = LD z ;; spill z
	;; body
	{ x1 : r1, 
	  y1 : r2, 
	  x0 : r2, 
	  y0 : r1, 
	  z  : r3 }

The end result is that `r3` is used as a temporary register to enable the swap of the values in `r1` and `r2`. The final generated code will look like this:

	r1 = ... (initialize x0)
	r2 = ... (initialize y0)
	LOOP:
		r3 = r2
		r2 = r1
		r1 = r3
		r3 = LD z
	;; body
		... = r3 (z)
		... = r2 (x0)
		r2  = ...(y1)
		... = r1 (y0)
		r1  = ...(x1)
	goto LOOP


Spills of phi nodes inside loops
=================================
If a value that is the LHS of a phi node is spilled inside of a loop, then we must insert a store of that value at the top of the loop so that the load inside the loop will find the updated value.

Case 1: r(LHS) is r1 at top of loop
	Will get a copy from r(RHS) to r1

Case 2: r(LHS) is NONE at top of loop
	r(RHS) is free
		pass
	r(RHS) is occupied
		if r(RHS) is occupied by invariant
			evict invarient
		otherwise r(RHS) will be freed by later phi reg 
			(it cannot be part of a phi cycle becuase r(LHS) is None)
Allocate a register for r(LHS) (either r or its existing).
Insert a save from r(LHS) to s(LHS)


Spills when r(LHS) is not NONE
-------------------------------

Example:
	
	... = x0
	... = z
	... = y
	... = x0
	x1  = ...
	phi(x0,x1)

In this example assume that we only have two register `r1` and `r2`. Now a backwards walk over the code will give the final allocation below. 

    {x1 : r1, 
	 x0 : r2,
	 y  : __, 
	 z  : r1}

In this allocation, first `x0` was spilled for `z`, then later `y` was spilled for `x0`. The `x0` value was assigned to two different registers in the body of the loop. The final code will look like this
    
	... = r2 
	r2  = LD y (spill y)
	... = r1
	r1  = LD x (spill x0)
	... = r2
	... = r1
	r1  = ...

Now when we get to the top of the loop we will still have `x0` allocated to register `r2`. The allocator will proceed as above to ensure that the all the correct copies are in place. This will allocate `x0` to `r1` outside of the loop and insert a copy from `r1` to `r2` for `x0`.

Finally, since `x0` was spilled in the loop, we need to insert a store at the top of the loop to ensure that an updated value will be read when the value for `x0` is loaded. The final code will look like this:

	r1 = ... (initialize x0)
	LOOP:
	;; stores for spilled phi values
		ST r1 => x0
	;; copies for phi nodes
		r2  = r1
		ST r1 => z
	;; body
		... = r2 x0
		r2  = LD y (spill y)
		... = r1 z
		r1  = LD x0(spill x0)
		... = r2 y
		... = r1 x0
		r1  = ...
	goto LOOP


Spills when r(LHS) is NONE
-------------------------------
The picture is slightly different when the LHS phi node does not have a register by the time we reach the top of the loop (in a bacwards walk).

Example:

	... = z
	... = y
	... = x0
	x1  = ...
	phi(x0,x1)

In this example assume that we only have two registers. A backwords walk over the code will initially assign `x0` to `r1`, but later spill it to make room for `z`. At the top of the loop we will have code and allocation that looks like

	... = r1
	r1  = LD x0 (spill x0)
	... = r2
	... = r1
	r1  = ...
    {x1 : r1, 
	 x0 : __,
	 y  : r2, 
	 z  : r1}

When we go to insert copies for the phi nodes we will find that the LHS and RHS registers do not match because LHS does not have a register. The allocator will behave as in case 1 or case 2 from above (in LHS and RHS were given different registers). Case 3 is impossible because LHS has no register so it cannot be part of a cycle of phi nodes. The end result will be that the RHS register will be freed, either because it is already free or by spilling the invariant value in the register. In this specific example, it will spill `z` to free up r1. 

Finally, since `x0`

	r1 = ... (initialize x0)
	LOOP:
	;; stores for spilled phi values
		ST r1 => x0
    ;; phi node fixing
		r1  = LD z  (spill z)
	;; body
		... = r1
		r1  = LD x0 (spill x0)
		... = r2
		... = r1
		r1  = ...
	goto LOOP


Handling Renamings in Snapshots
=================================

When processing a snapshot during register allocation we have two options for recording where the IR references are currently mapped to. We can directly write the stack slot or register for the ir instruciton into the snapshot entry, or we can use the value stored in the ir instruction itself. The advantage of writing into the snapshot directly is that it is simple to implement. The disadvantage is that the snapshot no longer contains valid IR references, which could break other code examining the snapshot. 

To keep the references valid in the snapshots, we will use the value in the IR as the mapping for the snapshot. If the value was spilled, then we will always take the value from the spill slot, otherwise we can use the register value. This works fine except in the case where a value has been assigned two differnet registers to handle a phi-node mismatch.

When we change register assignments to handle phi-node mismatches we need to be careful to make sure we restore the correct values for the snapshot corresponding to our exit number.

Consider the following example

	x0  = ...
	GUARD(1) {0: x0}
	LOOP:
		x1  = x0 + 1
		GUARD(2) {0: x0}
		... = x0
		phi(x0,x1)
	

Now when we do the bottom up allocation, we will first have an allocation like this:

	{x1 : r1, x0 : r2}

but after performing the renaming, we will have allocated `x0` to `r1`. The final code will look like this:


	r1  = ... (initialize x0)
	GUARD(1) {0: x0}
	LOOP:
		r2 = r1
	;; body
		r1  = r2 + 1
		GUARD(2) {0: x0}
		... = r2

Now assuming that `x0` does not get spilled, its final register assignment will be `r1`. If we fail Guard(1), then this is not a problem, but if we fail Guard(2), we will wrongly assume that `x0` is in `r1` (since `r1` is stored in the register field of the IR instruction for `x0`).

To make sure that we always reference the right location for a snapshot, we emit `IR_RENAME` instructions at the end of the IR. These instructions tell us that a reference has been assigned two different registers. When we got to restore a value from a snapshot we need to check if we are above or below the point where the rename occured, and if so we take the register from the renamed value.

The `IR_RENAME` instruction takes two paramters, the first is the IR Reference that is being renamed, and the second is the IR reference below which the renaming is valid. The register for the value above the change point will be stored in the origininal IR instruction, and the register below the change point will be stored in the `IR_RENAME` instruction.

When we go to restore a snapshot, we check to see if the ir reference is an input to a IR_RENAME instruction and if we are below the rename point. If so, we will use the renamed register value, otherwise we use the original register value.
