1. The hint gets lost when a register is allocated, so phi's that get allocated to separate register will never join together.

2. Need to try and not spill a phi-node by setting the cost higher for spilling phi nodes.

Generating code for loops
=============================

Phi Nodes
-----------
1. Backwards walk, assign regs to rhs of the phi node.
	* Mark the register assigned to the RHS as a hint for the LHS
	* Keep track of each register that we have assigned to the RHS of a phi node (phiset)
	* Keep track of the LHS of each register we have assigned to a RHS of a phi node (phimap[r] -> LHS)
2. If we see the LHS of the phi node and cannot assign the same register (stored as a hint), then we have overlapping live ranges and the two sides of the phi node must be given different registers.
3. If we see the LHS of a phi node and the register is free then we can assign the same register and the copying will happen automatically.
4. Generate copies and stores as neede for phi nodes at the loop marker

Loop Marker
-----------
Check all assigned phi register
	* If reg(LHS) == reg(RHS) then ok
	* Else
		* If reg(RHS) is free then rename(LHS to RHS) (r(RHS) => r(LHS))
		* Else
			* If we are blocked by another phi register GIVE UP (for now)
			* Else, spill


Register allocation for PHI Nodes
==================================

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





