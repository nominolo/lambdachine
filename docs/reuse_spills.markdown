---- TRACE 0175 IR -----------
K071 -            inf KWORD    0x1000720d0 Bench.Nofib.Spectral.Circsim.$wgomax`info
K069 -            cls KWORD    0x10000b140 Bench.Nofib.Spectral.Circsim.$wgomax`info
K067 -            pc  KWORD    0x1000853a4
K065 -            pc  KWORD    0x100024500
K063 -            cls KWORD    0x1000081b8 []`con_info
K061 -            inf KWORD    0x100001470 []`con_info
K059 -            inf KWORD    0x100003dd0 GHC.List.zipWith`info
K057 -            cls KWORD    0x1000088b0 GHC.List.zipWith`info
K055 -            cls KWORD    0x10000ac90 Bench.Nofib.Spectral.Circsim.simulate_restore_outport`info
K053 -            inf KWORD    0x100071f20 Bench.Nofib.Spectral.Circsim.simulate2`info
K052 -            ptr KBASEO   #17
K050 -            pc  KWORD    0x10005c78c
K049 -            ptr KBASEO   #5
K047 -            pc  KWORD    0x100086c64
K045 -            pc  KWORD    0x10005ddbc
K043 -            inf KWORD    0x100072f30 Bench.Nofib.Spectral.Circsim.go`info
K041 -            cls KWORD    0x10000b548 Bench.Nofib.Spectral.Circsim.go`info
K039 -            inf KWORD    0x10004f1e0 Bench.Nofib.Spectral.Circsim.cl_simulate_$sdo_send_sat_go8Dg`info
K037 -            inf KWORD    0x100000bc0 Ap1_p`info
K035 -            inf KWORD    0x1000726f0 Bench.Nofib.Spectral.Circsim.None`con_info
K033 -            pc  KWORD    0x10005dec0
K031 -            inf KWORD    0x100071fe0 Bench.Nofib.Spectral.Circsim.$w$sstore_inputs`info
K029 -            cls KWORD    0x10000b118 Bench.Nofib.Spectral.Circsim.$w$sstore_inputs`info
K027 -            inf KWORD    0x100001430 GHC.Types.I#`con_info
K025 -            inf KWORD    0x10004e640 Bench.Nofib.Spectral.Circsim.cl_simulate6_satCP`info
K023 -            inf KWORD    0x1000723d0 Bench.Nofib.Spectral.Circsim.PS`con_info
K021 -            inf KWORD    0x1000720a0 Bench.Nofib.Spectral.Circsim.simulate6`info
K020 -            ptr KBASEO   #10
K019 -            ptr KBASEO   #0
K017 -            pc  KWORD    0x10005de80
K015 -            inf KWORD    0x100000c80 Ap2_pp`info
K014 -            ptr KBASEO   #-5
K012 -            pc  KWORD    0x10005dd4c
K011 -            ptr KBASEO   #-11
K009 -            pc  KWORD    0x10002001c
K008 -            ptr KBASEO   #-16
K006 -            cls KWORD    0x100008010 stg_UPD
K004 -            pc  KWORD    0x100023d9c
K003 -            ptr KBASEO   #-22
K001 -            inf KWORD    0x1000005b0 stg_IND
0000 -            ptr BASE     #0   #0  
0001 -    [ 3]    unk SLOAD    #65520 #1  
0002 r11          unk SLOAD    #65524 #1  
0003 r13          unk SLOAD    #65525 #1  
0004 rdx  [ 1]    unk SLOAD    #65535 #1  
0005 r9           unk SLOAD    #0   #1  
  SNAP #0 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:|0005] pc=0x10005de78 (0) base=0
0006 -                EQINFO   0005 K015
0007 -            ptr FREF     0005 #1  
0008 rbx          unk FLOAD    0007
0009 -            ptr FREF     0005 #2  
0010 rsi          unk FLOAD    0009
0011 -            ptr FREF     0005 #3  
0012 r8           unk FLOAD    0011
  SNAP #1 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:0005 ---- ---- ---- 4:---- ---- ---- K019 8:K017 K006 0005 ---- 12:K020 K009 0005 |0010 16:0012 0008] pc=0x10002036c (5) base=15
0013 -                EQINFO   0008 K021
  SNAP #2 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:0005 ---- ---- ---- 4:---- ---- ---- K019 8:K017 K006 0005 ---- 12:K020 K009 0008 |0010 16:0012] pc=0x100085328 (7) base=15
0014 -                EQINFO   0012 K001
0015 -            ptr FREF     0012 #1  
0016 r14          cls FLOAD    0015
  SNAP #3 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:0005 ---- ---- ---- 4:---- ---- ---- K019 8:K017 K006 0005 ---- 12:K020 K009 0008 |0010 16:0016] pc=0x100085328 (7) base=15
0017 -                EQINFO   0016 K023
0018 -            ptr FREF     0016 #5  
0019 r15          unk FLOAD    0018
  SNAP #4 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:0005 ---- ---- ---- 4:---- ---- ---- K019 8:K017 K006 0005 ---- 12:K020 K009 0008 |0010 16:0016 K025 0019] pc=0x10008533c (11) base=15
0020 -                HEAPCHK  #27 
0021 r10          cls NEW      K025 #0  [0010 0019] =>K063
0022 -            ptr FREF     0016 #1  
0023 r8           unk FLOAD    0022
0024 -            ptr FREF     0016 #2  
0025 rbx          unk FLOAD    0024
0026 -            ptr FREF     0016 #3  
0027 rax          unk FLOAD    0026
0028 -            ptr FREF     0016 #4  
0029 r14          unk FLOAD    0028
0030 rdi  [ 2]    cls NEW      K023 #1  [0023 0025 0027 0029 0021]
0031 -                UPDATE   0005 0030
0032 -            ptr FREF     0030 #1  
  SNAP #5 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:|0030 ---- 0023] pc=0x10005de88 (24) base=0
0033 -                EQINFO   0023 K027
0034 -            ptr FREF     0004 #1  
0035 rdx  [ 2]    unk FLOAD    0034
0036 -            ptr FREF     0023 #1  
0037 r8   [ 3]    unk FLOAD    0036
0038 -            ptr FREF     0030 #2  
0039 -            ptr FREF     0030 #3  
0040 -            ptr FREF     0030 #4  
0041 -            ptr FREF     0030 #5  
0042 r8           cls NEW      K027 #2  [0037]
  SNAP #6 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 0003 -4:---- K014 K009 0004 0:K029 0035 0037 0025 4:0027 0029 0021 K019 8:K033 K029 |0035 0025 12:0025 0027 0029 0021 16:0037 0042] pc=0x100085204 (39) base=10
0043 -                EQINFO   0025 K035
0044 r14          cls NEW      K023 #3  [0042 0025 0027 0029 0021]
  SNAP #7 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 0003 ---- ---- -8:K011 K012 K006 |0003 -4:0044] pc=0x100020020 (57) base=-5
0045 -                EQINFO   0003 K037
0046 -                UPDATE   0003 0044
0047 -            ptr FREF     0044 #3  
  SNAP #8 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 |0044 0027] pc=0x10005dd54 (61) base=-11
0048 -                EQINFO   0027 K027
0049 -            ptr FREF     0002 #1  
0050 r13          unk FLOAD    0049
  SNAP #9 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 |0044 0027 0050] pc=0x10005dd64 (64) base=-11
0051 -                EQINFO   0050 K027
0052 -            ptr FREF     0027 #1  
0053 rax  [ 2]    unk FLOAD    0052
0054 -            ptr FREF     0050 #1  
0055 r13  [ 3]    unk FLOAD    0054
  SNAP #10 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 |0044 0053 0055] pc=0x10005dd78 (68) base=-11
0056 -                NE       0053 0055
0057 -            ptr FREF     0044 #1  
0058 r13          cls NEW      K039 #4  [0042]
0059 -            ptr FREF     0044 #5  
0060 -            ptr FREF     0021 #1  
  SNAP #11 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 K041 0058 0021 -8:K011 K045 K041 0021 -4:---- ---- ---- ---- 0:---- ---- K014 K047 4:K006 0021 ---- K049 8:K009 0021 |---- 0010] pc=0x10005c784 (79) base=10
0061 -                EQINFO   0010 K037
0062 -            ptr FREF     0010 #1  
0063 rdx          unk FLOAD    0062
0064 -            ptr FREF     0010 #2  
0065 r14          unk FLOAD    0064
  SNAP #12 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 K041 0058 0021 -8:K011 K045 K041 0021 -4:---- ---- ---- ---- 0:---- ---- K014 K047 4:K006 0021 ---- K049 8:K009 0021 ---- 0010 12:---- ---- K020 K050 16:K006 0010 ---- K052 20:K009 0010 |0065 0063] pc=0x10002031c (83) base=22
0066 -                EQINFO   0063 K053
  SNAP #13 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 K041 0058 0021 -8:K011 K045 K041 0021 -4:---- ---- ---- ---- 0:---- ---- K014 K047 4:K006 0021 ---- K049 8:K009 0021 ---- 0010 12:---- ---- K020 K050 16:K006 0010 ---- K052 20:K009 0063 |0065] pc=0x1000850ac (85) base=22
0067 -                EQINFO   0065 K023
0068 -            ptr FREF     0065 #2  
0069 rax          unk FLOAD    0068
  SNAP #14 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 K041 0058 0021 -8:K011 K045 K041 0021 -4:---- ---- ---- ---- 0:---- ---- K014 K047 4:K006 0021 ---- K049 8:K009 0021 ---- 0010 12:---- ---- K020 K050 16:K006 0010 ---- K052 20:K009 0063 |0065 ---- 24:0069] pc=0x1000850bc (88) base=22
0070 -                EQINFO   0069 K035
0071 -            ptr FREF     0065 #1  
0072 r8           unk FLOAD    0071
0073 -            ptr FREF     0065 #3  
0074 rdx          unk FLOAD    0073
0075 -            ptr FREF     0065 #4  
0076 rbx          unk FLOAD    0075
0077 -            ptr FREF     0065 #5  
0078 r14          unk FLOAD    0077
0079 rbx          cls NEW      K023 #5  [0072 0069 0074 0076 0078]
0080 -                UPDATE   0010 0079
0081 -            ptr FREF     0079 #5  
0082 -            ptr FREF     0021 #2  
  SNAP #15 [-19:K003 K004 K006 -16:0001 ---- K008 K009 -12:0002 K041 0058 0021 -8:K011 K045 K041 0021 -4:---- ---- ---- ---- 0:---- ---- K014 K047 4:K006 0021 ---- K049 8:K009 K057 |K055 0078 12:0019] pc=0x100024b54 (109) base=10
0083 -                EQINFO   0078 K061
0084 -                UPDATE   0021 K063
  SNAP #16 [-19:K003 K004 K006 -16:|0001 K063] pc=0x100020020 (130) base=-16
0085 -                EQINFO   0001 K037
0086 -                UPDATE   0001 K063
0087 rdx          unk SLOAD    #65512 #0  
  SNAP #17 [-22:|K063] pc=0x100023dbc (134) base=-22
0088 -                EQ       0087 K065
0089 rdx          unk SLOAD    #65515 #0  
0090 rcx          cls NEW      K027 #6  [0089]
0091 rbx          unk SLOAD    #65506 #0  
  SNAP #18 [-28:|0090 K027] pc=0x100024510 (138) base=-28
0092 -                EQ       0091 K009
0093 rbx          unk SLOAD    #65503 #0  
  SNAP #19 [-32:0090] pc=0x100020020 (140) base=-33
0094 -                EQINFO   0093 K037
0095 -                UPDATE   0093 0090
0096 rbx          unk SLOAD    #65501 #0  
  SNAP #20 [-32:0090] pc=0x100020024 (141) base=-33
0097 -                EQ       0096 K067
0098 -            ptr FREF     0090 #1  
0099 rcx          unk SLOAD    #65496 #0  
  SNAP #21 [-38:0089] pc=0x1000853ac (144) base=-40
0100 -                GT       0099 0089
0101 rdx          unk SLOAD    #65497 #0  
0102 -            ptr FREF     0101 #2  
0103 rdx          unk FLOAD    0102
  SNAP #22 [-41:K069 -40:|0099 0103] pc=0x100085380 (148) base=-40
0104 -                SAVE     #2   #0  
   free regs = {rax,rcx,rbx,rsi,rdi,r8,r10,r14,r15}
   [80.3:1.0]     - [3] <- rcx
   [b.0:b.0]    r11 <- r11
   [d.0:2.0]    r13 <- rdx
   [2.0:80.1]    rdx <-  - [1]
   [9.0:3.0]    r9 <- rbx

252d2761  0104 <<base += ffffffd8 words>>
252d275a  0104 alloc     K069 rax
252d2753  0104 alloc     0099 rcx
252d274c  0104 alloc     0103 rdx
252d2745  0103 dest           rdx
252d2745  0103 alloc     0101 rdx
252d2741  0101 dest           rdx
252d273a  0100 <<SNAP 00000015>>
252d273a  0100 alloc     0089 rdx
252d273a  0100 snapreg   0089 rdx
252d2731  0099 dest           rcx
252d272a  0097 <<SNAP 00000014>>
252d272a  0097 alloc     0090 rcx
252d272a  0097 snapreg   0090 rcx
252d272a  0097 alloc     0096 rbx
252d272a  0097 alloc     K067 rsi
252d2721  0096 dest           rbx
252d271a  0095 alloc     0093 rbx
252d2716  0095 alloc     K001 rdi
252d2713  0094 <<SNAP 00000013>>
252d270d  0094 alloc     K037 r8
252d270a  0093 dest           rbx
252d2703  0092 <<SNAP 00000012>>
252d2703  0092 alloc     0091 rbx
252d2703  0092 alloc     K009 r9
252d26fa  0091 dest           rbx
252d26f3  0090 dest           rcx
252d26e9  0090 alloc     K027 rcx
252d26e4  0089 dest           rdx
252d26dd  0088 <<SNAP 00000011>>
252d26dd  0088 alloc     0087 rdx
252d26dd  0088 alloc     K065 rbx
252d26d4  0087 dest           rdx
252d26cd  0086 remat     K027 rcx
252d26c3  0086 alloc     0001 rcx
252d26c3  0086 alloc     K063 rdx
252d26bc  0085 <<SNAP 00000010>>
252d26b3  0084 alloc     0021 r10
252d26ac  0083 <<SNAP 0000000f>>
252d26ac  0083 alloc     0002 r11
252d26ac  0083 snapreg   0002 r11
252d26ac  0083 alloc     0058 r13
252d26ac  0083 snapreg   0058 r13
252d26ac  0083 alloc     0078 r14
252d26ac  0083 snapreg   0078 r14
252d26ac  0083 alloc     0019 r15
252d26ac  0083 snapreg   0019 r15
252d26a6  0083 remat     K069 rax
252d269c  0083 alloc     K061 rax
252d2699  0080 remat     K067 rsi
252d268f  0080 alloc     0010 rsi
252d268f  0080 remat     K065 rbx
252d2685  0080 alloc     0079 rbx
252d267e  0079 dest           rbx
252d2674  0079 alloc     0076 rbx
252d266f  0079 remat     K063 rdx
252d2665  0079 alloc     0074 rdx
252d2660  0079 remat     K061 rax
252d2656  0079 alloc     0069 rax
252d2651  0079 remat     K037 r8
252d2647  0079 alloc     0072 r8
252d2642  0079 remat     K009 r9
252d2638  0079 alloc     K023 r9
252d2633  0078 dest           r14
252d2633  0078 alloc     0065 r14
252d262f  0076 dest           rbx
252d262b  0074 dest           rdx
252d2627  0072 dest           r8
252d2623  0070 <<SNAP 0000000e>>
252d2623  0070 alloc     0063 rdx
252d2623  0070 snapreg   0063 rdx
252d261d  0070 alloc     K035 rbx
252d261a  0069 dest           rax
252d2616  0067 <<SNAP 0000000d>>
252d260d  0066 <<SNAP 0000000c>>
252d2607  0066 alloc     K053 rax
252d2604  0065 dest           r14
252d2600  0063 dest           rdx
252d25fc  0061 <<SNAP 0000000b>>
252d25f6  0061 alloc     K037 rdx
252d25f3  0058 dest           r13
252d25ee  0058 alloc     0042 r8
252d25e9  0058 alloc     K039 r13
252d25e4  0056 <<SNAP 0000000a>>
252d25e4  0056 alloc     0044 r14
252d25e4  0056 snapreg   0044 r14
252d25e4  0056 snapspill 0053 [sp+16]
252d25e4  0056 snapspill 0055 [sp+24]
252d25e4  0056 remat     K053 rax
252d25da  0056 alloc     0053 rax
252d25da  0056 remat     K039 r13
252d25d0  0056 alloc     0055 r13
252d25c7  0055 dest           r13
252d25c7  0055 save      0055 r13
252d25c2  0055 alloc     0050 r13
252d25be  0053 dest           rax
252d25be  0053 save      0053 rax
252d25b9  0053 alloc     0027 rax
252d25b5  0051 <<SNAP 00000009>>
252d25af  0051 remat     K037 rdx
252d25a5  0051 alloc     K027 rdx
252d25a1  0050 dest           r13
252d259d  0048 <<SNAP 00000008>>
252d2594  0046 alloc     0003 r13
252d258c  0045 <<SNAP 00000007>>
252d2586  0045 remat     K035 rbx
252d257c  0045 alloc     K037 rbx
252d2578  0044 dest           r14
252d256e  0044 alloc     0029 r14
252d2564  0044 remat     K037 rbx
252d255a  0044 alloc     0025 rbx
252d254b  0043 <<SNAP 00000006>>
252d254b  0043 snapspill 0035 [sp+16]
252d254b  0043 snapspill 0037 [sp+24]
252d2545  0043 remat     K027 rdx
252d253b  0043 alloc     K035 rdx
252d2538  0042 dest           r8
252d2530  0042 alloc     0037 r8
252d2528  0042 remat     K035 rdx
252d251e  0042 alloc     K027 rdx
252d2516  0037 dest           r8
252d2516  0037 save      0037 r8
252d2511  0037 alloc     0023 r8
252d250d  0035 remat     K027 rdx
252d2503  0035 dest           rdx
252d2503  0035 save      0035 rdx
252d24fe  0035 alloc     0004 rdx
252d24fa  0033 <<SNAP 00000005>>
252d24fa  0033 snapspill 0030 [sp+16]
252d24f4  0033 remat     K023 r9
252d24ea  0033 alloc     K027 r9
252d24e7  0031 remat     K027 r9
252d24dd  0031 alloc     0005 r9
252d24dd  0031 remat     K001 rdi
252d24d3  0031 alloc     0030 rdi
252d24cf  0031 restore   0001 rcx
252d24ca  0031 alloc     K001 rcx
252d24c7  0030 dest           rdi
252d24c7  0030 save      0030 rdi
252d2492  0030 alloc     K023 rdi
252d248a  0029 dest           r14
252d248a  0029 alloc     0016 r14
252d2486  0027 dest           rax
252d2482  0025 dest           rbx
252d247e  0023 dest           r8
252d247a  0021 dest           r10
252d2462  0021 alloc     K025 rax
252d245a  0020 <<SNAP 00000004>>
252d245a  0020 alloc     0008 rbx
252d245a  0020 snapreg   0008 rbx
252d2448  0019 dest           r15
252d2444  0017 <<SNAP 00000003>>
252d243b  0016 dest           r14
252d243b  0016 alloc     0012 r8
252d2437  0014 <<SNAP 00000002>>
252d242e  0013 <<SNAP 00000001>>
252d2428  0013 alloc     K021 r10
252d2425  0012 dest           r8
252d2421  0010 dest           rsi
252d241d  0008 dest           rbx
252d2419  0006 <<SNAP 00000000>>
252d2413  0006 alloc     K015 rbx
252d2410  0005 remat     K025 rax
252d2406  0005 remat     K001 rcx
252d23fc  0005 remat     K015 rbx
252d23f2  0005 remat     K023 rdi
252d23e8  0005 remat     K021 r10
