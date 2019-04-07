module Evensen where

import Data.Word
import Data.Bits

import GHC.Base

rrmxmx :: Word64 -> Word64
rrmxmx v =
  let v1 = v `xor` (rotateR v 49 `xor` rotateR v 24)
      v2 = v1 * 0x9FB21C651E98DF25
      v3 = v2 `xor` (shiftR v2 28)
      v4 = v3 * 0x9FB21C651E98DF25
  in v4 `xor` (shiftR v4 28)

rrxmrrxmsx_0 :: Word64 -> Word64
rrxmrrxmsx_0 v =
  let v1 = v `xor` (rotateR v 25 `xor` rotateR v 50)
      v2 = v1 * 0xA24BAED4963EE407
      v3 = v2 `xor` (rotateR v 24 `xor` rotateR v 49)
      v4 = v3 * 0x9FB21C651E98DF25
  in v4 `xor` (shiftR v4 28)


tests = and
  [rrmxmx 0x0000000000000000 == 0x0000000000000000
  ,rrmxmx 0x0000000000000000 == 0x0000000000000000
  ,rrmxmx 0x0000000000000001 == 0x23085d6f7a569905
  ,rrmxmx 0x56ed9162154faac0 == 0x0000000000000001
  ,rrmxmx 0x0000000000000003 == 0xcaea878c77a59454
  ,rrmxmx 0x0ec1bfbe6983c5a0 == 0x0000000000000003
  ,rrmxmx 0x0000000000000007 == 0xa77bd5a63a7785c5
  ,rrmxmx 0x1718113ac9a1f119 == 0x0000000000000007
  ,rrmxmx 0x0101010101010101 == 0x36cb9e821eca6c5b
  ,rrmxmx 0xfa63351a390851cd == 0x0101010101010101
  ,rrmxmx 0x0123456789abcdef == 0xc337a528d7e42497
  ,rrmxmx 0x7529d4da142b1f1c == 0x0123456789abcdef
  ,rrmxmx 0x084c2a6e195d3b7f == 0x507d53f1ba22542c
  ,rrmxmx 0xec3694cd1c80b9cd == 0x084c2a6e195d3b7f
  ,rrmxmx 0x1000000000000001 == 0xedd3f3f24766de89
  ,rrmxmx 0xdb302dae3ad882e0 == 0x1000000000000001
  ,rrmxmx 0x1111111111111111 == 0x7547f019c63c1df3
  ,rrmxmx 0xea6d9bbf167027c9 == 0x1111111111111111
  ,rrmxmx 0x1fffffffffffffff == 0x05e3c8367d6677d6
  ,rrmxmx 0x7fbbf24327033cf0 == 0x1fffffffffffffff
  ,rrmxmx 0x3fffffffffffffff == 0x47e7c1e973d349ff
  ,rrmxmx 0x240ba915bbb5e089 == 0x3fffffffffffffff
  ,rrmxmx 0x6666666666666666 == 0xd9c6e8c9ecd1e30a
  ,rrmxmx 0xf4b9c6565f8d9529 == 0x6666666666666666
  ,rrmxmx 0x7777777777777777 == 0x29823cb92ada0068
  ,rrmxmx 0xdca549733043f019 == 0x7777777777777777
  ,rrmxmx 0x7f7f7f7f7f7f7f7f == 0xc58024da69c2eb57
  ,rrmxmx 0xf1d5238b66aaaf5e == 0x7f7f7f7f7f7f7f7f
  ,rrmxmx 0x7ffffffffffffff7 == 0x30c8918fcb6b2b3c
  ,rrmxmx 0x3a836e49ca560dd8 == 0x7ffffffffffffff7
  ,rrmxmx 0x7fffffffffffffff == 0x91b750beb6849d8f
  ,rrmxmx 0x90354478a1b6e49d == 0x7fffffffffffffff
  ,rrmxmx 0x8000000000000000 == 0x5e2d59ded82568fc
  ,rrmxmx 0xa0f3362cbce5bedb == 0x8000000000000000
  ,rrmxmx 0x8000000000000008 == 0xae03d8a5f03d42bb
  ,rrmxmx 0xed1a6dc89b6e22d2 == 0x8000000000000008
  ,rrmxmx 0x8080808080808080 == 0x269ed61ad0d4a3ad
  ,rrmxmx 0xcf8b0a0dccbf9da9 == 0x8080808080808080
  ,rrmxmx 0x8888888888888888 == 0x2f6af135bf8e9d79
  ,rrmxmx 0x2c50b3a1d5c7a854 == 0x8888888888888888
  ,rrmxmx 0x9999999999999999 == 0x50a99564c864eb28
  ,rrmxmx 0x6ae2b8e14b6d3c7c == 0x9999999999999999
  ,rrmxmx 0xc000000000000000 == 0xf5f0f95fcd968a80
  ,rrmxmx 0x6ae70fea73bd7a6d == 0xc000000000000000
  ,rrmxmx 0xe000000000000000 == 0x160c347d11027361
  ,rrmxmx 0x9a3d176b24d68305 == 0xe000000000000000
  ,rrmxmx 0xeeeeeeeeeeeeeeee == 0x9f9714241fb64d9e
  ,rrmxmx 0x0a40b8632cad4bfa == 0xeeeeeeeeeeeeeeee
  ,rrmxmx 0xeffffffffffffffe == 0x742025f2e92e6aec
  ,rrmxmx 0xf7eaaefaaa16ddb8 == 0xeffffffffffffffe
  ,rrmxmx 0xf7b3d591e6a2c480 == 0x60f421f08a38d500
  ,rrmxmx 0xf520f63f955ac204 == 0xf7b3d591e6a2c480
  ,rrmxmx 0xfedcba9876543210 == 0x8fec24c21c6d66de
  ,rrmxmx 0xf18dbb478c6d3943 == 0xfedcba9876543210
  ,rrmxmx 0xfefefefefefefefe == 0x125c8836f02c998f
  ,rrmxmx 0xe4b673f0521ad37d == 0xfefefefefefefefe
  ,rrmxmx 0xfffffffffffffff8 == 0x6018ed12f08b6eec
  ,rrmxmx 0x1b32e354639f82f1 == 0xfffffffffffffff8
  ,rrmxmx 0xfffffffffffffffc == 0x420b85f7b23fa512
  ,rrmxmx 0xe317247fad148210 == 0xfffffffffffffffc
  ,rrmxmx 0xfffffffffffffffe == 0xc320bdd84877d048
  ,rrmxmx 0x31c9d93c42d48cea == 0xfffffffffffffffe
  ,rrmxmx 0xffffffffffffffff == 0x8bc57fddf83265bd
  ,rrmxmx 0xb694bf1eaa6682c4 == 0xffffffffffffffff
  ]

