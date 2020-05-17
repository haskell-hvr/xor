{-# LANGUAGE CPP         #-}
{-# LANGUAGE Haskell2010 #-}

-- |
-- Copyright: © 2019-2020  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Endianness
    ( Word8, Word16, Word32, Word64
    , ByteOrder(LittleEndian,BigEndian), targetByteOrder

    , byteSwap16
    , byteSwap32
    , byteSwap64

    , pokeWord16be
    , pokeWord32be
    , pokeWord64be
    , peekWord16be
    , peekWord32be
    , peekWord64be

    ) where

import           Data.Word        (Word16, Word32, Word64, Word8)
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.ByteOrder    (ByteOrder (..), targetByteOrder)

#if MIN_VERSION_base(4,7,0)
import           Data.Word        (byteSwap16, byteSwap32, byteSwap64)
#else
import           Data.Bits

-- supply missing byteSwap operations

byteSwap16 :: Word16 -> Word16
byteSwap16 = (`rotateL` 8)

byteSwap32 :: Word32 -> Word32
byteSwap32 x
  = (x                  `shiftR` 24)  .|.
    ((x .&. 0x00ff0000) `shiftR`  8)  .|.
    ((x .&. 0x0000ff00) `shiftL`  8)  .|.
    (x                  `shiftL` 24)

byteSwap64 :: Word64 -> Word64
byteSwap64 x = xh .|. (xl `shiftL` 32)
  where
    xl = fromIntegral (byteSwap32 (fromIntegral x))
    xh = fromIntegral (byteSwap32 (fromIntegral (x `shiftR` 32)))
#endif

pokeWord16be :: Ptr Word16 -> Word16 -> IO ()
pokeWord16be = case targetByteOrder of
  BigEndian    -> poke
  LittleEndian -> \p w -> poke p (byteSwap16 w)

pokeWord32be :: Ptr Word32 -> Word32 -> IO ()
pokeWord32be = case targetByteOrder of
  BigEndian    -> poke
  LittleEndian -> \p w -> poke p (byteSwap32 w)

pokeWord64be :: Ptr Word64 -> Word64 -> IO ()
pokeWord64be = case targetByteOrder of
  BigEndian    -> poke
  LittleEndian -> \p w -> poke p (byteSwap64 w)


peekWord16be :: Ptr Word16 -> IO Word16
peekWord16be = case targetByteOrder of
  BigEndian    -> peek
  LittleEndian -> \p -> fmap byteSwap16 (peek p)

peekWord32be :: Ptr Word32 -> IO Word32
peekWord32be = case targetByteOrder of
  BigEndian    -> peek
  LittleEndian -> \p -> fmap byteSwap32 (peek p)

peekWord64be :: Ptr Word64 -> IO Word64
peekWord64be = case targetByteOrder of
  BigEndian    -> peek
  LittleEndian -> \p -> fmap byteSwap64 (peek p)
